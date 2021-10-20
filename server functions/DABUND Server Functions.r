
	########################################################################	
	## disable Differential-Abundance tab on page load
	########################################################################
    js$disableTab("Differential-Abundance")

	########################################################################		
	## Open Differential Abundance tab when input$goIMPORT is observed
	########################################################################	
    observeEvent(input$goIMPORT, {
		# enable Differential-Abundance when clicking the button
		js$enableTab("Differential-Abundance")
		# switch to Differential-Abundance
		updateTabsetPanel(session, "adivpage", "Differential-Abundance")
    })

	######################################################################
	## Toggle regular/larger text 
	########################################################################	
	observe({
		shinyjs::toggleClass("text_dabund", "big", input$dABUNDbig)
    })

	########################################################################	
	## Render Taxa Selections Experimental Groups options
	########################################################################				
	output$DABUNDtaxaselectRENDER <- renderUI({
		req(phyloseqFINAL())
		phyloseqFINAL <- phyloseqFINAL()
		psFtaxatab <- tax_table(phyloseqFINAL)
		TAXAnms <- colnames(psFtaxatab)
		TAXAnms <- TAXAnms[!(TAXAnms %in% "Kingdom")]
		if(length(TAXAnms) != 6){
			list(
				HTML("
					  <h4><strong>Select Taxonomic Level(s):</strong></h4>
					 "
				),	 
				selectizeInput("dabundTAXAselect", label="", selected="Phylum",
					choices=TAXAnms, options=list(placeholder = 'Click box to select parameters'), 
					multiple=TRUE),
				HTML("
					  <p>Selecting multiple taxonomic levels will require longer computation time.</p>
					  <p>.</p>
					 "
				)
			)
		} else {
			list(
				HTML("
					  <h4><strong>Select Taxonomic Level(s):</strong></h4>
					 "
				),	 
				selectizeInput("dabundTAXAselect", label="", selected="Phylum",
					choices=c(taxaL, "OTU"), options=list(placeholder = 'Click box to select parameters'), 
					multiple=TRUE),
				HTML("
					  <p>Selecting multiple taxonomic levels will require longer computation time.</p>
					  <p>.</p>
					 "
				)
			)
		}
	})
			
	########################################################################	
	## Isolate TAXA selections
	########################################################################
	DABUNDtaxa <- reactive({
		if(input$goDABUND){
			isolate({
				input$dabundTAXAselect
			})
		} else {
			NULL
		}
	})

	########################################################################	
	## Agglomerate reads according to TAXA selections
	########################################################################
	daTAXALEVEL <- reactive({
		if(input$goDABUND){
			isolate({		
				DABUNDtaxa <- DABUNDtaxa()				
				## OTU does not require to be agglomerated 
				## Needs to be handled by itself if selected
				if("OTU" %in% DABUNDtaxa){
					## If only OTU is selected then move object into a list, name list element, and return
					if(setequal(DABUNDtaxa == "OTU", TRUE)) {
						
						phylo_TAX_LIST <- list("OTU" = phyloseqFINAL())
						names(phylo_TAX_LIST) <- DABUNDtaxa()
						phylo_TAX_LIST
						
					} else {
						## Run phyloseq::tax_glom() function on selected TAXA
						## using pblapply() to render status boxes
						DABUNDtaxa <- DABUNDtaxa[!(DABUNDtaxa %in% "OTU")]
						withProgress(message = "Agglomerating Taxa", value=0, { 
							percentage <- 0 
							phylo_TAX_LIST <- pblapply(DABUNDtaxa, function(x) {
								Sys.sleep(0.05); 
								percentage <<- percentage + 1/length(DABUNDtaxa())*100
								incProgress(1/length(DABUNDtaxa()), detail = paste0("Progress: ", round(percentage,2),"%"))
								tax_glom(phyloseqFINAL(), x)
							})
						})
						phylo_TAX_LIST[[length(phylo_TAX_LIST) + 1]] <- phyloseqFINAL()
						names(phylo_TAX_LIST) <- DABUNDtaxa()
						phylo_TAX_LIST
					}
				} else {
				## If OTU is NOT selected then run phyloseq::tax_glom() function on selected TAXA
				## using pblapply() to render status boxes
					DABUNDtaxa <- DABUNDtaxa
					withProgress(message = "Agglomerating Taxa", value=0, { 
						percentage <- 0 
						phylo_TAX_LIST <- pblapply(DABUNDtaxa, function(x) {
							Sys.sleep(0.05); 
							percentage <<- percentage + 1/length(DABUNDtaxa())*100
							incProgress(1/length(DABUNDtaxa()), detail = paste0("Progress: ", round(percentage,2),"%"))
							tax_glom(phyloseqFINAL(), x)
						})						
					})
					## Set list element names and return
					names(phylo_TAX_LIST) <- DABUNDtaxa()
					phylo_TAX_LIST
				}
				
			})
		} else {
			NULL
		}
	})

	########################################################################	
	## Transform reads into relative abundance	
	########################################################################	
	daTAXALEVEL_RA <- reactive({
		if(input$goDABUND){
			isolate({	
			
				## Use selected TAXA and isolate phyloseq object.
				## Then use phyloseq::transform_sample_counts() to transform reads into relative abundance
				req(daTAXALEVEL())
				withProgress(message = "Calculating Relative Abundance", value=0, { 
					percentage <- 0 
					Sys.sleep(0.05); 
					phylo_TAX_RA_LIST <- pblapply(DABUNDtaxa(), function(taxa) {
						percentage <<- percentage + 1/length(DABUNDtaxa())*100
						incProgress(1/length(DABUNDtaxa()), detail = paste0("Progress: ", round(percentage,2),"%"))
							pob <- daTAXALEVEL()[[taxa]]
							transform_sample_counts(pob, function(x) 100 * (x / sum(x)))
					})
				})
				names(phylo_TAX_RA_LIST) <- DABUNDtaxa()
				phylo_TAX_RA_LIST
			})
		} else {
			NULL
		}				
	})	

	########################################################################
	## Render header text for NBR group and pairwise comparisons.
	## Loads after goDABUND observed so control of NBM and boxplots will be 
	## updated based on selection.
	########################################################################	
	output$DABUNDnbrtestexplanRENDER <- renderUI({
		if(input$dabundNBMtest == "Wald") {
			HTML("
				<p>Runs Negative Binomial Regression - pairwise group comparisons.</p>
				"
			)
		} else {
			HTML("
				<p>Runs Negative Binomial Regression - overall group effect.</p>
				"
			)			
		} 	
	})

	########################################################################
	## Render header text for NBR group and pairwise comparisons.
	## Loads after goDABUND observed so control of NBM and boxplots will be 
	## updated based on selection.
	########################################################################	
	output$DABUNDgrouppairtextRENDER <- renderUI({
				if(is.null(daTAXALEVEL())) {
					NULL
				} else {
					if(input$dabundNBMtest == "Wald") {
						list(
							HTML("
								<h3><em><strong>Select the comparison group and pairwise comparison. The \"Finalize Differential Abundance\" does NOT 
								have to be pressed to update group and pairwise selections.</em></strong></h3></strong></em></h3>
								"
							)
						)
					} else {
						list(
							HTML("
								<h3><em><strong>Select the comparison group. The \"Finalize Differential Abundance\" does NOT 
								have to be pressed to update group selections.</em></strong></h3></strong></em></h3>
								"
							)
						)
					}
				} 	
	})

	########################################################################	
	## Render NBR group and pairwise comparison selections.
	## Loads after goDABUND observed so control of NBM and boxplots will be 
	## updated based on selection.
	########################################################################

	output$DABUNDgroupselectRENDER <- renderUI({
		if(input$goDABUND){
			isolate({
				list(
					hr(),
					fluidPage(
						column(12,
							uiOutput("DABUNDgrouppairtextRENDER")
						)
					),
					fluidPage(
						column(3,
					
							HTML("
								<h4><strong>Select Group(s):</strong></h4>
								"
							),			

							selectInput(inputId="dabundGROUPselect", label="", choices=CompGroups()$Groups, 
								selected=CompGroups()$Groups[1])
						),
						column(3,
							uiOutput("DABUNDpaircompRENDER")
						)
					)
				)
			})
		} else {
			NULL
		}					
	})

	########################################################################	
	## Identify Experimental Group combinations for pairwise selectInput function
	########################################################################
	DABUNDpaircomp <- reactive({
		req(input$dabundGROUPselect)
		req(daTAXALEVEL())
		if(is.null(input$dabundGROUPselect)) {
			NULL
		} else {
			## Acquire sample data from phyloseq object, 
			## select column based on input$dabundGROUPselect, 
			## Make all combinations, 
			## and paste pairwise combinations into a vector to be used in pairwise selections
			sdata <- sample_data(phyloseqFINAL())
			groupD <- sdata[, colnames(sdata) %in% input$dabundGROUPselect]
			apply(combn(levels(groupD@.Data[[1]]), 2), 2, paste, collapse=" vs ")
		}
	})	

	########################################################################
	## Render select input for Pairwise selection.
	## Loads after goDABUND observed so control of NBM and boxplots will be 
	## updated based on selection.
	########################################################################	
	output$DABUNDpaircompRENDER <- renderUI({
				if(is.null(daTAXALEVEL())) {
					NULL
				} else {
					if(input$dabundNBMtest == "Wald") {
						list(
								HTML("
									<h4><strong>Select Pairwise Comparison:</strong></h4>
									"
								),	
								selectInput(inputId="dabundPAIRCOMPselect", label="", choices=DABUNDpaircomp(), 
									selected=DABUNDpaircomp()[1])
						
						)
					} else {
						NULL
					}
				} 	
	})

	########################################################################	
	## Identify Experimental Group combinations for pairwise selectInput function	
	########################################################################	
	DABUNDpaircompNBM <- reactive({
		if(is.null(input$dabundGROUPselect)) {
			NULL
		} else {
			## Acquire sample data from phyloseq object, 
			## select column based on input$dabundGROUPselect, 
			## Make all combinations
			sdata <- sample_data(phyloseqFINAL())
			groupD <- sdata[, colnames(sdata) %in% input$dabundGROUPselect]
			combn(levels(groupD@.Data[[1]]), 2)
		}
	})	

	########################################################################
	## Assess Negative Binomial Models
	## Pairwise Comparisons
	## Adapted from http://joey711.github.io/phyloseq-extensions/DESeq2.html
	########################################################################	
	dabundNBM <- reactive({
		req(daTAXALEVEL())
		daTAXALEVEL <- daTAXALEVEL()
		DABUNDtaxa <- DABUNDtaxa()
		dabundGROUPselect <- input$dabundGROUPselect
		## Make formula for phyloseq::phyloseq_to_deseq2()
		validate(
			need(dabundGROUPselect != "", "")
		)
		deseqform <- formula(paste0("~ ", dabundGROUPselect))
		
		## using pblapply() to render status boxes
		withProgress(message = "Running NBM", value=0, { 
			percentage <- 0 
			deseq2ob <- pblapply(DABUNDtaxa, function(x) {
				Sys.sleep(0.05); 
				percentage <<- percentage + 1/length(DABUNDtaxa)*100
				incProgress(1/length(DABUNDtaxa), detail = paste0("Progress: ", round(percentage,2),"%"))
				
				## Isolate phyloseq object based on TAXA level 
				pob <- daTAXALEVEL[[x]]
				## Convert to DESeq2's DESeqDataSet class
				deseqob <- phyloseq_to_deseq2(pob, deseqform)
				## Calculate Geometric Means
				deseqob_GM = apply(counts(deseqob), 1, gm_mean)
				## Estimate size factors
				deseqobESF = estimateSizeFactors(deseqob, geoMeans = deseqob_GM)
				
				fullMODEL <- paste0("~ ", dabundGROUPselect)
				reducedMODEL <- "~ 1"
				
				if(input$dabundNBMtest == "Wald"){
					## Calculate Negative Binomial GLM fitting with LRT
					# deseqob_NB = DESeq(deseqobESF, fitType="local", test="LRT",reduced=~1)
					## Calculate Negative Binomial GLM fitting with Wald Test
					deseqob_NB <- DESeq(deseqobESF, fitType="local", test=input$dabundNBMtest)
					## Extract results from DESeq analysis across all pairwise comparisons
					NBM_paircomps <- lapply(1:ncol(DABUNDpaircompNBM()), function(pair) {
						## Isolate pairs and then set vector for comparison extraction
						Cpair1 <- DABUNDpaircompNBM()[1,pair]
						Cpair2 <- DABUNDpaircompNBM()[2,pair]				
						NBMcontrasts <- c(dabundGROUPselect, Cpair1, Cpair2)
						
						## Extract comparison from DESeq object
						deseqob_pairres <- results(deseqob_NB, contrast=NBMcontrasts, test="Wald")
						## Bind DESeq results and Taxonomic data into data frame
						deseqob_pairresDF <- cbind(as(deseqob_pairres, "data.frame"), as(tax_table(pob)[rownames(deseqob_pairres), ], "matrix"))
						## Import OTUs into 'Species' column, change column name to 'OTU', and return
						deseqob_pairresDF$Species <- rownames(deseqob_pairresDF)
						colnames(deseqob_pairresDF)[which(colnames(deseqob_pairresDF) %in% "Species")] <- "OTU"
						deseqob_pairresDF
					})
					## Set list element names and return
					names(NBM_paircomps) <- DABUNDpaircomp()
					NBM_paircomps
				} else {
					# deseqob_NB <- DESeq(deseqobESF, test=input$dabundNBMtest, fitType="local", full=as.formula(fullMODEL), reduced=as.formula(reducedMODEL))
					deseqob_NB = DESeq(deseqobESF, fitType="local", test="LRT",full=as.formula(fullMODEL), reduced=as.formula(reducedMODEL))
					## Extract comparison from DESeq object
					deseqob_pairres <- results(deseqob_NB)
					## Bind DESeq results and Taxonomic data into data frame
					deseqob_pairresDF <- cbind(as(deseqob_pairres, "data.frame"), as(tax_table(pob)[rownames(deseqob_pairres), ], "matrix"))
					## Import OTUs into 'Species' column, change column name to 'OTU', and return
					deseqob_pairresDF$Species <- rownames(deseqob_pairresDF)
					colnames(deseqob_pairresDF)[which(colnames(deseqob_pairresDF) %in% "Species")] <- "OTU"
					NBM_paircomps <- deseqob_pairresDF
					NBM_paircomps
					
					NBM_paircomps
				}
			})
		})
		## Set list element names and return
		names(deseq2ob) <- DABUNDtaxa
		deseq2ob
	})	

	# output$dabundTEXT <- renderPrint({


	# })	
		
	########################################################################
	## Create DataTable object with Phylum NBM data
	########################################################################	
	output$DABUNDphylumNBMtableRENDER <- DT::renderDataTable({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			## If phylum TAXA is selected run:
			if("Phylum" %in% names(dabundNBM())) {
				## Extract TAXA and remove relevant columns
				if(input$dabundNBMtest == "Wald"){
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Phylum")]][[input$dabundPAIRCOMPselect]]
					t1DF <- t1[,c("OTU","Phylum", "baseMean", "log2FoldChange", "pvalue", "padj")]
				} else {
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Phylum")]]
					t1DF <- t1[,c("OTU","Phylum", "baseMean", "pvalue", "padj")]					
				}
				## Remove excess characters from OTU labels and reset into DF
				OTU <- as.character(t1DF$OTU)
				OTU <- gsub("New.CleanUp.ReferenceOTU", "", OTU)
				OTU <- gsub("New.ReferenceOTU", "", OTU)
				t1DF$OTU <- OTU
				## Round data to the 5 decimal and order data by adjusted pvalue
				t1DF[,!(colnames(t1DF) %in% c("OTU","Phylum"))] <- 
					round(t1DF[,!(colnames(t1DF) %in% c("OTU","Phylum"))], 5)
				t1DF <- t1DF[order(t1DF$padj),]
				## Need to set continuous data as numeric or DataTable will not render properly
				t1DF[,!(colnames(t1DF) %in% c("OTU","Phylum"))] <- 
					apply(t1DF[,!(colnames(t1DF) %in% c("OTU","Phylum"))], 2, as.numeric)	
				## Create DataTable
				if(input$dabundNBMtest == "Wald"){				
					datatable(t1DF, extensions='Buttons', rownames = FALSE,
							container = htmltools::withTags(table(
								class = 'display',
									thead(
									tr(
										th(colspan = 1, 'ID'),
										th(colspan = 1, 'Phylum'),
										th(colspan = 1, 'Base Mean'),
										th(colspan = 1, 'Log2 Fold Change'),
										th(colspan = 1, 'P'),
										th(colspan = 1, 'P-adjusted')
									)
								)
							)
						),
							options = list(
								dom = 'lf<"floatright"B>rtip',
								buttons = c('excel', 'pdf', 'csv'),
								searching = TRUE,
								pageLength = 5,
								lengthMenu = c(5, nrow(t1DF))
							)
						) 
				} else {		
					datatable(t1DF, extensions='Buttons', rownames = FALSE,
							container = htmltools::withTags(table(
								class = 'display',
									thead(
									tr(
										th(colspan = 1, 'ID'),
										th(colspan = 1, 'Phylum'),
										th(colspan = 1, 'Base Mean'),
										th(colspan = 1, 'P'),
										th(colspan = 1, 'P-adjusted')
									)
								)
							)
						),
							options = list(
								dom = 'lf<"floatright"B>rtip',
								buttons = c('excel', 'pdf', 'csv'),
								searching = TRUE,
								pageLength = 5,
								lengthMenu = c(5, nrow(t1DF))
							)
						)
					}
			} else {
				NULL
			}
		}			
	})	

	########################################################################
	## Create DataTable object with Class NBM data
	########################################################################	
	output$DABUNDclassNBMtableRENDER <- DT::renderDataTable({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			if("Class" %in% names(dabundNBM())) {
			## If class TAXA is selected run:
				## Extract TAXA and remove relevant columns
				# t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Class")]][[input$dabundPAIRCOMPselect]]
				# t1DF <- t1[,c("OTU","Phylum", "Class", "baseMean", "log2FoldChange", "pvalue", "padj")]
				if(input$dabundNBMtest == "Wald"){
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Class")]][[input$dabundPAIRCOMPselect]]
					t1DF <- t1[,c("OTU","Phylum","Class", "baseMean", "log2FoldChange", "pvalue", "padj")]
				} else {
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Class")]]
					t1DF <- t1[,c("OTU","Phylum","Class", "baseMean", "pvalue", "padj")]					
				}				
				## Remove excess characters from OTU labels and reset into DF
				OTU <- as.character(t1DF$OTU)
				OTU <- gsub("New.CleanUp.ReferenceOTU", "", OTU)
				OTU <- gsub("New.ReferenceOTU", "", OTU)
				t1DF$OTU <- OTU
				## Round data to the 5 decimal and order data by adjusted pvalue
				t1DF[,!(colnames(t1DF) %in% c("OTU","Phylum","Class"))] <- 
					round(t1DF[,!(colnames(t1DF) %in% c("OTU","Phylum","Class"))], 5)
				t1DF <- t1DF[order(t1DF$padj),]
				
				## Need to set continuous data as numeric or DataTable will not render properly
				t1DF[,!(colnames(t1DF) %in% c("OTU","Phylum","Class"))] <- 
					apply(t1DF[,!(colnames(t1DF) %in% c("OTU","Phylum","Class"))], 2, as.numeric)
				## Create DataTable	
				if(input$dabundNBMtest == "Wald"){		
					datatable(t1DF, extensions='Buttons', rownames = FALSE,
							container = htmltools::withTags(table(
								class = 'display',
									thead(
									tr(
										th(colspan = 1, 'ID'),
										th(colspan = 1, 'Phylum'),
										th(colspan = 1, 'Class'),
										th(colspan = 1, 'Base Mean'),
										th(colspan = 1, 'Log2 Fold Change'),
										th(colspan = 1, 'P'),
										th(colspan = 1, 'P-adjusted')
									)
								)
							)
						),
							options = list(
								dom = 'lf<"floatright"B>rtip',
								buttons = c('excel', 'pdf', 'csv'),
								searching = TRUE,
								pageLength = 5,
								lengthMenu = c(5, nrow(t1DF))
							)
						)
				} else {		
					datatable(t1DF, extensions='Buttons', rownames = FALSE,
							container = htmltools::withTags(table(
								class = 'display',
									thead(
									tr(
										th(colspan = 1, 'ID'),
										th(colspan = 1, 'Phylum'),
										th(colspan = 1, 'Class'),
										th(colspan = 1, 'Base Mean'),
										th(colspan = 1, 'P'),
										th(colspan = 1, 'P-adjusted')
									)
								)
							)
						),
							options = list(
								dom = 'lf<"floatright"B>rtip',
								buttons = c('excel', 'pdf', 'csv'),
								searching = TRUE,
								pageLength = 5,
								lengthMenu = c(5, nrow(t1DF))
							)
						)
				} 
			} else {
				NULL
			}
		}
	})	

	########################################################################
	## Create DataTable object with Order NBM data
	########################################################################	
	output$DABUNDorderNBMtableRENDER <- DT::renderDataTable({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			## If order TAXA is selected run:
			if("Order" %in% names(dabundNBM())) {
				## Extract TAXA and remove relevant columns
				# t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Order")]][[input$dabundPAIRCOMPselect]]
				# t1DF <- t1[,c("OTU","Class", "Order","baseMean", "log2FoldChange", "pvalue", "padj")]
				if(input$dabundNBMtest == "Wald"){
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Order")]][[input$dabundPAIRCOMPselect]]
					t1DF <- t1[,c("OTU","Class","Order", "baseMean", "log2FoldChange", "pvalue", "padj")]
				} else {
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Order")]]
					t1DF <- t1[,c("OTU","Class","Order", "baseMean", "pvalue", "padj")]					
				}				
				## Remove excess characters from OTU labels and reset into DF
				OTU <- as.character(t1DF$OTU)
				OTU <- gsub("New.CleanUp.ReferenceOTU", "", OTU)
				OTU <- gsub("New.ReferenceOTU", "", OTU)
				t1DF$OTU <- OTU
				## Round data to the 5 decimal and order data by adjusted pvalue
				t1DF[,!(colnames(t1DF) %in% c("OTU","Class", "Order"))] <- 
					round(t1DF[,!(colnames(t1DF) %in% c("OTU","Class", "Order"))], 5)
				t1DF <- t1DF[order(t1DF$padj),]
				
				## Need to set continuous data as numeric or DataTable will not render properly
				t1DF[,!(colnames(t1DF) %in% c("OTU","Class", "Order"))] <- 
					apply(t1DF[,!(colnames(t1DF) %in% c("OTU","Phylum","Class", "Order"))], 2, as.numeric)	
				## Create DataTable
				if(input$dabundNBMtest == "Wald"){		
					datatable(t1DF, extensions='Buttons', rownames = FALSE,
							container = htmltools::withTags(table(
								class = 'display',
									thead(
									tr(
										th(colspan = 1, 'ID'),
										th(colspan = 1, 'Class'),
										th(colspan = 1, 'Order'),
										th(colspan = 1, 'Base Mean'),
										th(colspan = 1, 'Log2 Fold Change'),
										th(colspan = 1, 'P'),
										th(colspan = 1, 'P-adjusted')
									)
								)
							)
						),
							options = list(
								dom = 'lf<"floatright"B>rtip',
								buttons = c('excel', 'pdf', 'csv'),
								searching = TRUE,
								pageLength = 5,
								lengthMenu = c(5, nrow(t1DF))
							)
					)
				} else {		
					datatable(t1DF, extensions='Buttons', rownames = FALSE,
							container = htmltools::withTags(table(
								class = 'display',
									thead(
									tr(
										th(colspan = 1, 'ID'),
										th(colspan = 1, 'Class'),
										th(colspan = 1, 'Order'),
										th(colspan = 1, 'Base Mean'),
										th(colspan = 1, 'P'),
										th(colspan = 1, 'P-adjusted')
									)
								)
							)
						),
							options = list(
								dom = 'lf<"floatright"B>rtip',
								buttons = c('excel', 'pdf', 'csv'),
								searching = TRUE,
								pageLength = 5,
								lengthMenu = c(5, nrow(t1DF))
							)
						)
				} 
			} else {
				NULL
			}
		}	
	})	

	########################################################################
	## Create DataTable object with Family NBM data
	########################################################################	
	output$DABUNDfamilyNBMtableRENDER <- DT::renderDataTable({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			## If family TAXA is selected run:
			if("Family" %in% names(dabundNBM())) {
				## Extract TAXA and remove relevant columns
				# t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Family")]][[input$dabundPAIRCOMPselect]]
				# t1DF <- t1[,c("OTU", "Order", "Family","baseMean", "log2FoldChange", "pvalue", "padj")]
				if(input$dabundNBMtest == "Wald"){
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Family")]][[input$dabundPAIRCOMPselect]]
					t1DF <- t1[,c("OTU","Order","Family", "baseMean", "log2FoldChange", "pvalue", "padj")]
				} else {
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Family")]]
					t1DF <- t1[,c("OTU","Order","Family", "baseMean", "pvalue", "padj")]					
				}				
				## Remove excess characters from OTU labels and reset into DF
				OTU <- as.character(t1DF$OTU)
				OTU <- gsub("New.CleanUp.ReferenceOTU", "", OTU)
				OTU <- gsub("New.ReferenceOTU", "", OTU)
				t1DF$OTU <- OTU
				## Round data to the 5 decimal and order data by adjusted pvalue
				t1DF[,!(colnames(t1DF) %in% c("OTU","Order", "Family"))] <- 
					round(t1DF[,!(colnames(t1DF) %in% c("OTU","Order", "Family"))], 5)
				t1DF <- t1DF[order(t1DF$padj),]
				
				## Need to set continuous data as numeric or DataTable will not render properly
				t1DF[,!(colnames(t1DF) %in% c("OTU","Order", "Family"))] <- 
					apply(t1DF[,!(colnames(t1DF) %in% c("OTU","Order", "Family"))], 2, as.numeric)	
				## Create DataTable
				if(input$dabundNBMtest == "Wald"){		
					datatable(t1DF, extensions='Buttons', rownames = FALSE,
							container = htmltools::withTags(table(
								Order = 'display',
									thead(
									tr(
										th(colspan = 1, 'ID'),
										th(colspan = 1, 'Order'),
										th(colspan = 1, 'Family'),
										th(colspan = 1, 'Base Mean'),
										th(colspan = 1, 'Log2 Fold Change'),
										th(colspan = 1, 'P'),
										th(colspan = 1, 'P-adjusted')
									)
								)
							)
						),
							options = list(
								dom = 'lf<"floatright"B>rtip',
								buttons = c('excel', 'pdf', 'csv'),
								searching = TRUE,
								pageLength = 5,
								lengthMenu = c(5, 10, 20, 50, nrow(t1DF))
							)
					)
				} else {		
					datatable(t1DF, extensions='Buttons', rownames = FALSE,
							container = htmltools::withTags(table(
								class = 'display',
									thead(
									tr(
										th(colspan = 1, 'ID'),
										th(colspan = 1, 'Order'),
										th(colspan = 1, 'Family'),
										th(colspan = 1, 'Base Mean'),
										th(colspan = 1, 'P'),
										th(colspan = 1, 'P-adjusted')
									)
								)
							)
						),
							options = list(
								dom = 'lf<"floatright"B>rtip',
								buttons = c('excel', 'pdf', 'csv'),
								searching = TRUE,
								pageLength = 5,
								lengthMenu = c(5, nrow(t1DF))
							)
						)
				}  
			} else {
				NULL
			}
		}	
	})			
	
	########################################################################
	## Create DataTable object with Genus NBM data
	########################################################################	
	output$DABUNDgenusNBMtableRENDER <- DT::renderDataTable({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			## If genus TAXA is selected run:
			if("Genus" %in% names(dabundNBM())) {
				## Extract TAXA and remove relevant columns
				# t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Genus")]][[input$dabundPAIRCOMPselect]]
				# t1DF <- t1[,c("OTU", "Family", "Genus","baseMean", "log2FoldChange", "pvalue", "padj")]
				if(input$dabundNBMtest == "Wald"){
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Genus")]][[input$dabundPAIRCOMPselect]]
					t1DF <- t1[,c("OTU","Family","Genus", "baseMean", "log2FoldChange", "pvalue", "padj")]
				} else {
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Genus")]]
					t1DF <- t1[,c("OTU","Family","Genus", "baseMean", "pvalue", "padj")]					
				}				
				## Remove excess characters from OTU labels and reset into DF
				OTU <- as.character(t1DF$OTU)
				OTU <- gsub("New.CleanUp.ReferenceOTU", "", OTU)
				OTU <- gsub("New.ReferenceOTU", "", OTU)
				t1DF$OTU <- OTU
				## Round data to the 5 decimal and order data by adjusted pvalue
				t1DF[,!(colnames(t1DF) %in% c("OTU","Family", "Genus"))] <- 
					round(t1DF[,!(colnames(t1DF) %in% c("OTU","Family", "Genus"))], 5)
				t1DF <- t1DF[order(t1DF$padj),]
				
				## Need to set continuous data as numeric or DataTable will not render properly
				t1DF[,!(colnames(t1DF) %in% c("OTU","Family", "Genus"))] <- 
					apply(t1DF[,!(colnames(t1DF) %in% c("OTU","Family", "Genus"))], 2, as.numeric)
				## Create DataTable	
				if(input$dabundNBMtest == "Wald"){		
					datatable(t1DF, extensions='Buttons', rownames = FALSE,
							container = htmltools::withTags(table(
								Order = 'display',
									thead(
									tr(
										th(colspan = 1, 'OTU'),
										th(colspan = 1, 'Family'),
										th(colspan = 1, 'Genus'),
										th(colspan = 1, 'Base Mean'),
										th(colspan = 1, 'Log2 Fold Change'),
										th(colspan = 1, 'P'),
										th(colspan = 1, 'P-adjusted')
									)
								)
							)
						),
							options = list(
								dom = 'lf<"floatright"B>rtip',
								buttons = c('excel', 'pdf', 'csv'),
								searching = TRUE,
								pageLength = 5,
								lengthMenu = c(5, 10, 20, 50, nrow(t1DF))
							)
					)
				} else {		
					datatable(t1DF, extensions='Buttons', rownames = FALSE,
							container = htmltools::withTags(table(
								class = 'display',
									thead(
									tr(
										th(colspan = 1, 'ID'),
										th(colspan = 1, 'Family'),
										th(colspan = 1, 'Genus'),
										th(colspan = 1, 'Base Mean'),
										th(colspan = 1, 'P'),
										th(colspan = 1, 'P-adjusted')
									)
								)
							)
						),
							options = list(
								dom = 'lf<"floatright"B>rtip',
								buttons = c('excel', 'pdf', 'csv'),
								searching = TRUE,
								pageLength = 5,
								lengthMenu = c(5, nrow(t1DF))
							)
						)
				}  
			} else {
				NULL
			}
		}
	})	
	
	########################################################################	
	## Create DataTable object with OTU NBM data
	########################################################################	
	output$DABUNDotuNBMtableRENDER <- DT::renderDataTable({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			## If otu TAXA is selected run:
			if("OTU" %in% names(dabundNBM())) {
				## Extract TAXA and remove relevant columns
				# t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "OTU")]][[input$dabundPAIRCOMPselect]]
				# t1DF <- t1[,c("OTU","Family", "Genus", "baseMean", "log2FoldChange", "pvalue", "padj")]
				if(input$dabundNBMtest == "Wald"){
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "OTU")]][[input$dabundPAIRCOMPselect]]
					t1DF <- t1[,c("OTU","Family","Genus", "baseMean", "log2FoldChange", "pvalue", "padj")]
				} else {
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "OTU")]]
					t1DF <- t1[,c("OTU","Family","Genus", "baseMean", "pvalue", "padj")]					
				}				
				## Remove excess characters from OTU labels and reset into DF
				OTU <- as.character(t1DF$OTU)
				OTU <- gsub("New.CleanUp.ReferenceOTU", "", OTU)
				OTU <- gsub("New.ReferenceOTU", "", OTU)
				t1DF$OTU <- OTU
				## Round data to the 5 decimal and order data by adjusted pvalue
				t1DF[,!(colnames(t1DF) %in% c("OTU", "Family","Genus"))] <- 
					round(t1DF[,!(colnames(t1DF) %in% c("OTU", "Family","Genus"))], 5)
				t1DF <- t1DF[order(t1DF$padj),]
				
				## Need to set continuous data as numeric or DataTable will not render properly
				t1DF[,!(colnames(t1DF) %in% c("OTU", "Family","Genus"))] <- 
					apply(t1DF[,!(colnames(t1DF) %in% c("OTU", "Family","Genus"))], 2, as.numeric)
				## Create DataTable	
				if(input$dabundNBMtest == "Wald"){		
					datatable(t1DF, extensions='Buttons', rownames = FALSE,
							container = htmltools::withTags(table(
								Order = 'display',
									thead(
									tr(
										th(colspan = 1, 'OTU'),
										th(colspan = 1, 'Family'),
										th(colspan = 1, 'Genus'),
										th(colspan = 1, 'Base Mean'),
										th(colspan = 1, 'Log2 Fold Change'),
										th(colspan = 1, 'P'),
										th(colspan = 1, 'P-adjusted')
									)
								)
							)
						),
							options = list(
								dom = 'lf<"floatright"B>rtip',
								buttons = c('excel', 'pdf', 'csv'),
								searching = TRUE,
								pageLength = 5,
								lengthMenu = c(5, 10, 20, 50, 100)
							)
					) 
				} else {		
					datatable(t1DF, extensions='Buttons', rownames = FALSE,
							container = htmltools::withTags(table(
								class = 'display',
									thead(
									tr(
										th(colspan = 1, 'ID'),
										th(colspan = 1, 'Family'),
										th(colspan = 1, 'Genus'),
										th(colspan = 1, 'Base Mean'),
										th(colspan = 1, 'P'),
										th(colspan = 1, 'P-adjusted')
									)
								)
							)
						),
							options = list(
								dom = 'lf<"floatright"B>rtip',
								buttons = c('excel', 'pdf', 'csv'),
								searching = TRUE,
								pageLength = 5,
								lengthMenu = c(5, nrow(t1DF))
							)
						)
				} 
			} else {
				NULL
			}
		}
	})	

	########################################################################
	## Identify OTU list for OTU boxplot selection and 
	## Phylum
	########################################################################	
	DABUNDphylumOTUlist <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			if("Phylum" %in% names(dabundNBM())) {
				## Extract NBM based on TAXA and Pairwise comparison
				if(input$dabundNBMtest == "Wald"){
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Phylum")]][[input$dabundPAIRCOMPselect]]
				} else {
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Phylum")]]			
				}	
				## Remove excess characters from OTU labels and reset into DF
				OTU <- as.character(t1$OTU)
				OTU <- gsub("New.CleanUp.ReferenceOTU", "", OTU)
				OTU <- gsub("New.ReferenceOTU", "", OTU)
				t1$OTU <- OTU
				## Isolate non character columns and round to the 5th decimal
				t1[,!(colnames(t1) %in% c("OTU","Kingdom", "Phylum", "Class", "Order", "Family", "Genus"))] <- 
					round(t1[,!(colnames(t1) %in% c("OTU","Kingdom", "Phylum", "Class", "Order", "Family", "Genus"))], 5)
				## Order data based on adjusted p value and extract OTUS
				t1 <- t1[order(t1$padj),]
				POTU <- as.character(t1$OTU)
				## Paste TAXA and OTU and then use that as vector names for selectInput choices
				names(POTU) <- paste(t1$Phylum, t1$OTU)
				## return selectInput choices as 'TAXA OTU' in alphabetical order and
				## also return selectInput choice as OTU with lowest adjusted pvalue
				list(Selection=POTU[order(names(POTU))], Choice=t1$OTU[1])
			} else {
				NULL
			}
		}
	})	

	########################################################################
	## Identify OTU list for OTU boxplot selection and 
	## Class
	########################################################################	
	DABUNDclassOTUlist <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			if("Class" %in% names(dabundNBM())) {
				## Extract NBM based on TAXA and Pairwise comparison
				# t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Class")]][[input$dabundPAIRCOMPselect]]
				if(input$dabundNBMtest == "Wald"){
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Class")]][[input$dabundPAIRCOMPselect]]
				} else {
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Class")]]				
				}	
				## Remove excess characters from OTU labels and reset into DF
				OTU <- as.character(t1$OTU)
				OTU <- gsub("New.CleanUp.ReferenceOTU", "", OTU)
				OTU <- gsub("New.ReferenceOTU", "", OTU)
				t1$OTU <- OTU
				## Isolate non character columns and round to the 5th decimal
				t1[,!(colnames(t1) %in% c("OTU","Kingdom", "Phylum", "Class", "Order", "Family", "Genus"))] <- 
					round(t1[,!(colnames(t1) %in% c("OTU","Kingdom", "Phylum", "Class", "Order", "Family", "Genus"))], 5)
				## Order data based on adjusted p value and extract OTUS
				t1 <- t1[order(t1$padj),]
				POTU <- as.character(t1$OTU)
				## Paste TAXA and OTU and then use that as vector names for selectInput choices
				names(POTU) <- paste(t1$Class, t1$OTU)
				## return selectInput choices as 'TAXA OTU' in alphabetical order and
				## also return selectInput choice as OTU with lowest adjusted pvalue
				list(Selection=POTU[order(names(POTU))], Choice=t1$OTU[1])
			} else {
				NULL
			}
		}
	})	

	########################################################################
	## Identify OTU list for OTU boxplot selection and 
	## Order
	########################################################################	
	DABUNDorderOTUlist <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			if("Order" %in% names(dabundNBM())) {
				## Extract NBM based on TAXA and Pairwise comparison
				# t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Order")]][[input$dabundPAIRCOMPselect]]
				if(input$dabundNBMtest == "Wald"){
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Order")]][[input$dabundPAIRCOMPselect]]
				} else {
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Order")]]					
				}	
				## Remove excess characters from OTU labels and reset into DF
				OTU <- as.character(t1$OTU)
				OTU <- gsub("New.CleanUp.ReferenceOTU", "", OTU)
				OTU <- gsub("New.ReferenceOTU", "", OTU)
				t1$OTU <- OTU
				## Isolate non character columns and round to the 5th decimal
				t1[,!(colnames(t1) %in% c("OTU","Kingdom", "Phylum", "Class", "Order", "Family", "Genus"))] <- 
					round(t1[,!(colnames(t1) %in% c("OTU","Kingdom", "Phylum", "Class", "Order", "Family", "Genus"))], 5)
				## Order data based on adjusted p value and extract OTUS
				t1 <- t1[order(t1$padj),]
				POTU <- as.character(t1$OTU)
				## Paste TAXA and OTU and then use that as vector names for selectInput choices
				names(POTU) <- paste(t1$Order, t1$OTU)
				## return selectInput choices as 'TAXA OTU' in alphabetical order and
				## also return selectInput choice as OTU with lowest adjusted pvalue
				list(Selection=POTU[order(names(POTU))], Choice=t1$OTU[1])
			} else {
				NULL
			}
		}
	})	

	########################################################################
	## Identify OTU list for OTU boxplot selection and 
	## Family
	########################################################################	
	DABUNDfamilyOTUlist <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			if("Family" %in% names(dabundNBM())) {
				## Extract NBM based on TAXA and Pairwise comparison
				# t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Family")]][[input$dabundPAIRCOMPselect]]
				if(input$dabundNBMtest == "Wald"){
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Family")]][[input$dabundPAIRCOMPselect]]
				} else {
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Family")]]				
				}	
				## Remove excess characters from OTU labels and reset into DF
				OTU <- as.character(t1$OTU)
				OTU <- gsub("New.CleanUp.ReferenceOTU", "", OTU)
				OTU <- gsub("New.ReferenceOTU", "", OTU)
				t1$OTU <- OTU
				## Isolate non character columns and round to the 5th decimal
				t1[,!(colnames(t1) %in% c("OTU","Kingdom", "Phylum", "Class", "Order", "Family", "Genus"))] <- 
					round(t1[,!(colnames(t1) %in% c("OTU","Kingdom", "Phylum", "Class", "Order", "Family", "Genus"))], 5)
				## Order data based on adjusted p value and extract OTUS
				t1 <- t1[order(t1$padj),]
				POTU <- as.character(t1$OTU)
				## Paste TAXA and OTU and then use that as vector names for selectInput choices
				names(POTU) <- paste(t1$Family, t1$OTU)
				## return selectInput choices as 'TAXA OTU' in alphabetical order and
				## also return selectInput choice as OTU with lowest adjusted pvalue
				list(Selection=POTU[order(names(POTU))], Choice=t1$OTU[1])
			} else {
				NULL
			}
		}
	})	

	########################################################################
	## Identify OTU list for OTU boxplot selection and 
	## Genus
	########################################################################	
	DABUNDgenusOTUlist <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			if("Genus" %in% names(dabundNBM())) {
				## Extract NBM based on TAXA and Pairwise comparison
				# t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Genus")]][[input$dabundPAIRCOMPselect]]
				if(input$dabundNBMtest == "Wald"){
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Genus")]][[input$dabundPAIRCOMPselect]]
				} else {
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "Genus")]]					
				}	
				## Remove excess characters from OTU labels and reset into DF
				OTU <- as.character(t1$OTU)
				OTU <- gsub("New.CleanUp.ReferenceOTU", "", OTU)
				OTU <- gsub("New.ReferenceOTU", "", OTU)
				t1$OTU <- OTU
				## Isolate non character columns and round to the 5th decimal
				t1[,!(colnames(t1) %in% c("OTU","Kingdom", "Phylum", "Class", "Order", "Family", "Genus"))] <- 
					round(t1[,!(colnames(t1) %in% c("OTU","Kingdom", "Phylum", "Class", "Order", "Family", "Genus"))], 5)
				## Order data based on adjusted p value and extract OTUS
				t1 <- t1[order(t1$padj),]
				POTU <- as.character(t1$OTU)
				## Paste TAXA and OTU and then use that as vector names for selectInput choices
				names(POTU) <- paste(t1$Genus, t1$OTU)
				## return selectInput choices as 'TAXA OTU' in alphabetical order and
				## also return selectInput choice as OTU with lowest adjusted pvalue
				list(Selection=POTU[order(names(POTU))], Choice=t1$OTU[1])
			} else {
				NULL
			}
		}
	})	

	########################################################################
	## Identify OTU list for OTU boxplot selection and 
	## OTU
	########################################################################	
	DABUNDOTU_OTUlist <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			if("OTU" %in% names(dabundNBM())) {
				## Extract NBM based on TAXA and Pairwise comparison
				t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "OTU")]][[1]]
				if(input$dabundNBMtest == "Wald"){
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "OTU")]][[1]]
				} else {
					t1 <- dabundNBM()[[which(names(dabundNBM()) %in% "OTU")]]				
				}	
				## Remove excess characters from OTU labels and reset into DF
				OTU <- as.character(t1$OTU)
				OTU <- gsub("New.CleanUp.ReferenceOTU", "", OTU)
				OTU <- gsub("New.ReferenceOTU", "", OTU)
				t1$OTU <- OTU
				## Isolate non character columns and round to the 5th decimal
				t1[,!(colnames(t1) %in% c("OTU","Kingdom", "Phylum", "Class", "Order", "Family", "Genus"))] <- 
					round(t1[,!(colnames(t1) %in% c("OTU","Kingdom", "Phylum", "Class", "Order", "Family", "Genus"))], 5)
				## Order data based on adjusted p value and extract OTUS
				t1 <- t1[order(t1$padj),]
				POTU <- as.character(t1$OTU)
				## Use OTU as vector names for selectInput choices
				names(POTU) <- as.character(t1$OTU)
				## return selectInput choices as 'OTU' in numerical order and
				## also return selectInput choice as OTU with lowest adjusted pvalue
				list(Selection=POTU[order(as.numeric(POTU))], Choice=t1$OTU[1])
			} else {
				NULL
			}
		}
	})	

	########################################################################			
	## Set NBM results for Phylum .CSV download
	########################################################################	
	DABUNDphylumNBMdata <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			## Extract NBM based on TAXA 
			if("Phylum" %in% names(dabundNBM())) {
				LIST <- dabundNBM()[[which(names(dabundNBM()) %in% "Phylum")]]
				## Extract results by Pairwise Comparisons	
				if(input$dabundNBMtest == "Wald"){				
					LISTord <- lapply(names(LIST), function(x) {
						nbm <- LIST[[x]]
						## Order by adjusted pvalue, add new column with pairwise comparison, and return
						nbmord <- nbm[order(nbm$padj),]
						
						nbmord$Comparison <- rep(input$dabundNBMtest, nrow(nbm))
						nbmord$Group <- rep(input$dabundGROUPselect, nrow(nbm))
						nbmord$Pair <- rep(x, nrow(nbm))
						nbmord						
					})
					## Bind list into a single data frame, remove unused columns, set row names, and return
					DF <- do.call("rbind", LISTord)
				} else {
					DF <- LIST[order(LIST$padj),]
							
				}
				FinalDF <- DF[,!(colnames(DF) %in% c("Class", "Order", "Family", "Genus"))]
				
				rownames(FinalDF) <- c(1:nrow(FinalDF))
				if(input$dabundNBMtest == "Wald"){	
					FinalDF$Reference <- factorSPLIT(FinalDF$Pair, " vs ", 2)
				} else {
					FinalDF$Comparison <- rep(input$dabundNBMtest, nrow(FinalDF))
					FinalDF$Group <- rep(input$dabundGROUPselect, nrow(FinalDF))
				}
				FinalDF
			} else {
				NULL
			}
		}	
	})	

	########################################################################			
	## Set NBM results for Class .CSV download
	########################################################################	
	DABUNDclassNBMdata <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			## Extract NBM based on TAXA 
			if("Class" %in% names(dabundNBM())) {
				LIST <- dabundNBM()[[which(names(dabundNBM()) %in% "Class")]]
				## Extract results by Pairwise Comparisons	
				if(input$dabundNBMtest == "Wald"){				
					LISTord <- lapply(names(LIST), function(x) {
						nbm <- LIST[[x]]
						## Order by adjusted pvalue, add new column with pairwise comparison, and return
						nbmord <- nbm[order(nbm$padj),]
						
						nbmord$Comparison <- rep(input$dabundNBMtest, nrow(nbm))
						nbmord$Group <- rep(input$dabundGROUPselect, nrow(nbm))
						nbmord$Pair <- rep(x, nrow(nbm))
						nbmord						
					})
					## Bind list into a single data frame, remove unused columns, set row names, and return
					DF <- do.call("rbind", LISTord)
				} else {
					DF <- LIST[order(LIST$padj),]
							
				}
				FinalDF <- DF[,!(colnames(DF) %in% c("Order", "Family", "Genus"))]
				rownames(FinalDF) <- c(1:nrow(FinalDF))
				if(input$dabundNBMtest == "Wald"){	
					FinalDF$Reference <- factorSPLIT(FinalDF$Pair, " vs ", 2)
				} else {
					FinalDF$Comparison <- rep(input$dabundNBMtest, nrow(FinalDF))
					FinalDF$Group <- rep(input$dabundGROUPselect, nrow(FinalDF))
				}
				FinalDF
			} else {
				NULL
			}
		}
	})	

	########################################################################			
	## Set NBM results for Order .CSV download
	########################################################################	
	DABUNDorderNBMdata <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			## Extract NBM based on TAXA 
			if("Order" %in% names(dabundNBM())) {
				LIST <- dabundNBM()[[which(names(dabundNBM()) %in% "Order")]]
				## Extract results by Pairwise Comparisons	
				if(input$dabundNBMtest == "Wald"){				
					LISTord <- lapply(names(LIST), function(x) {
						nbm <- LIST[[x]]
						## Order by adjusted pvalue, add new column with pairwise comparison, and return
						nbmord <- nbm[order(nbm$padj),]
						
						nbmord$Comparison <- rep(input$dabundNBMtest, nrow(nbm))
						nbmord$Group <- rep(input$dabundGROUPselect, nrow(nbm))
						nbmord$Pair <- rep(x, nrow(nbm))
						nbmord						
					})
					## Bind list into a single data frame, remove unused columns, set row names, and return
					DF <- do.call("rbind", LISTord)
				} else {
					DF <- LIST[order(LIST$padj),]
							
				}
				FinalDF <- DF[,!(colnames(DF) %in% c("Family", "Genus"))]
				rownames(FinalDF) <- c(1:nrow(FinalDF))
				if(input$dabundNBMtest == "Wald"){	
					FinalDF$Reference <- factorSPLIT(FinalDF$Pair, " vs ", 2)
				} else {
					FinalDF$Comparison <- rep(input$dabundNBMtest, nrow(FinalDF))
					FinalDF$Group <- rep(input$dabundGROUPselect, nrow(FinalDF))
				}
				FinalDF
			} else {
				NULL
			}
		}
	})	
			

	########################################################################
	## Set NBM results for Family .CSV download
	########################################################################	
	DABUNDfamilyNBMdata <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			## Extract NBM based on TAXA 
			if("Family" %in% names(dabundNBM())) {
				LIST <- dabundNBM()[[which(names(dabundNBM()) %in% "Family")]]
				## Extract results by Pairwise Comparisons	
				if(input$dabundNBMtest == "Wald"){				
					LISTord <- lapply(names(LIST), function(x) {
						nbm <- LIST[[x]]
						## Order by adjusted pvalue, add new column with pairwise comparison, and return
						nbmord <- nbm[order(nbm$padj),]
						
						nbmord$Comparison <- rep(input$dabundNBMtest, nrow(nbm))
						nbmord$Group <- rep(input$dabundGROUPselect, nrow(nbm))
						nbmord$Pair <- rep(x, nrow(nbm))
						nbmord						
					})
					## Bind list into a single data frame, remove unused columns, set row names, and return
					DF <- do.call("rbind", LISTord)
				} else {
					DF <- LIST[order(LIST$padj),]
							
				}
				FinalDF <- DF[,!(colnames(DF) %in% c("Genus"))]
				rownames(FinalDF) <- c(1:nrow(FinalDF))
				if(input$dabundNBMtest == "Wald"){	
					FinalDF$Reference <- factorSPLIT(FinalDF$Pair, " vs ", 2)
				} else {
					FinalDF$Comparison <- rep(input$dabundNBMtest, nrow(FinalDF))
					FinalDF$Group <- rep(input$dabundGROUPselect, nrow(FinalDF))
				}
				FinalDF
			} else {
				NULL
			}
		}
	})	

	########################################################################			
	## Set NBM results for Genus .CSV download
	########################################################################	
	DABUNDgenusNBMdata <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			## Extract NBM based on TAXA 
			if("Genus" %in% names(dabundNBM())) {
				LIST <- dabundNBM()[[which(names(dabundNBM()) %in% "Genus")]]
				## Extract results by Pairwise Comparisons	
				if(input$dabundNBMtest == "Wald"){				
					LISTord <- lapply(names(LIST), function(x) {
						nbm <- LIST[[x]]
						## Order by adjusted pvalue, add new column with pairwise comparison, and return
						nbmord <- nbm[order(nbm$padj),]
						
						nbmord$Comparison <- rep(input$dabundNBMtest, nrow(nbm))
						nbmord$Group <- rep(input$dabundGROUPselect, nrow(nbm))
						nbmord$Pair <- rep(x, nrow(nbm))
						nbmord						
					})
					## Bind list into a single data frame, remove unused columns, set row names, and return
					DF <- do.call("rbind", LISTord)
				} else {
					DF <- LIST[order(LIST$padj),]
							
				}
				FinalDF <- DF
				rownames(FinalDF) <- c(1:nrow(FinalDF))
				if(input$dabundNBMtest == "Wald"){	
					FinalDF$Reference <- factorSPLIT(FinalDF$Pair, " vs ", 2)
				} else {
					FinalDF$Comparison <- rep(input$dabundNBMtest, nrow(FinalDF))
					FinalDF$Group <- rep(input$dabundGROUPselect, nrow(FinalDF))
				}
				FinalDF
			} else {
				NULL
			}
		}
	})	

	########################################################################			
	## Set NBM results for OTU .CSV download
	########################################################################	
	DABUNDotuNBMdata <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			## Extract NBM based on TAXA 
			if("OTU" %in% names(dabundNBM())) {
				LIST <- dabundNBM()[[which(names(dabundNBM()) %in% "OTU")]]
				## Extract results by Pairwise Comparisons	
				if(input$dabundNBMtest == "Wald"){				
					LISTord <- lapply(names(LIST), function(x) {
						nbm <- LIST[[x]]
						## Order by adjusted pvalue, add new column with pairwise comparison, and return
						nbmord <- nbm[order(nbm$padj),]
						
						nbmord$Comparison <- rep(input$dabundNBMtest, nrow(nbm))
						nbmord$Group <- rep(input$dabundGROUPselect, nrow(nbm))
						nbmord$Pair <- rep(x, nrow(nbm))
						nbmord						
					})
					## Bind list into a single data frame, remove unused columns, set row names, and return
					DF <- do.call("rbind", LISTord)
				} else {
					DF <- LIST[order(LIST$padj),]
							
				}
				FinalDF <- DF
				rownames(FinalDF) <- c(1:nrow(FinalDF))
				if(input$dabundNBMtest == "Wald"){	
					FinalDF$Reference <- factorSPLIT(FinalDF$Pair, " vs ", 2)
				} else {
					FinalDF$Comparison <- rep(input$dabundNBMtest, nrow(FinalDF))
					FinalDF$Group <- rep(input$dabundGROUPselect, nrow(FinalDF))
				}
				FinalDF
			} else {
				NULL
			}
		}
	})	

	########################################################################
	## Toggle hidden Phylum data download options
	########################################################################	
	shinyjs::onclick("togglephylumRABUND_DataDownload",
        shinyjs::toggle(id = "phylumRABUND_DataDownload", anim = TRUE)
	)  	

	## Download Phylum NBM data option
	########################################################################	
	output$phylumRABUNDData_download <- downloadHandler(
		
		filename = function() { 
			paste(input$phylumRABUND_Datafilename, '.csv', sep='') 
		},
		content = function(file) {
			write.csv(DABUNDphylumNBMdata(), file)
		}
	)

	########################################################################
	## Toggle hidden Class data download options
	########################################################################	
	shinyjs::onclick("toggleclassRABUND_DataDownload",
        shinyjs::toggle(id = "classRABUND_DataDownload", anim = TRUE)
	)  	

	########################################################################
	## Download Class NBM data option
	########################################################################	
	output$classRABUNDData_download <- downloadHandler(
		
		filename = function() { 
			paste(input$classRABUND_Datafilename, '.csv', sep='') 
		},
		content = function(file) {
			write.csv(DABUNDclassNBMdata(), file)
		}
	)

	########################################################################
	## Toggle hidden Order data download options
	########################################################################	
	shinyjs::onclick("toggleorderRABUND_DataDownload",
        shinyjs::toggle(id = "orderRABUND_DataDownload", anim = TRUE)
	)  	

	########################################################################
	## Download Order NBM data option
	########################################################################	
	output$orderRABUNDData_download <- downloadHandler(
		
		filename = function() { 
			paste(input$orderRABUND_Datafilename, '.csv', sep='') 
		},
		content = function(file) {
			write.csv(DABUNDorderNBMdata(), file)
		}
	)

	########################################################################
	## Toggle hidden Family data download options
	########################################################################	
	shinyjs::onclick("togglefamilyRABUND_DataDownload",
        shinyjs::toggle(id = "familyRABUND_DataDownload", anim = TRUE)
	)  	

	########################################################################
	## Download Family NBM data option
	########################################################################	
	output$familyRABUNDData_download <- downloadHandler(
		
		filename = function() { 
			paste(input$familyRABUND_Datafilename, '.csv', sep='') 
		},
		content = function(file) {
			write.csv(DABUNDfamilyNBMdata(), file)
		}
	)

	########################################################################
	## Toggle hidden Genus data download options
	########################################################################	
	shinyjs::onclick("togglegenusRABUND_DataDownload",
        shinyjs::toggle(id = "genusRABUND_DataDownload", anim = TRUE)
	)  	

	########################################################################
	## Download Genus NBM data option
	########################################################################	
	output$genusRABUNDData_download <- downloadHandler(
		
		filename = function() { 
			paste(input$genusRABUND_Datafilename, '.csv', sep='') 
		},
		content = function(file) {
			write.csv(DABUNDgenusNBMdata(), file)
		}
	)

	########################################################################
	## Toggle hidden OTU data download options
	########################################################################	
	shinyjs::onclick("toggleotuRABUND_DataDownload",
        shinyjs::toggle(id = "otuRABUND_DataDownload", anim = TRUE)
	)  	

	########################################################################
	## Download OTU NBM data option
	########################################################################	
	output$otuRABUNDData_download <- downloadHandler(
		
		filename = function() { 
			paste(input$otuRABUND_Datafilename, '.csv', sep='') 
		},
		content = function(file) {
			write.csv(DABUNDotuNBMdata(), file)
		}
	)

	########################################################################
	## Make reactive object if there are more than one pairwise combination in Experimental group
	## ie, there are > 2 factor levels in Experimental Group selection
	########################################################################	
	RABUNDnumbcompRENDER <- reactive({ 
		req(DABUNDpaircomp())
		if(input$dabundNBMtest == "Wald") {	
			if(length(DABUNDpaircomp()) > 1) {
				"More than one levels in group factor"
			} else {
				NULL
			}
		} else {
			NULL
		}
	})
	
	########################################################################				
	## Set data for Phylum Boxplots
	########################################################################	
	RABUNDphylumbpdata <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			if("Phylum" %in% names(dabundNBM())) {
				## Get both reads and relative abundance data based on TAXA
				pob_reads <- daTAXALEVEL()[[which(names(daTAXALEVEL()) %in% "Phylum")]]
				pob_RA <- daTAXALEVEL_RA()[[which(names(daTAXALEVEL_RA()) %in% "Phylum")]]
				## If reads is selected for boxplot data
				if(input$RABUNDphylumBPdatatype == "reads") { 
					## Extract OTU, TAXA, and SAMPLE data from phyloseq object
					OTU <- otu_table(pob_reads)@.Data
					rownames(OTU) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(OTU))
					rownames(OTU) <- gsub("New.ReferenceOTU", "", rownames(OTU))
					TAXA <- tax_table(pob_reads)@.Data
					rownames(TAXA) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(TAXA))
					rownames(TAXA) <- gsub("New.ReferenceOTU", "", rownames(TAXA))
					## SAMPLE data extracts as a list, so turn into data frame, set column names, 
					## and add row names as new column
					SAMPLE <- as.data.frame(sample_data(pob_reads)@.Data)
					colnames(SAMPLE) <- colnames(sample_data(pob_reads))
					SAMPLE$ID <- rownames(sample_data(pob_reads))
				} else {
					## If relative abundance is selected for boxplot data
					## Extract OTU, TAXA, and SAMPLE data from phyloseq object
					OTU <- otu_table(pob_RA)@.Data
					rownames(OTU) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(OTU))
					rownames(OTU) <- gsub("New.ReferenceOTU", "", rownames(OTU))
					TAXA <- tax_table(pob_RA)@.Data
					rownames(TAXA) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(TAXA))
					rownames(TAXA) <- gsub("New.ReferenceOTU", "", rownames(TAXA))
					## SAMPLE data extracts as a list, so turn into data frame, set column names, 
					## and add row names as new column
					SAMPLE <- as.data.frame(sample_data(pob_RA)@.Data)
					colnames(SAMPLE) <- colnames(sample_data(pob_reads))
					SAMPLE$ID <- rownames(sample_data(pob_reads))			
				}
				## add selected OTU as a column in SAMPLE data and 
				## subset Experimental Group and value columns
				SAMPLE$value <- OTU[input$dabundPBACTERIAselect,]
				plotDATA <- SAMPLE[,c(input$dabundGROUPselect, "value")]
				## Since NBM assess pairwise comparisons, the return will default to the 
				## selected pairwise comparison.
				## If there is only 1 set of pairwise comparisons, then there will not be
				## an option to expand the data to show more than two groups
				if(input$dabundNBMtest == "Wald") {
					if(is.null(input$RABUNDphylumbpgroup)){
						plotDATA <- plotDATA
					} else {
						if(input$RABUNDphylumbpgroup == 1) {
							## If > 1 pairwise comparison and only want to show 2 groups
							plotDATA <- droplevels(plotDATA[plotDATA[,input$dabundGROUPselect] %in% strsplit(input$dabundPAIRCOMPselect, " vs ")[[1]],])
						} else {
							## If > 1 pairwise comparison and want to show all groups
							plotDATA <- plotDATA
						}
					}
				} else {
					plotDATA <- plotDATA
				}
				plotDATA		
			} else {
				NULL
			}
		}
	})	

	########################################################################				
	## Set data for Class Boxplots
	########################################################################	
	RABUNDclassbpdata <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			if("Class" %in% names(dabundNBM())) {
				## Get both reads and relative abundance data based on TAXA
				pob_reads <- daTAXALEVEL()[[which(names(daTAXALEVEL()) %in% "Class")]]
				pob_RA <- daTAXALEVEL_RA()[[which(names(daTAXALEVEL_RA()) %in% "Class")]]
				## If reads is selected for boxplot data
				if(input$RABUNDclassBPdatatype == "reads") { 
					## Extract OTU, TAXA, and SAMPLE data from phyloseq object
					OTU <- otu_table(pob_reads)@.Data
					rownames(OTU) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(OTU))
					rownames(OTU) <- gsub("New.ReferenceOTU", "", rownames(OTU))
					TAXA <- tax_table(pob_reads)@.Data
					rownames(TAXA) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(TAXA))
					rownames(TAXA) <- gsub("New.ReferenceOTU", "", rownames(TAXA))
					## SAMPLE data extracts as a list, so turn into data frame, set column names, 
					## and add row names as new column
					SAMPLE <- as.data.frame(sample_data(pob_reads)@.Data)
					colnames(SAMPLE) <- colnames(sample_data(pob_reads))
					SAMPLE$ID <- rownames(sample_data(pob_reads))
				} else {
					## If relative abundance is selected for boxplot data
					## Extract OTU, TAXA, and SAMPLE data from phyloseq object
					OTU <- otu_table(pob_RA)@.Data
					rownames(OTU) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(OTU))
					rownames(OTU) <- gsub("New.ReferenceOTU", "", rownames(OTU))
					TAXA <- tax_table(pob_RA)@.Data
					rownames(TAXA) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(TAXA))
					rownames(TAXA) <- gsub("New.ReferenceOTU", "", rownames(TAXA))
					## SAMPLE data extracts as a list, so turn into data frame, set column names, 
					## and add row names as new column
					SAMPLE <- as.data.frame(sample_data(pob_RA)@.Data)
					colnames(SAMPLE) <- colnames(sample_data(pob_reads))
					SAMPLE$ID <- rownames(sample_data(pob_reads))			
				}
				## add selected OTU as a column in SAMPLE data and 
				## subset Experimental Group and value columns
				SAMPLE$value <- OTU[input$dabundCBACTERIAselect,]
				plotDATA <- SAMPLE[,c(input$dabundGROUPselect, "value")]
				## Since NBM assess pairwise comparisons, the return will default to the 
				## selected pairwise comparison.
				## If there is only 1 set of pairwise comparisons, then there will not be
				## an option to expand the data to show more than two groups
				if(input$dabundNBMtest == "Wald") {
					if(is.null(input$RABUNDclassbpgroup)){
						plotDATA <- plotDATA
					} else {
						if(input$RABUNDclassbpgroup == 1) {
							## If > 1 pairwise comparison and only want to show 2 groups
							plotDATA <- droplevels(plotDATA[plotDATA[,input$dabundGROUPselect] %in% strsplit(input$dabundPAIRCOMPselect, " vs ")[[1]],])
						} else {
							## If > 1 pairwise comparison and want to show all groups
							plotDATA <- plotDATA
						}
					}
				} else {
					plotDATA <- plotDATA
				}
				plotDATA		

			} else {
				NULL
			}
		}
	})	

	########################################################################				
	## Set data for Order Boxplots
	########################################################################	
	RABUNDorderbpdata <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			if("Order" %in% names(dabundNBM())) {
				## Get both reads and relative abundance data based on TAXA
				pob_reads <- daTAXALEVEL()[[which(names(daTAXALEVEL()) %in% "Order")]]
				pob_RA <- daTAXALEVEL_RA()[[which(names(daTAXALEVEL_RA()) %in% "Order")]]
				## If reads is selected for boxplot data
				if(input$RABUNDorderBPdatatype == "reads") { 
					## Extract OTU, TAXA, and SAMPLE data from phyloseq object
					OTU <- otu_table(pob_reads)@.Data
					rownames(OTU) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(OTU))
					rownames(OTU) <- gsub("New.ReferenceOTU", "", rownames(OTU))
					TAXA <- tax_table(pob_reads)@.Data
					rownames(TAXA) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(TAXA))
					rownames(TAXA) <- gsub("New.ReferenceOTU", "", rownames(TAXA))
					## SAMPLE data extracts as a list, so turn into data frame, set column names, 
					## and add row names as new column
					SAMPLE <- as.data.frame(sample_data(pob_reads)@.Data)
					colnames(SAMPLE) <- colnames(sample_data(pob_reads))
					SAMPLE$ID <- rownames(sample_data(pob_reads))
				} else {
					## If relative abundance is selected for boxplot data
					## Extract OTU, TAXA, and SAMPLE data from phyloseq object
					OTU <- otu_table(pob_RA)@.Data
					rownames(OTU) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(OTU))
					rownames(OTU) <- gsub("New.ReferenceOTU", "", rownames(OTU))
					TAXA <- tax_table(pob_RA)@.Data
					rownames(TAXA) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(TAXA))
					rownames(TAXA) <- gsub("New.ReferenceOTU", "", rownames(TAXA))
					## SAMPLE data extracts as a list, so turn into data frame, set column names, 
					## and add row names as new column
					SAMPLE <- as.data.frame(sample_data(pob_RA)@.Data)
					colnames(SAMPLE) <- colnames(sample_data(pob_reads))
					SAMPLE$ID <- rownames(sample_data(pob_reads))			
				}
				## add selected OTU as a column in SAMPLE data and 
				## subset Experimental Group and value columns
				SAMPLE$value <- OTU[input$dabundOBACTERIAselect,]
				plotDATA <- SAMPLE[,c(input$dabundGROUPselect, "value")]
				## Since NBM assess pairwise comparisons, the return will default to the 
				## selected pairwise comparison.
				## If there is only 1 set of pairwise comparisons, then there will not be
				## an option to expand the data to show more than two groups
				if(input$dabundNBMtest == "Wald") {
					if(is.null(input$RABUNDorderbpgroup)){
						plotDATA <- plotDATA
					} else {
						if(input$RABUNDorderbpgroup == 1) {
							## If > 1 pairwise comparison and only want to show 2 groups
							plotDATA <- droplevels(plotDATA[plotDATA[,input$dabundGROUPselect] %in% strsplit(input$dabundPAIRCOMPselect, " vs ")[[1]],])
						} else {
							## If > 1 pairwise comparison and want to show all groups
							plotDATA <- plotDATA
						}
					}
				} else {
					plotDATA <- plotDATA
				}
				plotDATA		

			} else {
				NULL
			}
		}
	})	

	########################################################################				
	## Set data for Family Boxplots
	########################################################################	
	RABUNDfamilybpdata <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			if("Family" %in% names(dabundNBM())) {
				## Get both reads and relative abundance data based on TAXA
				pob_reads <- daTAXALEVEL()[[which(names(daTAXALEVEL()) %in% "Family")]]
				pob_RA <- daTAXALEVEL_RA()[[which(names(daTAXALEVEL_RA()) %in% "Family")]]
				## If reads is selected for boxplot data
				if(input$RABUNDfamilyBPdatatype == "reads") { 
					## Extract OTU, TAXA, and SAMPLE data from phyloseq object
					OTU <- otu_table(pob_reads)@.Data
					rownames(OTU) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(OTU))
					rownames(OTU) <- gsub("New.ReferenceOTU", "", rownames(OTU))
					TAXA <- tax_table(pob_reads)@.Data
					rownames(TAXA) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(TAXA))
					rownames(TAXA) <- gsub("New.ReferenceOTU", "", rownames(TAXA))
					## SAMPLE data extracts as a list, so turn into data frame, set column names, 
					## and add row names as new column
					SAMPLE <- as.data.frame(sample_data(pob_reads)@.Data)
					colnames(SAMPLE) <- colnames(sample_data(pob_reads))
					SAMPLE$ID <- rownames(sample_data(pob_reads))
				} else {
					## If relative abundance is selected for boxplot data
					## Extract OTU, TAXA, and SAMPLE data from phyloseq object
					OTU <- otu_table(pob_RA)@.Data
					rownames(OTU) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(OTU))
					rownames(OTU) <- gsub("New.ReferenceOTU", "", rownames(OTU))
					TAXA <- tax_table(pob_RA)@.Data
					rownames(TAXA) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(TAXA))
					rownames(TAXA) <- gsub("New.ReferenceOTU", "", rownames(TAXA))
					## SAMPLE data extracts as a list, so turn into data frame, set column names, 
					## and add row names as new column
					SAMPLE <- as.data.frame(sample_data(pob_RA)@.Data)
					colnames(SAMPLE) <- colnames(sample_data(pob_reads))
					SAMPLE$ID <- rownames(sample_data(pob_reads))			
				}
				## add selected OTU as a column in SAMPLE data and 
				## subset Experimental Group and value columns
				SAMPLE$value <- OTU[input$dabundFBACTERIAselect,]
				plotDATA <- SAMPLE[,c(input$dabundGROUPselect, "value")]
				## Since NBM assess pairwise comparisons, the return will default to the 
				## selected pairwise comparison.
				## If there is only 1 set of pairwise comparisons, then there will not be
				## an option to expand the data to show more than two groups
				if(input$dabundNBMtest == "Wald") {
					if(is.null(input$RABUNDfamilybpgroup)){
						plotDATA <- plotDATA
					} else {
						if(input$RABUNDfamilybpgroup == 1) {
							## If > 1 pairwise comparison and only want to show 2 groups
							plotDATA <- droplevels(plotDATA[plotDATA[,input$dabundGROUPselect] %in% strsplit(input$dabundPAIRCOMPselect, " vs ")[[1]],])
						} else {
							## If > 1 pairwise comparison and want to show all groups
							plotDATA <- plotDATA
						}
					}
				} else {
					plotDATA <- plotDATA
				}
				plotDATA		

			} else {
				NULL
			}
		}
	})	
	
	########################################################################			
	## Set data for Genus Boxplots
	########################################################################	
	RABUNDgenusbpdata <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			if("Genus" %in% names(dabundNBM())) {
				## Get both reads and relative abundance data based on TAXA
				pob_reads <- daTAXALEVEL()[[which(names(daTAXALEVEL()) %in% "Genus")]]
				pob_RA <- daTAXALEVEL_RA()[[which(names(daTAXALEVEL_RA()) %in% "Genus")]]
				## If reads is selected for boxplot data
				if(input$RABUNDgenusBPdatatype == "reads") { 
					## Extract OTU, TAXA, and SAMPLE data from phyloseq object
					OTU <- otu_table(pob_reads)@.Data
					rownames(OTU) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(OTU))
					rownames(OTU) <- gsub("New.ReferenceOTU", "", rownames(OTU))
					TAXA <- tax_table(pob_reads)@.Data
					rownames(TAXA) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(TAXA))
					rownames(TAXA) <- gsub("New.ReferenceOTU", "", rownames(TAXA))
					## SAMPLE data extracts as a list, so turn into data frame, set column names, 
					## and add row names as new column
					SAMPLE <- as.data.frame(sample_data(pob_reads)@.Data)
					colnames(SAMPLE) <- colnames(sample_data(pob_reads))
					SAMPLE$ID <- rownames(sample_data(pob_reads))
				} else {
					## If relative abundance is selected for boxplot data
					## Extract OTU, TAXA, and SAMPLE data from phyloseq object
					OTU <- otu_table(pob_RA)@.Data
					rownames(OTU) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(OTU))
					rownames(OTU) <- gsub("New.ReferenceOTU", "", rownames(OTU))
					TAXA <- tax_table(pob_RA)@.Data
					rownames(TAXA) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(TAXA))
					rownames(TAXA) <- gsub("New.ReferenceOTU", "", rownames(TAXA))
					## SAMPLE data extracts as a list, so turn into data frame, set column names, 
					## and add row names as new column
					SAMPLE <- as.data.frame(sample_data(pob_RA)@.Data)
					colnames(SAMPLE) <- colnames(sample_data(pob_reads))
					SAMPLE$ID <- rownames(sample_data(pob_reads))			
				}
				## add selected OTU as a column in SAMPLE data and 
				## subset Experimental Group and value columns
				SAMPLE$value <- OTU[input$dabundGBACTERIAselect,]
				plotDATA <- SAMPLE[,c(input$dabundGROUPselect, "value")]
				## Since NBM assess pairwise comparisons, the return will default to the 
				## selected pairwise comparison.
				## If there is only 1 set of pairwise comparisons, then there will not be
				## an option to expand the data to show more than two groups
				if(input$dabundNBMtest == "Wald") {
					if(is.null(input$RABUNDgenusbpgroup)){
						plotDATA <- plotDATA
					} else {
						if(input$RABUNDgenusbpgroup == 1) {
							## If > 1 pairwise comparison and only want to show 2 groups
							plotDATA <- droplevels(plotDATA[plotDATA[,input$dabundGROUPselect] %in% strsplit(input$dabundPAIRCOMPselect, " vs ")[[1]],])
						} else {
							## If > 1 pairwise comparison and want to show all groups
							plotDATA <- plotDATA
						}
					}
				} else {
					plotDATA <- plotDATA
				}
				plotDATA		

			} else {
				NULL
			}
		}
	})	

	########################################################################				
	## Set data for OTU Boxplots
	########################################################################	
	RABUNDotubpdata <- reactive({
		if(is.null(dabundNBM())) {
			NULL
		} else {
			if("OTU" %in% names(dabundNBM())) {
				## Get both reads and relative abundance data based on TAXA
				pob_reads <- daTAXALEVEL()[[which(names(daTAXALEVEL()) %in% "OTU")]]
				pob_RA <- daTAXALEVEL_RA()[[which(names(daTAXALEVEL_RA()) %in% "OTU")]]
				## If reads is selected for boxplot data
				if(input$RABUNDotuBPdatatype == "reads") { 
					## Extract OTU, TAXA, and SAMPLE data from phyloseq object
					OTU <- otu_table(pob_reads)@.Data
					rownames(OTU) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(OTU))
					rownames(OTU) <- gsub("New.ReferenceOTU", "", rownames(OTU))
					TAXA <- tax_table(pob_reads)@.Data
					rownames(TAXA) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(TAXA))
					rownames(TAXA) <- gsub("New.ReferenceOTU", "", rownames(TAXA))
					## SAMPLE data extracts as a list, so turn into data frame, set column names, 
					## and add row names as new column
					SAMPLE <- as.data.frame(sample_data(pob_reads)@.Data)
					colnames(SAMPLE) <- colnames(sample_data(pob_reads))
					SAMPLE$ID <- rownames(sample_data(pob_reads))
				} else {
					## If relative abundance is selected for boxplot data
					## Extract OTU, TAXA, and SAMPLE data from phyloseq object
					OTU <- otu_table(pob_RA)@.Data
					rownames(OTU) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(OTU))
					rownames(OTU) <- gsub("New.ReferenceOTU", "", rownames(OTU))
					TAXA <- tax_table(pob_RA)@.Data
					rownames(TAXA) <- gsub("New.CleanUp.ReferenceOTU", "", rownames(TAXA))
					rownames(TAXA) <- gsub("New.ReferenceOTU", "", rownames(TAXA))
					## SAMPLE data extracts as a list, so turn into data frame, set column names, 
					## and add row names as new column
					SAMPLE <- as.data.frame(sample_data(pob_RA)@.Data)
					colnames(SAMPLE) <- colnames(sample_data(pob_reads))
					SAMPLE$ID <- rownames(sample_data(pob_reads))			
				}
				## add selected OTU as a column in SAMPLE data and 
				## subset Experimental Group and value columns
				SAMPLE$value <- OTU[input$dabundOTUBACTERIAselect,]
				plotDATA <- SAMPLE[,c(input$dabundGROUPselect, "value")]
				## Since NBM assess pairwise comparisons, the return will default to the 
				## selected pairwise comparison.
				## If there is only 1 set of pairwise comparisons, then there will not be
				## an option to expand the data to show more than two groups
				if(input$dabundNBMtest == "Wald") {
					if(is.null(input$RABUNDotubpgroup)){
						plotDATA <- plotDATA
					} else {
						if(input$RABUNDotubpgroup == 1) {
							## If > 1 pairwise comparison and only want to show 2 groups
							plotDATA <- droplevels(plotDATA[plotDATA[,input$dabundGROUPselect] %in% strsplit(input$dabundPAIRCOMPselect, " vs ")[[1]],])
						} else {
							## If > 1 pairwise comparison and want to show all groups
							plotDATA <- plotDATA
						}
					}
				} else {
					plotDATA <- plotDATA
				}
				plotDATA		

			} else {
				NULL
			}
		}
	})	

	########################################################################
	## Create rbokeh object with Phylum boxplot data
	########################################################################	
	output$DABUNDphylumboxplotRENDER <- renderRbokeh({
		req(RABUNDphylumbpdata())
		bpdata <- RABUNDphylumbpdata()
		colnames(bpdata)[1] <- "Level"
		
		boxfig <- figure(data = bpdata, legend_location=NULL)
		if(input$RABUNDphylumBPdatatype == "reads") {
			boxfig <- boxfig %>% ly_points(catjitter(Level), value, color = "black", hover = list("Group" = Level, "Reads" = value), 
				xlab = "", ylab="Sequence reads") 
		} else {
			boxfig <- boxfig %>% ly_points(catjitter(Level), value, color = "black", hover = list("Group" = Level, "Relative Abundance" = value), 
				xlab = "", ylab="Relative abundance, %")	
		}
		boxfig %>% ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
				set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)		
				
	})

	########################################################################
	## Create highcharter object with Class boxplot data
	########################################################################	
	output$DABUNDclassboxplotRENDER <- renderRbokeh({
		req(RABUNDclassbpdata())
		bpdata <- RABUNDclassbpdata()
		colnames(bpdata)[1] <- "Level"
		
		boxfig <- figure(data = bpdata, legend_location=NULL)
		if(input$RABUNDclassBPdatatype == "reads") {
			boxfig <- boxfig %>% ly_points(catjitter(Level), value, color = "black", hover = list("Group" = Level, "Reads" = value), 
				xlab = "", ylab="Sequence reads") 
		} else {
			boxfig <- boxfig %>% ly_points(catjitter(Level), value, color = "black", hover = list("Group" = Level, "Relative Abundance" = value), 
				xlab = "", ylab="Relative abundance, %")	
		}
		boxfig %>% ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
				set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)		
			
	})

	########################################################################		
	## Create highcharter object with Order boxplot data
	########################################################################	
	output$DABUNDorderboxplotRENDER <- renderRbokeh({
		req(RABUNDorderbpdata())
		bpdata <- RABUNDorderbpdata()
		colnames(bpdata)[1] <- "Level"
		
		boxfig <- figure(data = bpdata, legend_location=NULL)
		if(input$RABUNDorderBPdatatype == "reads") {
			boxfig <- boxfig %>% ly_points(catjitter(Level), value, color = "black", hover = list("Group" = Level, "Reads" = value), 
				xlab = "", ylab="Sequence reads") 
		} else {
			boxfig <- boxfig %>% ly_points(catjitter(Level), value, color = "black", hover = list("Group" = Level, "Relative Abundance" = value), 
				xlab = "", ylab="Relative abundance, %")	
		}
		boxfig %>% ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
				set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)		
			
	})

	########################################################################
	## Create highcharter object with Family boxplot data
	########################################################################	
	output$DABUNDfamilyboxplotRENDER <- renderRbokeh({
		req(RABUNDfamilybpdata())
		bpdata <- RABUNDfamilybpdata()
		colnames(bpdata)[1] <- "Level"
		
		boxfig <- figure(data = bpdata, legend_location=NULL)
		if(input$RABUNDfamilyBPdatatype == "reads") {
			boxfig <- boxfig %>% ly_points(catjitter(Level), value, color = "black", hover = list("Group" = Level, "Reads" = value), 
				xlab = "", ylab="Sequence reads") 
		} else {
			boxfig <- boxfig %>% ly_points(catjitter(Level), value, color = "black", hover = list("Group" = Level, "Relative Abundance" = value), 
				xlab = "", ylab="Relative abundance, %")	
		}
		boxfig %>% ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
				set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)		
			
	})

	########################################################################
	## Create highcharter object with Genus boxplot data
		
	output$DABUNDgenusboxplotRENDER <- renderRbokeh({
		req(RABUNDgenusbpdata())
		bpdata <- RABUNDgenusbpdata()
		colnames(bpdata)[1] <- "Level"
		
		boxfig <- figure(data = bpdata, legend_location=NULL)
		if(input$RABUNDgenusBPdatatype == "reads") {
			boxfig <- boxfig %>% ly_points(catjitter(Level), value, color = "black", hover = list("Group" = Level, "Reads" = value), 
				xlab = "", ylab="Sequence reads") 
		} else {
			boxfig <- boxfig %>% ly_points(catjitter(Level), value, color = "black", hover = list("Group" = Level, "Relative Abundance" = value), 
				xlab = "", ylab="Relative abundance, %")	
		}
		boxfig %>% ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
				set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)		
			
	})

	########################################################################
	## Create highcharter object with OTU boxplot data
	########################################################################	
	output$DABUNDotuboxplotRENDER <- renderRbokeh({
		req(RABUNDotubpdata())
		bpdata <- RABUNDotubpdata()
		colnames(bpdata)[1] <- "Level"
		
		boxfig <- figure(data = bpdata, legend_location=NULL)
		if(input$RABUNDotuBPdatatype == "reads") {
			boxfig <- boxfig %>% ly_points(catjitter(Level), value, color = "black", hover = list("Group" = Level, "Reads" = value), 
				xlab = "", ylab="Sequence reads") 
		} else {
			boxfig <- boxfig %>% ly_points(catjitter(Level), value, color = "black", hover = list("Group" = Level, "Relative Abundance" = value), 
				xlab = "", ylab="Relative abundance, %")	
		}
		boxfig %>% ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
				set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)		
			
	})

	output$DABUNDphylumtesttabletextRENDER <- renderUI({
		if(input$goDABUND){
			if(input$dabundNBMtest == "Wald") {
					list(
						HTML("
							<p>Assessing pairwise comparisons with Wald Test</p>
							<p>Reference group is ", factorSPLIT(input$dabundPAIRCOMPselect, " vs ", 2), "</p>
							 "
						)
					)
				} else {
						HTML("
							<p>Assessing overall group effect using Likelihood Ratio Test</p>
							 "
						)					
				}
			
		} else {
			NULL
		}					
	})

	output$DABUNDclasstesttabletextRENDER <- renderUI({
		if(input$goDABUND){
			if(input$dabundNBMtest == "Wald") {
					list(
						HTML("
							<p>Assessing pairwise comparisons with Wald Test</p>
							<p>Reference group is ", factorSPLIT(input$dabundPAIRCOMPselect, " vs ", 2), "</p>
							 "
						)
					)
				} else {
						HTML("
							<p>Assessing overall group effect using Likelihood Ratio Test</p>
							 "
						)					
				}
			
		} else {
			NULL
		}					
	})

	output$DABUNDordertesttabletextRENDER <- renderUI({
		if(input$goDABUND){
			if(input$dabundNBMtest == "Wald") {
					list(
						HTML("
							<p>Assessing pairwise comparisons with Wald Test</p>
							<p>Reference group is ", factorSPLIT(input$dabundPAIRCOMPselect, " vs ", 2), "</p>
							 "
						)
					)
				} else {
						HTML("
							<p>Assessing overall group effect using Likelihood Ratio Test</p>
							 "
						)					
				}
			
		} else {
			NULL
		}					
	})

	output$DABUNDfamilytesttabletextRENDER <- renderUI({
		if(input$goDABUND){
			if(input$dabundNBMtest == "Wald") {
					list(
						HTML("
							<p>Assessing pairwise comparisons with Wald Test</p>
							<p>Reference group is ", factorSPLIT(input$dabundPAIRCOMPselect, " vs ", 2), "</p>
							 "
						)
					)
				} else {
						HTML("
							<p>Assessing overall group effect using Likelihood Ratio Test</p>
							 "
						)					
				}
			
		} else {
			NULL
		}					
	})

	output$DABUNDgenustesttabletextRENDER <- renderUI({
		if(input$goDABUND){
			if(input$dabundNBMtest == "Wald") {
					list(
						HTML("
							<p>Assessing pairwise comparisons with Wald Test</p>
							<p>Reference group is ", factorSPLIT(input$dabundPAIRCOMPselect, " vs ", 2), "</p>
							 "
						)
					)
				} else {
						HTML("
							<p>Assessing overall group effect using Likelihood Ratio Test</p>
							 "
						)					
				}
			
		} else {
			NULL
		}					
	})

	output$DABUNDotutesttabletextRENDER <- renderUI({
		if(input$goDABUND){
			if(input$dabundNBMtest == "Wald") {
					list(
						HTML("
							<p>Assessing pairwise comparisons with Wald Test</p>
							<p>Reference group is ", factorSPLIT(input$dabundPAIRCOMPselect, " vs ", 2), "</p>
							 "
						)
					)
				} else {
						HTML("
							<p>Assessing overall group effect using Likelihood Ratio Test</p>
							 "
						)					
				}
			
		} else {
			NULL
		}					
	})

	########################################################################
	## Render Phylum UI after goDABUND observed
	########################################################################	
	output$DABUNDphylumBPoptionsRENDER <- renderUI({
		if(input$goDABUND){
			if("Phylum" %in% DABUNDtaxa()){	
				list(
					hr(class="hr_SINGLE_sep"),
					fluidPage(
						column(6,
							HTML("
								<h3><strong>Differential Abundance Results at <em>Phylum</em> level.</strong></h3>
								"
							)
						),
						column(6,
							HTML("
								<h3><strong>Differential Abundance Boxplots <em>Phylum</em> level.</strong></h3>
								"
							)
						)
					),
					fluidPage(
						column(3,
							a(id = "togglephylumRABUND_DataDownload", "Show/hide Differential Abundance Downloading Options", href = "#"),
							p(),
							shinyjs::hidden(
								div(id = "phylumRABUND_DataDownload",	
									textInput("phylumRABUND_Datafilename", label = "Choose File Name", value = "Differential Abundance Data"),
									tags$p(
										tags$a(id = "phylumRABUNDData_download", href = "#", class = "shiny-download-link", target="_blank",
											style="color: #fff; background-color: #2c2cad; border-color: #000; padding-top: 8px;
											padding-right: 8px; padding-bottom: 8px; padding-left: 8px; border-radius: 5px;",
											HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download Relative Abundance Data")
										)
									)	
								)
							)
						),
						column(3
						),
						column(3,
							HTML("
								<h4><strong>Select Bacteria:</strong></h4>
								"
							),			

							selectInput(inputId="dabundPBACTERIAselect", label="", choices=DABUNDphylumOTUlist()$Selection, 
								selected=DABUNDphylumOTUlist()$Choice)
						),
						column(3,
							radioButtons("RABUNDphylumBPdatatype", label = "Select Data Type",
								choices = list("Total Reads" = "reads", "Percent Abundance" = "PA"), selected = "reads"
							),
							{
								if(is.null(RABUNDnumbcompRENDER())) {
									NULL
								} else {
									radioButtons("RABUNDphylumbpgroup", label = "Show all Groups",
										choices = list("NO" = 1, "YES" = 2), selected = 1)
								}
							}
							
						)
					),
					fluidPage(
						column(6,				
							DT::dataTableOutput("DABUNDphylumNBMtableRENDER"),
							uiOutput("DABUNDphylumtesttabletextRENDER")
							
						),
						column(6,
							rbokehOutput("DABUNDphylumboxplotRENDER", height="500px", width="1000px")
							
						)
					)
				)
			} else {
				return(NULL)
			}
		} else {
			NULL
		}								
	})		

	########################################################################
	## Render Class UI after goDABUND observed
	########################################################################	
	output$DABUNDclassBPoptionsRENDER <- renderUI({
		if(input$goDABUND){
			if("Class" %in% DABUNDtaxa()){	
				list(
					hr(class="hr_SINGLE_sep"),
					fluidPage(
						column(6,
							HTML("
								<h3><strong>Differential Abundance Results at <em>Class</em> level.</strong></h3>
								"
							)
						),
						column(6,
							HTML("
								<h3><strong>Differential Abundance Boxplots <em>Class</em> level.</strong></h3>
								"
							)
						)
					),
					fluidPage(
						column(3,
							a(id = "toggleclassRABUND_DataDownload", "Show/hide Differential Abundance Downloading Options", href = "#"),
							p(),
							shinyjs::hidden(
								div(id = "classRABUND_DataDownload",	
									textInput("classRABUND_Datafilename", label = "Choose File Name", value = "Differential Abundance Data"),
									tags$p(
										tags$a(id = "classRABUNDData_download", href = "#", class = "shiny-download-link", target="_blank",
											style="color: #fff; background-color: #2c2cad; border-color: #000; padding-top: 8px;
											padding-right: 8px; padding-bottom: 8px; padding-left: 8px; border-radius: 5px;",
											HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download Relative Abundance Data")
										)
									)	
								)
							)
						),
						column(3
						),
						column(3,
							HTML("
								<h4><strong>Select Bacteria:</strong></h4>
								"
							),			

							selectInput(inputId="dabundCBACTERIAselect", label="", choices=DABUNDclassOTUlist()$Selection, 
								selected=DABUNDclassOTUlist()$Choice)
						),
						column(3,
							radioButtons("RABUNDclassBPdatatype", label = "Select Data Type",
								choices = list("Total Reads" = "reads", "Percent Abundance" = "PA"), selected = "reads"
							),
							{
								if(is.null(RABUNDnumbcompRENDER())) {
									NULL
								} else {
									radioButtons("RABUNDclassbpgroup", label = "Show all Groups",
										choices = list("NO" = 1, "YES" = 2), selected = 1)
								}
							}
							
						)
					),
					fluidPage(
						column(6,				
							DT::dataTableOutput("DABUNDclassNBMtableRENDER"),
							uiOutput("DABUNDclasstesttabletextRENDER")

							
						),
						column(6,
							rbokehOutput("DABUNDclassboxplotRENDER", height="700px", width="1000px")
							
						)
					)
				)
			} else {
				return(NULL)
			}
		} else {
			NULL
		}								
	})		

	########################################################################
	## Render Order UI after goDABUND observed
	########################################################################	
	output$DABUNDorderBPoptionsRENDER <- renderUI({
		if(input$goDABUND){
			if("Order" %in% DABUNDtaxa()){	
				list(
					hr(class="hr_SINGLE_sep"),
					fluidPage(
						column(6,
							HTML("
								<h3><strong>Differential Abundance Results at <em>Order</em> level.</strong></h3>
								"
							)
						),
						column(6,
							HTML("
								<h3><strong>Differential Abundance Boxplots <em>Order</em> level.</strong></h3>
								"
							)
						)
					),
					fluidPage(
						column(3,
							a(id = "toggleorderRABUND_DataDownload", "Show/hide Differential Abundance Downloading Options", href = "#"),
							p(),
							shinyjs::hidden(
								div(id = "orderRABUND_DataDownload",	
									textInput("orderRABUND_Datafilename", label = "Choose File Name", value = "Differential Abundance Data"),
									tags$p(
										tags$a(id = "orderRABUNDData_download", href = "#", class = "shiny-download-link", target="_blank",
											style="color: #fff; background-color: #2c2cad; border-color: #000; padding-top: 8px;
											padding-right: 8px; padding-bottom: 8px; padding-left: 8px; border-radius: 5px;",
											HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download Relative Abundance Data")
										)
									)	
								)
							)
						),
						column(3
						),
						column(3,
							HTML("
								<h4><strong>Select Bacteria:</strong></h4>
								"
							),			

							selectInput(inputId="dabundOBACTERIAselect", label="", choices=DABUNDorderOTUlist()$Selection, 
								selected=DABUNDorderOTUlist()$Choice)
						),
						column(3,
							radioButtons("RABUNDorderBPdatatype", label = "Select Data Type",
								choices = list("Total Reads" = "reads", "Percent Abundance" = "PA"), selected = "reads"
							),
							{
								if(is.null(RABUNDnumbcompRENDER())) {
									NULL
								} else {
									radioButtons("RABUNDorderbpgroup", label = "Show all Groups",
										choices = list("NO" = 1, "YES" = 2), selected = 1)
								}
							}
							
						)
					),
					
					fluidPage(
						column(6,				
							DT::dataTableOutput("DABUNDorderNBMtableRENDER"),
							uiOutput("DABUNDordertesttabletextRENDER")

							
						),
						column(6,
							rbokehOutput("DABUNDorderboxplotRENDER", height="700px", width="1000px")
							
						)
					)
				)
			} else {
				return(NULL)
			}
		} else {
			NULL
		}								
	})		

	########################################################################
	## Render Family UI after goDABUND observed
	########################################################################	
	output$DABUNDfamilyBPoptionsRENDER <- renderUI({
		if(input$goDABUND){
			if("Family" %in% DABUNDtaxa()){	
				list(
					hr(class="hr_SINGLE_sep"),
					fluidPage(
						column(6,
							HTML("
								<h3><strong>Differential Abundance Results at <em>Family</em> level.</strong></h3>
								"
							)
						),
						column(6,
							HTML("
								<h3><strong>Differential Abundance Boxplots <em>Family</em> level.</strong></h3>
								"
							)
						)
					),
					fluidPage(
						column(3,
							a(id = "togglefamilyRABUND_DataDownload", "Show/hide Differential Abundance Downloading Options", href = "#"),
							p(),
							shinyjs::hidden(
								div(id = "familyRABUND_DataDownload",	
									textInput("familyRABUND_Datafilename", label = "Choose File Name", value = "Differential Abundance Data"),
									tags$p(
										tags$a(id = "familyRABUNDData_download", href = "#", class = "shiny-download-link", target="_blank",
											style="color: #fff; background-color: #2c2cad; border-color: #000; padding-top: 8px;
											padding-right: 8px; padding-bottom: 8px; padding-left: 8px; border-radius: 5px;",
											HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download Relative Abundance Data")
										)
									)	
								)
							)
						),
						column(3
						),
						column(3,
							HTML("
								<h4><strong>Select Bacteria:</strong></h4>
								"
							),			

							selectInput(inputId="dabundFBACTERIAselect", label="", choices=DABUNDfamilyOTUlist()$Selection, 
								selected=DABUNDfamilyOTUlist()$Choice)
						),
						column(3,
							radioButtons("RABUNDfamilyBPdatatype", label = "Select Data Type",
								choices = list("Total Reads" = "reads", "Percent Abundance" = "PA"), selected = "reads"
							),
							{
								if(is.null(RABUNDnumbcompRENDER())) {
									NULL
								} else {
									radioButtons("RABUNDfamilybpgroup", label = "Show all Groups",
										choices = list("NO" = 1, "YES" = 2), selected = 1)
								}
							}
							
						)
					),
					fluidPage(
						column(6,				
							DT::dataTableOutput("DABUNDfamilyNBMtableRENDER"),
							uiOutput("DABUNDfamilytesttabletextRENDER")

							
						),
						column(6,
							rbokehOutput("DABUNDfamilyboxplotRENDER", height="700px", width="1000px")
							
						)
					)
				)
			} else {
				return(NULL)
			}
		} else {
			NULL
		}								
	})		

	########################################################################
	## Render Genus UI after goDABUND observed
	########################################################################	
	output$DABUNDgenusBPoptionsRENDER <- renderUI({
		if(input$goDABUND){
			if("Genus" %in% DABUNDtaxa()){	
				list(
					hr(class="hr_SINGLE_sep"),
					fluidPage(
						column(6,
							HTML("
								<h3><strong>Differential Abundance Results at <em>Genus</em> level.</strong></h3>
								"
							)
						),
						column(6,
							HTML("
								<h3><strong>Differential Abundance Boxplots <em>Genus</em> level.</strong></h3>
								"
							)
						)
					),
					fluidPage(
						column(3,
							a(id = "togglegenusRABUND_DataDownload", "Show/hide Differential Abundance Downloading Options", href = "#"),
							p(),
							shinyjs::hidden(
								div(id = "genusRABUND_DataDownload",	
									textInput("genusRABUND_Datafilename", label = "Choose File Name", value = "Differential Abundance Data"),
									tags$p(
										tags$a(id = "genusRABUNDData_download", href = "#", class = "shiny-download-link", target="_blank",
											style="color: #fff; background-color: #2c2cad; border-color: #000; padding-top: 8px;
											padding-right: 8px; padding-bottom: 8px; padding-left: 8px; border-radius: 5px;",
											HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download Relative Abundance Data")
										)
									)	
								)
							)
						),
						column(3
						),
						column(3,
							HTML("
								<h4><strong>Select Bacteria:</strong></h4>
								"
							),			

							selectInput(inputId="dabundGBACTERIAselect", label="", choices=DABUNDgenusOTUlist()$Selection, 
								selected=DABUNDgenusOTUlist()$Choice)
						),
						column(3,
							radioButtons("RABUNDgenusBPdatatype", label = "Select Data Type",
								choices = list("Total Reads" = "reads", "Percent Abundance" = "PA"), selected = "reads"
							),
							{
								if(is.null(RABUNDnumbcompRENDER())) {
									NULL
								} else {
									radioButtons("RABUNDgenusbpgroup", label = "Show all Groups",
										choices = list("NO" = 1, "YES" = 2), selected = 1)
								}
							}
							
						)
					),
					fluidPage(
						column(6,		
							DT::dataTableOutput("DABUNDgenusNBMtableRENDER"),
							uiOutput("DABUNDgenustesttabletextRENDER")

							
						),
						column(6,
							rbokehOutput("DABUNDgenusboxplotRENDER", height="500px", width="1000px")
							
						)
					)
				)
			} else {
				return(NULL)
			}
		} else {
			NULL
		}								
	})		
	
	########################################################################
	## Render OTU UI after goDABUND observed
	########################################################################	
	output$DABUNDotuBPoptionsRENDER <- renderUI({
		if(input$goDABUND){
			if("OTU" %in% DABUNDtaxa()){	
				list(
					hr(class="hr_SINGLE_sep"),
					fluidPage(
						column(6,
							HTML("
								<h3><strong>Differential Abundance Results at <em>OTU</em> level.</strong></h3>
								"
							)
						),
						column(6,
							HTML("
								<h3><strong>Differential Abundance Boxplots <em>OTU</em> level.</strong></h3>
								"
							)
						)
					),
					fluidPage(
						column(3,
							a(id = "toggleotuRABUND_DataDownload", "Show/hide Differential Abundance Downloading Options", href = "#"),
							p(),
							shinyjs::hidden(
								div(id = "otuRABUND_DataDownload",	
									textInput("otuRABUND_Datafilename", label = "Choose File Name", value = "Differential Abundance Data"),
									tags$p(
										tags$a(id = "otuRABUNDData_download", href = "#", class = "shiny-download-link", target="_blank",
											style="color: #fff; background-color: #2c2cad; border-color: #000; padding-top: 8px;
											padding-right: 8px; padding-bottom: 8px; padding-left: 8px; border-radius: 5px;",
											HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download Relative Abundance Data")
										)
									)	
								)
							)
						),
						column(3
						),
						column(3,
							HTML("
								<h4><strong>Select Bacteria:</strong></h4>
								"
							),			

							selectInput(inputId="dabundOTUBACTERIAselect", label="", choices=DABUNDOTU_OTUlist()$Selection, 
								selected=DABUNDOTU_OTUlist()$Choice)
						),
						column(3,
							radioButtons("RABUNDotuBPdatatype", label = "Select Data Type",
								choices = list("Total Reads" = "reads", "Percent Abundance" = "PA"), selected = "reads"
							),
							{
								if(is.null(RABUNDnumbcompRENDER())) {
									NULL
								} else {
									radioButtons("RABUNDotubpgroup", label = "Show all Groups",
										choices = list("NO" = 1, "YES" = 2), selected = 1)
								}
							}
							
						)
					),
					fluidPage(
						column(6,				
							DT::dataTableOutput("DABUNDotuNBMtableRENDER"),
							uiOutput("DABUNDotutesttabletextRENDER")

							
							
						),
						column(6,
							rbokehOutput("DABUNDotuboxplotRENDER", height="700px", width="1000px")
							
						)
					)
				)
			} else {
				return(NULL)
			}
		} else {
			NULL
		}								
	})		
		
	