
	########################################################################	
	## disable alpha-diversity tab on page load
	########################################################################
    js$disableTab("Alpha-Diversity")

	########################################################################		
	## Open alpha-diversity tab when input$goIMPORT is observed
	########################################################################	
    observeEvent(input$goIMPORT, {
		# enable Alpha-Diversity when clicking the button
		js$enableTab("Alpha-Diversity")
		# switch to Alpha-Diversity
		updateTabsetPanel(session, "adivpage", "Alpha-Diversity")
    })

	######################################################################
	## Toggle regular/larger text 
	########################################################################
	observe({
		shinyjs::toggleClass("text_adiv", "big", input$aDIVbig)
    })

	########################################################################	
	## Render Taxa Selections Experimental Groups options
	########################################################################				
	output$ADIVtaxaselectRENDER <- renderUI({
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
				selectizeInput("adivTAXAselect", label="", selected="Phylum",
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
				selectizeInput("adivTAXAselect", label="", selected="Phylum",
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
	## Render ANOVA Experimental Groups options
	########################################################################				
	output$ADIVgroupselectRENDER <- renderUI({
		req(phyloseqFINAL())	
		list(
			HTML("
				<h4><strong>Select Group(s):</strong></h4>
				"
			),	
			selectizeInput("adivGROUPselect", label="", selected=CompGroups()$Groups[1],
				choices=CompGroups()$Groups, options = list(minItems = 1), 
					multiple=TRUE),
			HTML("
				<p>Reactivity of Statistical Test widget is dependant on number of levels in group or number of groups selected</p>
				"
			)
		)
	})

	########################################################################	
	## Render ANOVA options if Experimental Groups > 1
	########################################################################	
	output$ADIVtesttypeRENDER <- renderUI({
		# req(phyloseqFINAL())	
		if(length(input$adivGROUPselect) == 1) {
			if(length(CompGroups()[[2]][names(CompGroups()[[2]]) %in% input$adivGROUPselect][[1]]) == 2) {
				list(
					HTML("
						<h4><strong>Select Type of Test:</strong></h4>
						"
					),	
					selectInput(inputId="adivTESTtype", label = "", 
						choices = list("t-test" = "tt", "Mann Whitney U" = "mwu"), selected = "tt"),
					HTML("
						<p>Only 1 group selected with 2 levels. Options include t-test (parametric) and Mann Whitney U test (nonparametric). 
						Selection of t-test will provide option to view distribution of samples in Q-Q plot. Selection of Mann Whitney U test will
						only provide boxplots for data visualization.</p>
						"
					)
				)
			} else {
				list(
					HTML("
						<h4><strong>Select Type of Test:</strong></h4>
						"
					),	
					selectInput(inputId="adivTESTtype", label = "", 
						choices = list("1-way ANOVA" = "anova", "Kruskal Wallis" = "kw"), selected = "anova"),
					HTML("
						<p>Only 1 group selected with > 2 levels. Options include 1-way ANOVA (parametric) and Kruskal Wallis test (nonparametric). 
						Selection of 1-way ANOVA will provide option to view distribution of ANOVA residuals in Q-Q plot (check normality assumption)
						as well as a Fitted vs Residual plots (check homogeneity of variance assumption).  Selection of Kruskal Wallis test will only 
						provide boxplots for data visualization.</p>
						"
					)
				)
			}
		} else {
			list(
				HTML("
					<h4><strong>Select Type of Test:</strong></h4>
					"
				),	
				selectInput(inputId="adivTESTtype", label = "", 
					choices = list("1-way ANOVA" = "anova", "Multi-factor ANOVA" = "MFanova", "Kruskal Wallis" = "kw"), selected = "anova"),
					HTML("
						<p>Multiple groups selected. Options include 1-way ANOVA and Multi-factor ANOVA (both parametric) and Kruskal Wallis test (nonparametric). 
						Selection of either ANOVA tests will provide option to view distribution of ANOVA residuals in Q-Q plot (check normality assumption)
						as well as a Fitted vs Residual plots (check homogeneity of variance assumption).  Selection of Kruskal Wallis test will only 
						provide boxplots for data visualization.</p>
						"
					)
			)
		}
	})
		
	########################################################################	
	## Render graphic options
	########################################################################	
	output$ADIVadvanceplotRENDER <- renderUI({
		req(phyloseqFINAL())	
		if(input$goADIV){
			isolate({
				if(input$adivTESTtype %in% c("MFanova", "anova")) {
					list(
						
						hr(),
						fluidPage(
							column(12, 
								HTML("
									 <h4><strong><em>Advanced Options for Graphics.</em></strong></h4>
									"
								)
							)
						),
						fluidPage(
							column(3,
								
								a(id = "toggleADIV_HC_Advanced", "Show/hide plotting options", href = "#"),
								p(),
								shinyjs::hidden(
									div(id = "ADIV_HCadvanced",	
										HTML("
											 <h4><strong>Select Plot:</strong></h4>
											 "
										),	
										selectInput(inputId="adivPLOTtype", label = "", selected = "box",
											choices = c("Boxplot" = "box", "Barplot" = "bar", "qqplot" = "qq", "Fitted vs Residuals" = "fvsr")
										)									
									)
								)						
							)
						)
					)
				} else {
					if(input$adivTESTtype %in% c("tt")) {
						list(
							hr(),
							fluidPage(
								column(12, 
									HTML("
										 <h4><strong><em>Advanced Options for Graphics.</em></strong></h4>
										"
									)
								)
							),
							fluidPage(
								column(3,
									
									a(id = "toggleADIV_HC_Advanced", "Show/hide advanced plotting options", href = "#"),
									p(),
									shinyjs::hidden(
										div(id = "ADIV_HCadvanced",	
											HTML("
												 <h4><strong>Select Plot:</strong></h4>
												 "
											),	
											selectInput(inputId="adivPLOTtype", label = "", selected = "box",, 
												choices = c("Boxplot" = "box", "Barplot" = "bar", "qqplot" = "qq")
											)									
										)
									)						
								)
							)
						)
					} else {
						NULL
					}
				}
			})
		} else {
			NULL
		}		
	})

	########################################################################	
	## Render UI for alpha diversity download options
	########################################################################		
	output$ADIVdownloadRENDER <- renderUI({
		req(phyloseqFINAL())	
		if(input$goADIV){
			list(
					hr(),
					fluidPage(
						column(12, 
							HTML("
								 <h4><strong><em>Download &#945;-Diversity data.</em></strong></h4>
								"
							)
						)
					),
					fluidPage(
						column(3,

							a(id = "toggleADIV_DataDownload", HTML("Show/hide &#945;-Diversity Data Downloading Options"), href = "#"),
							p(),
							shinyjs::hidden(
								div(id = "ADIV_DataDownload",	
									textInput("ADIV_DataDownloadfilename", label = "Choose File Name", value = "Alpha-Diversity Data"),
									tags$p(
										tags$a(id = "ADIVData_download", href = "#", class = "shiny-download-link", target="_blank",
											style="color: #fff; background-color: #2c2cad; border-color: #000; padding-top: 8px;
											padding-right: 8px; padding-bottom: 8px; padding-left: 8px; border-radius: 5px;",
											HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download &#945;-Diversity  Data")
										)
									)	
								)
							)
						)
					)
			)
		} else {
			NULL
		}			
	})
	
	########################################################################	
	## Isolate TAXA selections 
	########################################################################	
	adivTAXA <- reactive({
		if(input$goADIV){
			isolate({
				input$adivTAXAselect
			})
		} else {
			NULL
		}
	})
	
	########################################################################	
	## Isolate alpha diveristy selections 
	########################################################################			
	adivADIV <- reactive({
		if(input$goADIV){
			isolate({
				input$adivADIVselect
			})
		} else {
			NULL
		}
	})
	
	########################################################################	
	## Isolate ANOVA type selection
	########################################################################				
	adivTEST <- reactive({
		if(input$goADIV){
			isolate({
				input$adivTESTtype
			})
		} else {
			NULL
		}
	})

	########################################################################	
	## Isolate group selections 
	########################################################################						
	adivGROUP <- reactive({
		if(input$goADIV){
			isolate({
				colnames(sample_data(phyloseqFINAL()))[colnames(sample_data(phyloseqFINAL())) %in% input$adivGROUPselect]
			})
		} else {
			NULL
		}
	})

	########################################################################
	## Agglomerate reads according to TAXA selections
	########################################################################	
	pyloseqTAXA <- reactive({
		if(input$goADIV){
			isolate({
				adivTAXA <- adivTAXA()
				## OTU does not require to be agglomerated 
				## Needs to be handled by itself if selected
				if("OTU" %in% adivTAXA){
					## If only OTU is selected then move object into a list, name list element, and return
					if(setequal(adivTAXA == "OTU", TRUE)) {
						phylo_TAX_LIST <- list("OTU" = phyloseqFINAL())
						names(phylo_TAX_LIST) <- adivTAXA()
						phylo_TAX_LIST
					} else {
						## Run phyloseq::tax_glom() function on selected TAXA
						## using pblapply() to render status boxes
						adivTAXA <- adivTAXA[!(adivTAXA %in% "OTU")]
						withProgress(message = "Agglomerating Taxa", value=0, { 
							percentage <- 0 
							phylo_TAX_LIST <- pblapply(adivTAXA, function(x) {
								Sys.sleep(0.05); 
								percentage <<- percentage + 1/length(adivTAXA())*100
								incProgress(1/length(adivTAXA()), detail = paste0("Progress: ", round(percentage,2),"%"))
								tax_glom(phyloseqFINAL(), x)
							})
						})
						phylo_TAX_LIST[[length(phylo_TAX_LIST) + 1]] <- phyloseqFINAL()
						names(phylo_TAX_LIST) <- adivTAXA()
						phylo_TAX_LIST
					}
				} else {
				## If OTU is NOT selected then run phyloseq::tax_glom() function on selected TAXA
				## using pblapply() to render status boxes
					adivTAXA <- adivTAXA
					withProgress(message = "Agglomerating Taxa", value=0, { 
						percentage <- 0 
						phylo_TAX_LIST <- pblapply(adivTAXA, function(x) {
							Sys.sleep(0.05); 
							percentage <<- percentage + 1/length(adivTAXA())*100
							incProgress(1/length(adivTAXA()), detail = paste0("Progress: ", round(percentage,2),"%"))
							tax_glom(phyloseqFINAL(), x)
						})						
					})
					## Set list element names and return
					names(phylo_TAX_LIST) <- adivTAXA()
					phylo_TAX_LIST
				}
			})
		} else {
			NULL
		}
	})

	########################################################################
	## Calculate alpha diversity
	########################################################################		
	phylo_TAX_aDIV <- reactive({
		req(pyloseqTAXA())
				withProgress(message = "Calculating a-Diversity", value=0, { 
					percentage <- 0 
					phylo_TAX_aDIV <- pblapply(adivTAXA(), function(x) {
						Sys.sleep(0.05); 
						percentage <<- percentage + 1/length(adivTAXA())*100
						incProgress(1/length(adivTAXA()), detail = paste0("Progress: ", round(percentage,2),"%"))
						## Calculate alpha diversity with phyloseq::estimate_richness function and
						## add SAMPLE data to results
						ER <- estimate_richness(pyloseqTAXA()[[x]], measures=adivADIV())
						ERDf <- data.frame(sample_data(phyloseqFINAL()), ER)
						ERDf
					})
				})
				## Set names and return
				names(phylo_TAX_aDIV) <- adivTAXA()
				phylo_TAX_aDIV

	})

	########################################################################
	## Calculate ANOVA results
	########################################################################	
	phylo_TAX_aDIVgroupTEST <- reactive({	
		req(phylo_TAX_aDIV())		
				withProgress(message = "Running ANOVA(s)", value=0, { 				
					percentage8 <- 0 
					phylo_TAX_aDIVaovTEST <- pblapply(adivTAXA(), function(x) {
						Sys.sleep(0.05); 
						percentage8 <<- percentage8 + 1/length(adivTAXA())*100
						incProgress(1/length(adivTAXA()), detail = paste0("Progress: ", round(percentage8,2),"%"))
						## Select alpha diversity return by TAXA
						ERDf <- phylo_TAX_aDIV()[[x]]
						## if ANOVA type selection was not rendered
						## ie, only one experimental group selected
							
						## Multifactor ANOVA
						if(adivTEST() == "MFanova"){							
							## Melt data based on Experimental group and alpha diversity measurements and
							## run ANOVA
							ERDfmelt <- as_tibble(melt(ERDf[,c(adivGROUP(),adivADIV())]))
							testRES <- ERDfmelt %>% group_by(variable) %>% 
								do(testRES = aov(formula(paste("value ~ ", paste(adivGROUP(), collapse=" * "))), data=.))
						}
						
						## 1-way ANOVA
						if(adivTEST() == "anova"){	
							if(length(adivGROUP()) == 1) {
								ERDfmelt <- as_tibble(melt(ERDf[,c(adivGROUP(),adivADIV())]))
								testRES <- ERDfmelt %>% group_by(variable) %>% 
									do(testRES = aov(formula(paste0("value ~ ", adivGROUP())), data=.))
							} else {
								# Concatenate Experimental groups
								ERDf$P <- factor(apply(ERDf[, which(colnames(ERDf) %in% adivGROUP())], 1, paste, collapse="_"))
								phyDATmelt <- as_tibble(melt(ERDf))
								testRES <- phyDATmelt %>% group_by(variable) %>% 
									do(testRES = aov(formula("value ~ P"), data=.))								
							}
						}
						
						## Kruskal Wallis Test
						if(adivTEST() == "kw"){	
							if(length(adivGROUP()) == 1) {
								ERDfmelt <- as_tibble(melt(ERDf[,c(adivGROUP(),adivADIV())]))
								testRES <- ERDfmelt %>% group_by(variable) %>% 
									do(testRES = kruskal.test(formula(paste0("value ~ ", adivGROUP())), data=.))
							} else {
								# Concatenate Experimental groups
								ERDf$P <- factor(apply(ERDf[, which(colnames(ERDf) %in% adivGROUP())], 1, paste, collapse="_"))
								phyDATmelt <- as_tibble(melt(ERDf))
								testRES <- phyDATmelt %>% group_by(variable) %>% 
									do(testRES = kruskal.test(formula("value ~ P"), data=.))								
							}							
						}
						
						## t test
						if(adivTEST() == "tt"){								
							ERDfmelt <- as_tibble(melt(ERDf[,c(adivGROUP(),adivADIV())]))
							testRES <- ERDfmelt %>% group_by(variable) %>% 
								do(testRES = t.test(formula(paste0("value ~ ", adivGROUP())), data=.))
								}
						
						## Mann Whitney U test
						if(adivTEST() == "mwu"){								
							ERDfmelt <- as_tibble(melt(ERDf[,c(adivGROUP(),adivADIV())]))
							testRES <- ERDfmelt %>% group_by(variable) %>% 
								do(testRES = wilcox.test(formula(paste0("value ~ ", adivGROUP())), data=.))
								
						} 
							
						
						
						testRES
					})
				})
				## set names and return
				names(phylo_TAX_aDIVaovTEST) <- adivTAXA()	
				phylo_TAX_aDIVaovTEST
	})			

	########################################################################
	## Calculate ANOVA results for data download
	########################################################################		
	phylo_TAX_aDIVtestDOWN <- reactive({
		req(pyloseqTAXA)			
		req(phylo_TAX_aDIVgroupTEST())	
		withProgress(message = "Running ANOVA(s)", value=0, { 				
			percentage2 <- 0 
			phylo_TAX_aDIVgroupDOWN <- pblapply(adivTAXA(), function(x) {
				Sys.sleep(0.05); 
				percentage2 <<- percentage2 + 1/length(adivTAXA())*100
				incProgress(1/length(adivTAXA()), detail = paste0("Progress: ", round(percentage2,2),"%"))
				## Select ANOVA results by TAXA						
				groupTEST <- phylo_TAX_aDIVgroupTEST()[[x]]
				# Extract result table amd set groups
				
				## ANOVAs
				if(adivTEST() %in% c("anova", "MFanova")){						
					groupTESTL <- apply(groupTEST, 1, function(x) summary(x[[2]]))
					# names(groupTESTL) <- as.character(groupTEST$variable)
					names(groupTESTL) <- groupTEST$variable
					if(length(adivGROUP()) == 1) {
						## format 1-way anova results and remove Residuals row.
						aovpvec <- sapply(adivADIV(), function(x) {
							datf <- groupTESTL[[x]][[1]]
							pval <- round(datf[!(grepl("Residuals", rownames(datf))),"Pr(>F)"],5)
						})
						## Combine group names and pvalues
						test_p_df <- data.frame(variable=names(aovpvec), P=round(aovpvec, 5))
						rownames(test_p_df) <- test_p_df$variable
					} else {
					## If > 1 groups are selected
						aovpdftall <- do.call("rbind", 
							## Format data, remove residuals, combine group names, pvalue
							lapply(adivADIV(), function(x) {
								datf <- groupTESTL[[x]][[1]]					
								pval <- round(datf[!(grepl("Residuals", rownames(datf))),"Pr(>F)"],5)
								test_p_df <- data.frame(adiv=rownames(datf)[!(grepl("Residuals", rownames(datf)))], P=round(pval, 5), variable=rep(x, length(pval)))
							})
						)
						## reformat to wide format and remove excess spaces in strings
						test_p_df <- dcast(aovpdftall, variable ~ adiv, value.var="P")
						colnames(test_p_df) <- gsub(" ", "", colnames(test_p_df))
					}
				}	
				## ANOVAs
				if(adivTEST() %in% c("kw", "tt", "mwu")){	
					list(apply(groupTEST, 1, function(x) x[[2]]$p.value), apply(groupTEST, 1, function(x) x[[1]]))
					test_p_df <- data.frame(variable=apply(groupTEST, 1, function(x) x[[1]]), P=round(apply(groupTEST, 1, function(x) x[[2]]$p.value), 5))
					
					rownames(test_p_df) <- as.character(test_p_df$variable)
				} 
				test_p_df	
			})	
		})
		## Set names and return
		names(phylo_TAX_aDIVgroupDOWN) <- adivTAXA()	
		phylo_TAX_aDIVgroupDOWN
	})
	
	########################################################################
	## Set dispersion data frame
	########################################################################	
	phylo_TAX_aDIV_DISPERSION <- reactive({
		req(pyloseqTAXA)
		req(phylo_TAX_aDIV())			
		withProgress(message = "Setting Plot Data", value=0, { 	
			percentage3 <- 0 
			phylo_TAX_aDIV_DISPERSION <- pblapply(adivTAXA(), function(x) {
				Sys.sleep(0.05); 
				percentage3 <<- percentage3 + 1/length(adivTAXA())*100
				incProgress(1/length(adivTAXA()), detail = paste0("Progress: ", round(percentage3,2),"%"))
				## Select alpha diversity return by TAXA						
				ERDf <- phylo_TAX_aDIV()[[x]]
				## Combine group and alpha diversity data and reformat to tall format
				ERDfmelt <- as_tibble(melt(ERDf[,c(adivGROUP(),adivADIV())]))
				## Group by Experimental Group and alpha diversity measurement
				## and calculate MEAN, SD, and SEM
				dots <- lapply(c("variable", adivGROUP()), as.symbol)
				if(adivTEST() %in% c("anova", "MFanova", "tt")) {
					DISPERSIONtall <- as.data.frame(ERDfmelt %>% group_by_(.dots=dots) %>% 
						summarise(MEAN=mean(value, na.rm=TRUE), SEM=sd(value, na.rm=TRUE)/sqrt(length(value))))
					## Reformat to tall foramt, change 2nd 'variable' column to 'stat', round to the 1st decimal, and return
				} else {
					DISPERSIONtall <- as.data.frame(ERDfmelt %>% group_by_(.dots=dots) %>% 
						summarise(MEDIAN=median(value, na.rm=TRUE), Q25=quantile(value, na.rm=TRUE)["25%"], Q75=quantile(value, na.rm=TRUE)["75%"]))
				}
				DISPERSION <- melt(DISPERSIONtall)
				colnames(DISPERSION)[which(colnames(DISPERSION) %in% "variable")[2]] <- "stat"
				DISPERSION$value <- round(DISPERSION$value,3)
					
				DISPERSION
			})
		})
		## set names and return
		names(phylo_TAX_aDIV_DISPERSION) <- adivTAXA()	
		phylo_TAX_aDIV_DISPERSION
	})			


	########################################################################
	## Set plotting data for alpha diversity - barplots
	########################################################################		
	phylo_TAX_aDIV_barplotDAT <- reactive({
		req(pyloseqTAXA)		
		req(phylo_TAX_aDIV_DISPERSION())
		withProgress(message = "Setting Plot Data", value=0, { 	
			percentage4 <- 0 	
			phylo_TAX_aDIV_plotDAT <- pblapply(adivTAXA(), function(x) {
				Sys.sleep(0.05); 
				percentage4 <<- percentage4 + 1/length(adivTAXA())*100
				incProgress(1/length(adivTAXA()), detail = paste0("Progress: ", round(percentage4,2),"%"))
				## Select alpha diversity mean/sd/sem return by TAXA						
				DISPERSIONtallmelt <- phylo_TAX_aDIV_DISPERSION()[[x]]
				
				## subset mean & SEM
				DISPtallmelt <- DISPERSIONtallmelt[DISPERSIONtallmelt$stat %in% c("MEAN", "SEM"),]
				
				## Reformat to wide data based on how many experimental groups selected	
				if(length(adivGROUP()) ==1) {
					DISPwide <- dcast(DISPtallmelt, paste("variable + stat ~ ", adivGROUP()))
				} else {
					DISPwide <- dcast(DISPtallmelt, paste("variable + stat ~ ", paste(adivGROUP(), collapse=" + ")))

				}
				colnames(DISPwide)[colnames(DISPwide) %in% "variable"] <- "ADIV"
				DISPwide
			})
		})
		## Set names and return
		names(phylo_TAX_aDIV_plotDAT) <- adivTAXA()
		phylo_TAX_aDIV_plotDAT
	})			

	########################################################################
	## Set alpha diversity data for download
	########################################################################	
	phylo_TAX_aDIV_DATADOWN <- reactive({
		req(pyloseqTAXA)
		req(phylo_TAX_aDIV())		
		## Extract data and bind into a single object
		do.call("rbind",
			lapply(adivTAXA(), function(x) {
				## Select alpha diversity data return by TAXA						
				dat <- phylo_TAX_aDIV()[[x]]
				## Set column with TAXA labels
				dat$TAXA <- rep(x, nrow(dat))
				dat
			})
		)	
	})		

	########################################################################
	## Set data needed for normality plots
	########################################################################		
	phylo_TAX_aDIV_normDATA <- reactive({	
		req(pyloseqTAXA)
		req(phylo_TAX_aDIV())	
		req(phylo_TAX_aDIVgroupTEST())	
		withProgress(message = "Running ANOVA(s)", value=0, { 				
			percentage8 <- 0 
			phylo_TAX_aDIVnormDATA <- pblapply(adivTAXA(), function(x) {
				Sys.sleep(0.05); 
				percentage8 <<- percentage8 + 1/length(adivTAXA())*100
				incProgress(1/length(adivTAXA()), detail = paste0("Progress: ", round(percentage8,2),"%"))
				## Select alpha diversity return by TAXA
				ERDf <- phylo_TAX_aDIV()[[x]]	
				groupTEST <- phylo_TAX_aDIVgroupTEST()[[x]]
				# Extract result table amd set groups
				groupTEST
				## ANOVAs
				if(adivTEST() %in% c("anova", "MFanova")){	
					aovres <- apply(groupTEST, 1, function(x) x[[2]])
					names(aovres) <- groupTEST$variable
					returnDF <- do.call(rbind, lapply(adivADIV(), function(y) {
						ADIVaovres <- aovres[[y]]
						ADIVaovDF <- data.frame(ADIV = y, ADIVaovres$model, Fitted=fitted(ADIVaovres), Residuals=residuals(ADIVaovres),
							QQx = qqnorm(rstandard(ADIVaovres), plot.it=FALSE)$x, QQy = qqnorm(rstandard(ADIVaovres), plot.it=FALSE)$y,
							SResids = rstandard(ADIVaovres))
						colnames(ADIVaovDF) <- gsub("X1.", "", colnames(ADIVaovDF))
						ADIVaovDF
					}))
				} 
				## t-test
				if(adivTEST() %in% "tt"){	
					meltERDF <- melt(ERDf)
					returnDF <- do.call(rbind, lapply(adivADIV(), function(y) {
						dat <- meltERDF[meltERDF$variable %in% y,]
						normdat <- data.frame(ADIV = y, dat, QQx = qqnorm(dat$value, plot.it=FALSE)$x, QQy = qqnorm(dat$value, plot.it=FALSE)$y)
						normdat
					}))				
				} 
				if(adivTEST() %in% c("mwu", "kw")){	
						
					returnDF <- NA
				}
				returnDF
			})
		})	
		## set names and return
		names(phylo_TAX_aDIVnormDATA) <- adivTAXA()	
		phylo_TAX_aDIVnormDATA		
	})

	# output$adivTEXT <- renderPrint({
		
	# })	
		
	########################################################################
	## Set ANOVA data for DataTable
	########################################################################						
	phylo_TAX_aDIV_testDAT <- reactive({
		req(pyloseqTAXA)
		req(phylo_TAX_aDIVtestDOWN())
		req(phylo_TAX_aDIV_DISPERSION())		
		withProgress(message = "Merging Data", value=0, { 	
			percentage7 <- 0 	
			phylo_TAX_aDIV_testDAT <- pblapply(adivTAXA(), function(x) {
				Sys.sleep(0.05); 
				percentage7 <<- percentage7 + 1/length(adivTAXA())*100
				incProgress(1/length(adivTAXA()), detail = paste0("Progress: ", round(percentage7,2),"%"))
				## Select alpha diversity mean/sd/sem return by TAXA
				DISPERSIONtallmelt <- phylo_TAX_aDIV_DISPERSION()[[x]]
				## If no selection for sd/sem
				if(adivTEST() %in% c("anova", "MFanova", "tt")) {	
					## Return data with SEM
					DISPtallmelt <- DISPERSIONtallmelt[DISPERSIONtallmelt$stat %in% c("MEAN","SEM"),]
				} else {
					## Return data with selection
					DISPtallmelt <- DISPERSIONtallmelt[DISPERSIONtallmelt$stat %in% c("MEDIAN","Q25","Q75"),]
				}
				## formula for data transformation is different 
				## Based on number of experimental groups selected
				if(length(adivGROUP()) ==1) {
					DISPwide <- dcast(DISPtallmelt, paste("variable + stat ~ ", adivGROUP()))
				} else {
					DISPwide <- dcast(DISPtallmelt, paste("variable + stat ~ ", paste(adivGROUP(), collapse=" + ")))
				}
				## melt results, change 2nd 'variable' column to 'group' column, and format back to wide format
				DISPwidemelt <- melt(DISPwide)
				colnames(DISPwidemelt)[which(colnames(DISPwidemelt) %in% "variable")[2]] <- "Group"
				DispW <- dcast(DISPwidemelt, variable ~ Group + stat)
				DispW
				
				## Extract levels from selected experimental groups
				compg2 <- CompGroups()$Levels[which(names(CompGroups()$Levels) %in% adivGROUP())]
				## if there are more than 1 groups selected
				if(length(compg2) > 1) {
					## Start vector with levels in first group
					## Then add the others into a the vector and concatenate levels with an underscore
					yvars <- compg2[[1]]						
					for(i in 2:length(compg2)) yvars <- as.vector(outer(yvars, compg2[[i]], paste, sep="_"))
				} else {
					yvars <- compg2[[1]]
				}
				## coerce mean/dispersion DF into list 
				DispW2 <- as.list(DispW)
				for(i in (1:length(yvars)) + ncol(DispW)) {
					## Add list elements to the end of the new list 		
					DispW2[[i]] <- rep(NA, nrow(DispW))
					DispW2
				}
				## If spaces in factor levels
				if(grepl(" ", yvars)){
					## Remove spaces from list names
					names(DispW2) <- gsub(" ", "", names(DispW2))
					## Remove spaces from factor name vector
					yvarsnopace <- gsub(" ", "", yvars)
				} else {
					yvarsnopace <- yvars
				}
								
				## Set new list elements with experimental group names
				names(DispW2)[(1:length(yvars)) + ncol(DispW)] <- yvarsnopace
				## Coerce list back into a data frame
				DispFINAL <- as.data.frame(DispW2)
			
				if(setequal(yvarsnopace %in% colnames(DispFINAL), TRUE)){
					colnameKEY <- data.frame(Original = yvarsnopace, Adjust = yvarsnopace, yvars=yvars, stringsAsFactors = FALSE)
				} else {	
					if(adivTEST() %in% c("anova", "MFanova", "tt")) {	
						sFIN <- grep("SEM", colnames(DispFINAL))
						Adjnames <- colnames(DispFINAL)[(sFIN[length(sFIN)]+1):ncol(DispFINAL)]
						colnameKEY <- data.frame(Original = yvarsnopace, Adjust = Adjnames, yvars=yvars, stringsAsFactors = FALSE)
					} else {
						sFIN <- grep("75", colnames(DispFINAL))
						Adjnames <- colnames(DispFINAL)[(sFIN[length(sFIN)]+1):ncol(DispFINAL)]					
						colnameKEY <- data.frame(Original = yvarsnopace, Adjust = Adjnames, yvars=yvars, stringsAsFactors = FALSE)
					}
				}
						
				## For each experimental group
				if(adivTEST() %in% c("anova", "MFanova", "tt")) {	
					for(i in colnameKEY$Adjust) {
						## Determine the data frame column positions needed to extract mean and dispersion data
						## Based on groups
						colsLOGIC <- captureCN(colnames(DispFINAL), i)
						## Remove data
						yvDF <- DispFINAL[,colsLOGIC]
						## make object with mean data
						yvMEAN <- yvDF[,1]
						## make object with dispersion data
						yvSEM <- yvDF[,2]
						## paste mean and dispersion data so presented as MEAN (SEM)
						yvFinal <- paste0(yvMEAN, " (", yvSEM, ")")
						## Deposit results in new Experimental Group column
						DispFINAL[,colsLOGIC][,3] <- yvFinal
					}
				} else {
					for(i in colnameKEY$Adjust) {
						## Determine the data frame column positions needed to extract mean and dispersion data
						## Based on groups
						colsLOGIC <- captureCN(colnames(DispFINAL), i)
						## Remove data
						yvDF <- DispFINAL[,colsLOGIC]
						## make object with median data
						yvMEDIAN <- yvDF[,1]
						## make object with Q25 data
						yvQ25 <- yvDF[,2]	
						## make object with Q75 data
						yvQ75 <- yvDF[,3]	
						## paste mean and dispersion data so presented as MEAN (SEM)
						yvFinal <- paste0(yvMEDIAN, " (", yvQ25, ", ", yvQ75, ")")						
						## Deposit results in new Experimental Group column
						DispFINAL[,colsLOGIC][,4] <- yvFinal					
					}
				}
				## Subset variable and experimental group columns presented as 'MEAN (SEM)'
				## Leave MEAN and SEM data behind...
				MF <- DispFINAL[,c("variable", colnameKEY$Adjust)]
				## If spaces or special characters in factor levels
				colnames(MF)[colnames(MF) %in% colnameKEY$Adjust] <- colnameKEY$yvars
				MF

				# merge mean/sem data with anova return by TAXA
				MFtest <- merge(MF, phylo_TAX_aDIVtestDOWN()[[x]], by="variable")
				MFtest			
			
			})
		})	
		## set names and return
		names(phylo_TAX_aDIV_testDAT) <- adivTAXA()
		phylo_TAX_aDIV_testDAT		
	})			

	########################################################################
	## hide/show advanced highcharter options
	########################################################################		
    shinyjs::onclick("toggleADIV_RAMC_Advanced",
        shinyjs::toggle(id = "ADIV_RAMCadvanced", anim = TRUE)
	)  	
	
	########################################################################
	## hide/show click option for advanced highcharter options
	########################################################################					
    shinyjs::onclick("toggleADIV_HC_Advanced",
        shinyjs::toggle(id = "ADIV_HCadvanced", anim = TRUE)
	)  	

	########################################################################		
	## Create rbokeh object with Phylum barplot data
	########################################################################	
	output$PHYLUMadivRbokeh_RENDER <- renderRbokeh({
		if(input$goADIV){
			if("Phylum" %in% adivTAXA()){	
				phylumDFmelt <- melt(phylumDF <- phylo_TAX_aDIV()[[which(names(phylo_TAX_aDIV()) %in% "Phylum")]])
				if(length(adivGROUP()) == 1) {				
					phylumDFmelt$Level <- factor(phylumDFmelt[,adivGROUP()])
				} else {
					phylumDFmelt$Level <- factor(apply(phylumDFmelt[,adivGROUP()], 1, paste, collapse = "_"))
				}
				
				normDAT <- phylo_TAX_aDIV_normDATA()[[which(names(phylo_TAX_aDIV_normDATA()) %in% "Phylum")]]	
				if(adivTEST() %in% c("anova", "MFanova")) {
					if(input$adivPLOTtype %in% "box") {
						plotfigs <- lapply(sort(adivADIV()), function(x) {
							dat <- phylumDFmelt[phylumDFmelt$variable %in% x,]   
							figure(data = dat, legend_location=NULL) %>%
								ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="Phylum Alpha Diversity") %>%
								ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
									set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
						}) 
					} else {
						if(input$adivPLOTtype %in% "bar") {
							bpDAT <- phylo_TAX_aDIV_barplotDAT()[[which(names(phylo_TAX_aDIV_barplotDAT()) %in% "Phylum")]]
							plotfigs <- lapply(sort(adivADIV()), function(x) {
								bpdat <- melt(bpDAT[bpDAT$ADIV %in% x,])
								bpMEANdat <- bpdat[bpdat$stat %in% "MEAN",]
								bpSEMdat <- bpdat[bpdat$stat %in% "SEM",]
								xpos <- as.character(bpMEANdat$variable)
								vtop <- bpMEANdat$value + bpSEMdat$value
								vbot <- bpMEANdat$value - bpSEMdat$value	
								xposhor1 <- paste0(as.character(bpMEANdat$variable), ":0.3")
								xposhor2 <- paste0(as.character(bpMEANdat$variable), ":0.7")							
								figure(data=bpMEANdat, legend_location=NULL) %>% ly_bar(variable, value, color=variable, hover = TRUE) %>%
									set_palette(discrete_color = pal_color(c("#c41e3a"))) %>% theme_axis("x", major_label_orientation = 45) %>%
									ly_segments(x0 = xpos, y0 = vtop, x1 = xpos, y1 = vbot, width = 1.5, color="#696969") %>%
									ly_segments(x0 = rep(xposhor1, 2), y0 = c(vtop,vbot), x1 = rep(xposhor2,2), y1 = c(vtop,vbot), width = 1.5, color="#696969")
							})
						} else {
							if(input$adivPLOTtype %in% "qq") {
								plotfigs <- lapply(sort(adivADIV()), function(x) {
									normdat <- normDAT[normDAT$ADIV %in% x,]
									qqlineres <- qqlineRESULTS(normdat$SResids)
									figure(data = normdat, xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>% 
										ly_points(QQx, QQy, color="#c41e3a", hover = list(QQx, QQy),xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>%
										ly_abline(qqlineres$intercept, qqlineres$slope, color="#696969")
								})
							} else {
								if(input$adivPLOTtype %in% "fvsr") {
									plotfigs <- lapply(sort(adivADIV()), function(x) {
										normdat <- normDAT[normDAT$ADIV %in% x,]
										figure(data = normdat, title = "Residuals vs Fitted") %>%
										ly_points(Fitted, Residuals,color="#c41e3a", xlab = "Fitted Values", ylab = "Residuals",
											hover = c("Fitted" = Fitted, "Residuals" = Residuals, "Alpha Diversity"= ADIV)) %>%
											ly_abline(h=0, type=2, color="black", alpha=0.5)
									})
								}
							}
						}
					}
				} else {
					if(adivTEST() %in% c("tt")) {
						if(input$adivPLOTtype %in% "box") {
							plotfigs <- lapply(sort(adivADIV()), function(x) {
								dat <- phylumDFmelt[phylumDFmelt$variable %in% x,]   
								figure(data = dat, legend_location=NULL) %>%
									ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="Phylum Alpha Diversity") %>%
									ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
										set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
							}) 
						} else {
							if(input$adivPLOTtype %in% "bar") {
								bpDAT <- phylo_TAX_aDIV_barplotDAT()[[which(names(phylo_TAX_aDIV_barplotDAT()) %in% "Phylum")]]
								plotfigs <- lapply(sort(adivADIV()), function(x) {									
									bpdat <- melt(bpDAT[bpDAT$ADIV %in% x,])
									bpMEANdat <- bpdat[bpdat$stat %in% "MEAN",]
									bpSEMdat <- bpdat[bpdat$stat %in% "SEM",]
									xpos <- as.character(bpMEANdat$variable)
									vtop <- bpMEANdat$value + bpSEMdat$value
									vbot <- bpMEANdat$value - bpSEMdat$value	
									xposhor1 <- paste0(as.character(bpMEANdat$variable), ":0.3")
									xposhor2 <- paste0(as.character(bpMEANdat$variable), ":0.7")							
									figure(data=bpMEANdat, legend_location=NULL) %>% ly_bar(variable, value, color=variable, hover = TRUE) %>%
										set_palette(discrete_color = pal_color(c("#c41e3a"))) %>% theme_axis("x", major_label_orientation = 45) %>%
										ly_segments(x0 = xpos, y0 = vtop, x1 = xpos, y1 = vbot, width = 1.5, color="#696969") %>%
										ly_segments(x0 = rep(xposhor1, 2), y0 = c(vtop,vbot), x1 = rep(xposhor2,2), y1 = c(vtop,vbot), width = 1.5, color="#696969")
								})
							} else {
								if(input$adivPLOTtype %in% "qq") {
									plotfigs <- lapply(sort(adivADIV()), function(x) {
										normdat <- normDAT[normDAT$ADIV %in% x,]
										qqlineres <- qqlineRESULTS(normdat$value)
										figure(data = normdat, xlab = "Theoretical Quantiles", ylab = "Sample Quartiles") %>% 
											ly_points(QQx, QQy, color="#c41e3a", hover = list(QQx, QQy),xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>%
											ly_abline(qqlineres$intercept, qqlineres$slope, color="#696969")
									})
								} else {
									NULL
								}
							}
						}
					} else {
						plotfigs <- lapply(sort(adivADIV()), function(x) {
							dat <- phylumDFmelt[phylumDFmelt$variable %in% x,]   
							figure(data = dat, legend_location=NULL) %>%
								ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="Phylum Alpha Diversity") %>%
								ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
									set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
						}) 
					}
				}
				names(plotfigs) <- sort(adivADIV())
				if(length(adivADIV()) == 1) {
					grid_plot(plotfigs, ncol=1)
				} else {
					grid_plot(plotfigs, ncol=2)
				}
			} else {
				return(NULL)
			}
		} else {
			NULL
		}
	})

	########################################################################		
	## Create DataTable object with Phylum alpha diversity data
	########################################################################
	output$ADIVphylumaovtableRENDER <- DT::renderDataTable({
		if(input$goADIV){
			if("Phylum" %in% adivTAXA()){		
				DT_DAT <- phylo_TAX_aDIV_testDAT()[[which(names(phylo_TAX_aDIV_testDAT()) %in% "Phylum")]]
				# pcol <- "P"
				datatable(DT_DAT, extensions='Buttons', rownames = FALSE,				
						options = list(
							dom = 'lf<"floatright"B>rtip',
							buttons = c('excel', 'pdf', 'csv'),
							searching = TRUE,
							pageLength = 5,
							lengthMenu = c(5, nrow(DT_DAT))
					)
				) 				
			} else {
				return(NULL)
			}
		} else {
			NULL
		}				
	})	
	
	PHYLUMdatatypeRENDER <- reactive({
		if(input$goADIV){
			switch(adivTEST(),
				"anova" = list("MEAN", "SEM", "1-way ANOVA"),
				"MFanova" = list("MEAN", "SEM", "Multi-Factor ANOVA"),
				"tt" = list("MEAN", "SEM", "t-test"),
				"kw" = list("MEDIAN", "25th Quartile, 75th Quartile", "Kruskal-Wallis Test"),
				"mwu" = list("MEDIAN", "25th Quartile, 75th Quartile", "Mann Whitney U Test")
			)
		} else {
			NULL
		}
	})
	
	output$ADIVphylumplottextRENDER <- renderUI({
		req(phyloseqFINAL())	
		if(input$goADIV){	
			switch(input$adivPLOTtype, 
				"box" = HTML("
							<p>Box-and-whisker plot that displays distribution of data between or among groups. Calculation of box plot statistics can be 
							found <a href=\"https://www.rdocumentation.org/packages/grDevices/versions/3.4.1/topics/boxplot.stats\" target=\"_blank\">here</a>.

							</p>
							"
						),
				"bar" = HTML("
							<p>Barplot displays the mean of all groups.  Error bars are standard error of the means.</p>
							"
						),	
				"qq" = HTML("
							<p>Q-Q plot helps to determine whether the data is normally distributed. The points on a Q-Q plot should follow a diagonal straight line if the data is normally distributed.
							Deviations from the line may suggest that assumptions are not met and non-parametric tests may be an alternative option.</p>
							"
						),	
				"fvsr" = HTML("
							<p>Fited vs Residuals plot helps to determine whether an ANOVA meets the heterogeneity of variance assumptions.  Points on the plot should appear at 
							random over and under the 0 value on the y axis.  Visual identification of a pattern (e.g., close clustering of low values on the x axis vs wide clustering of 
							greater x values may suggest heteroscedasticity. Presence of heteroscedasticity may suggest the use of a non-parametric alternative.</p>
							"
						)	
			)
		} else {
			NULL
		}		
	})

	ADIVplottextRENDER <- reactive({
		if(input$goADIV){
			if("Class" %in% adivTAXA()){	
				if(adivTEST() %in% c("anova", "MFanova")) {
					if(input$adivPLOTtype %in% "box") {
						"<p>Box-and-whisker plot that displays distribution of data between or among groups. Calculation of box plot statistics can be 
						found <a href=\"https://www.rdocumentation.org/packages/grDevices/versions/3.4.1/topics/boxplot.stats\" target=\"_blank\">here</a>.</p>"
					} else {
						if(input$adivPLOTtype %in% "bar") {
							"<p>Barplot displays the mean of all groups.  Error bars are standard error of the means.</p>"
						} else {
							if(input$adivPLOTtype %in% "qq") {
								"<p>Q-Q plot helps to determine whether the data is normally distributed. The points on a Q-Q plot should follow a diagonal straight line if the data is normally distributed.
								Deviations from the line may suggest that assumptions are not met and non-parametric tests may be an alternative option.</p>"
							} else {
								if(input$adivPLOTtype %in% "fvsr") {
									"<p>Fited vs Residuals plot helps to determine whether an ANOVA meets the heterogeneity of variance assumptions.  Points on the plot should appear at 
									random over and under the 0 value on the y axis.  Visual identification of a pattern (e.g., close clustering of low values on the x axis vs wide clustering of 
									greater x values may suggest heteroscedasticity. Presence of heteroscedasticity may suggest the use of a non-parametric alternative.</p>"
								}
							}
						}
					}
				} else {
					if(adivTEST() %in% c("tt")) {
						if(input$adivPLOTtype %in% "box") {
							"<p>Box-and-whisker plot that displays distribution of data between or among groups. Calculation of box plot statistics can be 
							found <a href=\"https://www.rdocumentation.org/packages/grDevices/versions/3.4.1/topics/boxplot.stats\" target=\"_blank\">here</a>.</p>"
						} else {
							if(input$adivPLOTtype %in% "bar") {
								"<p>Barplot displays the mean of all groups.  Error bars are standard error of the means.</p>"
							} else {
								if(input$adivPLOTtype %in% "qq") {
									"<p>Q-Q plot helps to determine whether the data is normally distributed. The points on a Q-Q plot should follow a diagonal straight line if the data is normally distributed.
									Deviations from the line may suggest that assumptions are not met and non-parametric tests may be an alternative option.</p>"
								} else {
									NULL
								}
							}
						}
					} else {
						"<p>Box-and-whisker plot that displays distribution of data between or among groups. Calculation of box plot statistics can be 
					found <a href=\"https://www.rdocumentation.org/packages/grDevices/versions/3.4.1/topics/boxplot.stats\" target=\"_blank\">here</a>.</p>"
					}
				}
			} else {
				return(NULL)
			}
		} else {
			NULL
		}	
	})
	
	########################################################################		
	## Render UI Phylum alpha diversity graphic/table
	########################################################################	
	output$ADIVphylumgraphicsRENDER <- renderUI({
		if(input$goADIV){
			if("Phylum" %in% adivTAXA()){	

				list(
					fluidPage(
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity Graphics at <em>Phylum</em> level.</strong></h4>
								 "
							),
							HTML(ADIVplottextRENDER()
							)
						),
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity", PHYLUMdatatypeRENDER()[[3]], "Results at <em>Phylum</em> level.</strong></h4>
								 "
							)
						)
					),
					fluidPage(
						column(6,
							rbokehOutput("PHYLUMadivRbokeh_RENDER", height="200px", width="600px")
							# rbokehOutput("PHYLUMadivRbokeh_normRENDER", height="200px", width="600px")
						),
						column(6,						
							HTML("
								 <p>Data are presented as ", PHYLUMdatatypeRENDER()[[1]], " (", PHYLUMdatatypeRENDER()[[2]],").  <em>P</em>-value(s) derived from", PHYLUMdatatypeRENDER()[[3]],".  </p>
								 "
							),
							DT::dataTableOutput("ADIVphylumaovtableRENDER", width = "100%", height = "800px"),
							br()
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
	## Create rbokeh object with Class barplot data
	########################################################################	
	output$CLASSadivRbokeh_RENDER <- renderRbokeh({
		if(input$goADIV){
			if("Class" %in% adivTAXA()){	
				DFmelt <- melt(phylumDF <- phylo_TAX_aDIV()[[which(names(phylo_TAX_aDIV()) %in% "Class")]])
				if(length(adivGROUP()) == 1) {				
					DFmelt$Level <- factor(DFmelt[,adivGROUP()])
				} else {
					DFmelt$Level <- factor(apply(DFmelt[,adivGROUP()], 1, paste, collapse = "_"))
				}
				
				normDAT <- phylo_TAX_aDIV_normDATA()[[which(names(phylo_TAX_aDIV_normDATA()) %in% "Class")]]	
				if(adivTEST() %in% c("anova", "MFanova")) {
					if(input$adivPLOTtype %in% "box") {
						plotfigs <- lapply(sort(adivADIV()), function(x) {
							dat <- DFmelt[DFmelt$variable %in% x,]   
							figure(data = dat, legend_location=NULL) %>%
								ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="Class Alpha Diversity") %>%
								ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
									set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
						}) 
					} else {
						if(input$adivPLOTtype %in% "bar") {
							bpDAT <- phylo_TAX_aDIV_barplotDAT()[[which(names(phylo_TAX_aDIV_barplotDAT()) %in% "Class")]]
							plotfigs <- lapply(sort(adivADIV()), function(x) {
								bpdat <- melt(bpDAT[bpDAT$ADIV %in% x,])
								bpMEANdat <- bpdat[bpdat$stat %in% "MEAN",]
								bpSEMdat <- bpdat[bpdat$stat %in% "SEM",]
								xpos <- as.character(bpMEANdat$variable)
								vtop <- bpMEANdat$value + bpSEMdat$value
								vbot <- bpMEANdat$value - bpSEMdat$value	
								xposhor1 <- paste0(as.character(bpMEANdat$variable), ":0.3")
								xposhor2 <- paste0(as.character(bpMEANdat$variable), ":0.7")							
								figure(data=bpMEANdat, legend_location=NULL) %>% ly_bar(variable, value, color=variable, hover = TRUE) %>%
									set_palette(discrete_color = pal_color(c("#c41e3a"))) %>% theme_axis("x", major_label_orientation = 45) %>%
									ly_segments(x0 = xpos, y0 = vtop, x1 = xpos, y1 = vbot, width = 1.5, color="#696969") %>%
									ly_segments(x0 = rep(xposhor1, 2), y0 = c(vtop,vbot), x1 = rep(xposhor2,2), y1 = c(vtop,vbot), width = 1.5, color="#696969")
							})
						} else {
							if(input$adivPLOTtype %in% "qq") {
								plotfigs <- lapply(sort(adivADIV()), function(x) {
									normdat <- normDAT[normDAT$ADIV %in% x,]
									qqlineres <- qqlineRESULTS(normdat$SResids)
									figure(data = normdat, xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>% 
										ly_points(QQx, QQy, color="#c41e3a", hover = list(QQx, QQy),xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>%
										ly_abline(qqlineres$intercept, qqlineres$slope, color="#696969")
								})
							} else {
								if(input$adivPLOTtype %in% "fvsr") {
									plotfigs <- lapply(sort(adivADIV()), function(x) {
										normdat <- normDAT[normDAT$ADIV %in% x,]
										figure(data = normdat, title = "Residuals vs Fitted") %>%
										ly_points(Fitted, Residuals,color="#c41e3a", xlab = "Fitted Values", ylab = "Residuals",
											hover = c("Fitted" = Fitted, "Residuals" = Residuals, "Alpha Diversity"= ADIV)) %>%
											ly_abline(h=0, type=2, color="black", alpha=0.5)
									})
								}
							}
						}
					}
				} else {
					if(adivTEST() %in% c("tt")) {
						if(input$adivPLOTtype %in% "box") {
							plotfigs <- lapply(sort(adivADIV()), function(x) {
								dat <- DFmelt[DFmelt$variable %in% x,]   
								figure(data = dat, legend_location=NULL) %>%
									ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="Class Alpha Diversity") %>%
									ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
										set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
							}) 
						} else {
							if(input$adivPLOTtype %in% "bar") {
								bpDAT <- phylo_TAX_aDIV_barplotDAT()[[which(names(phylo_TAX_aDIV_barplotDAT()) %in% "Class")]]
								plotfigs <- lapply(sort(adivADIV()), function(x) {									
									bpdat <- melt(bpDAT[bpDAT$ADIV %in% x,])
									bpMEANdat <- bpdat[bpdat$stat %in% "MEAN",]
									bpSEMdat <- bpdat[bpdat$stat %in% "SEM",]
									xpos <- as.character(bpMEANdat$variable)
									vtop <- bpMEANdat$value + bpSEMdat$value
									vbot <- bpMEANdat$value - bpSEMdat$value	
									xposhor1 <- paste0(as.character(bpMEANdat$variable), ":0.3")
									xposhor2 <- paste0(as.character(bpMEANdat$variable), ":0.7")							
									figure(data=bpMEANdat, legend_location=NULL) %>% ly_bar(variable, value, color=variable, hover = TRUE) %>%
										set_palette(discrete_color = pal_color(c("#c41e3a"))) %>% theme_axis("x", major_label_orientation = 45) %>%
										ly_segments(x0 = xpos, y0 = vtop, x1 = xpos, y1 = vbot, width = 1.5, color="#696969") %>%
										ly_segments(x0 = rep(xposhor1, 2), y0 = c(vtop,vbot), x1 = rep(xposhor2,2), y1 = c(vtop,vbot), width = 1.5, color="#696969")
								})
							} else {
								if(input$adivPLOTtype %in% "qq") {
									plotfigs <- lapply(sort(adivADIV()), function(x) {
										normdat <- normDAT[normDAT$ADIV %in% x,]
										qqlineres <- qqlineRESULTS(normdat$value)
										figure(data = normdat, xlab = "Theoretical Quantiles", ylab = "Sample Quartiles") %>% 
											ly_points(QQx, QQy, color="#c41e3a", hover = list(QQx, QQy),xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>%
											ly_abline(qqlineres$intercept, qqlineres$slope, color="#696969")
									})
								} else {
									NULL
								}
							}
						}
					} else {
						plotfigs <- lapply(sort(adivADIV()), function(x) {
							dat <- DFmelt[DFmelt$variable %in% x,]   
							figure(data = dat, legend_location=NULL) %>%
								ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="Class Alpha Diversity") %>%
								ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
									set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
						}) 
					}
				}
				names(plotfigs) <- sort(adivADIV())
				if(length(adivADIV()) == 1) {
					grid_plot(plotfigs, ncol=1)
				} else {
					grid_plot(plotfigs, ncol=2)
				}
			} else {
				return(NULL)
			}
		} else {
			NULL
		}
	})

	########################################################################		
	## Create DataTable object with Class alpha diversity data
	########################################################################	
	output$ADIVclassaovtableRENDER <- DT::renderDataTable({
		if(input$goADIV){
			if("Class" %in% adivTAXA()){		
			DT_DAT <- phylo_TAX_aDIV_testDAT()[[which(names(phylo_TAX_aDIV_testDAT()) %in% "Class")]]
			datatable(DT_DAT, extensions='Buttons', rownames = FALSE,				
						options = list(
							dom = 'lf<"floatright"B>rtip',
							buttons = c('excel', 'pdf', 'csv'),
							searching = TRUE,
							pageLength = 5,
							lengthMenu = c(5, nrow(DT_DAT))
							
							# extensions = list('FixedColumns'=NULL), 
							# rownames = FALSE, 
							# options = list(
								# dom = '<"floatright"B>t',
								# scrollX = TRUE,
								# fixedColumns = list(leftColumns = 1)
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
	## Render UI Class alpha diversity graphic/table
	########################################################################	
	output$ADIVclassgraphicsRENDER <- renderUI({
		if(input$goADIV){
			if("Class" %in% adivTAXA()){	
		
				list(
					fluidPage(
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity Graphics at <em>Class</em> level.</strong></h4>
								 "
							),						
							HTML(ADIVplottextRENDER()
							)
						),
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity", PHYLUMdatatypeRENDER()[[3]], "Results at <em>Class</em> level.</strong></h4>
								 "
							)
						)					
					),
					fluidPage(
						column(6,
							rbokehOutput("CLASSadivRbokeh_RENDER", height="200px", width="100px")
						),
						column(6,
							DT::dataTableOutput("ADIVclassaovtableRENDER", width = "100%", height = "800px"),
							br()
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
	## Create rbokeh object with Order barplot data
	########################################################################	
	output$ORDERadivRbokeh_RENDER <- renderRbokeh({
		if(input$goADIV){
			if("Order" %in% adivTAXA()){	
				DFmelt <- melt(phylumDF <- phylo_TAX_aDIV()[[which(names(phylo_TAX_aDIV()) %in% "Order")]])
				if(length(adivGROUP()) == 1) {				
					DFmelt$Level <- factor(DFmelt[,adivGROUP()])
				} else {
					DFmelt$Level <- factor(apply(DFmelt[,adivGROUP()], 1, paste, collapse = "_"))
				}
				
				normDAT <- phylo_TAX_aDIV_normDATA()[[which(names(phylo_TAX_aDIV_normDATA()) %in% "Order")]]	
				if(adivTEST() %in% c("anova", "MFanova")) {
					if(input$adivPLOTtype %in% "box") {
						plotfigs <- lapply(sort(adivADIV()), function(x) {
							dat <- DFmelt[DFmelt$variable %in% x,]   
							figure(data = dat, legend_location=NULL) %>%
								ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="Order Alpha Diversity") %>%
								ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
									set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
						}) 
					} else {
						if(input$adivPLOTtype %in% "bar") {
							bpDAT <- phylo_TAX_aDIV_barplotDAT()[[which(names(phylo_TAX_aDIV_barplotDAT()) %in% "Order")]]
							plotfigs <- lapply(sort(adivADIV()), function(x) {
								bpdat <- melt(bpDAT[bpDAT$ADIV %in% x,])
								bpMEANdat <- bpdat[bpdat$stat %in% "MEAN",]
								bpSEMdat <- bpdat[bpdat$stat %in% "SEM",]
								xpos <- as.character(bpMEANdat$variable)
								vtop <- bpMEANdat$value + bpSEMdat$value
								vbot <- bpMEANdat$value - bpSEMdat$value	
								xposhor1 <- paste0(as.character(bpMEANdat$variable), ":0.3")
								xposhor2 <- paste0(as.character(bpMEANdat$variable), ":0.7")							
								figure(data=bpMEANdat, legend_location=NULL) %>% ly_bar(variable, value, color=variable, hover = TRUE) %>%
									set_palette(discrete_color = pal_color(c("#c41e3a"))) %>% theme_axis("x", major_label_orientation = 45) %>%
									ly_segments(x0 = xpos, y0 = vtop, x1 = xpos, y1 = vbot, width = 1.5, color="#696969") %>%
									ly_segments(x0 = rep(xposhor1, 2), y0 = c(vtop,vbot), x1 = rep(xposhor2,2), y1 = c(vtop,vbot), width = 1.5, color="#696969")
							})
						} else {
							if(input$adivPLOTtype %in% "qq") {
								plotfigs <- lapply(sort(adivADIV()), function(x) {
									normdat <- normDAT[normDAT$ADIV %in% x,]
									qqlineres <- qqlineRESULTS(normdat$SResids)
									figure(data = normdat, xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>% 
										ly_points(QQx, QQy, color="#c41e3a", hover = list(QQx, QQy),xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>%
										ly_abline(qqlineres$intercept, qqlineres$slope, color="#696969")
								})
							} else {
								if(input$adivPLOTtype %in% "fvsr") {
									plotfigs <- lapply(sort(adivADIV()), function(x) {
										normdat <- normDAT[normDAT$ADIV %in% x,]
										figure(data = normdat, title = "Residuals vs Fitted") %>%
										ly_points(Fitted, Residuals,color="#c41e3a", xlab = "Fitted Values", ylab = "Residuals",
											hover = c("Fitted" = Fitted, "Residuals" = Residuals, "Alpha Diversity"= ADIV)) %>%
											ly_abline(h=0, type=2, color="black", alpha=0.5)
									})
								}
							}
						}
					}
				} else {
					if(adivTEST() %in% c("tt")) {
						if(input$adivPLOTtype %in% "box") {
							plotfigs <- lapply(sort(adivADIV()), function(x) {
								dat <- DFmelt[DFmelt$variable %in% x,]   
								figure(data = dat, legend_location=NULL) %>%
									ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="Order Alpha Diversity") %>%
									ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
										set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
							}) 
						} else {
							if(input$adivPLOTtype %in% "bar") {
								bpDAT <- phylo_TAX_aDIV_barplotDAT()[[which(names(phylo_TAX_aDIV_barplotDAT()) %in% "Order")]]
								plotfigs <- lapply(sort(adivADIV()), function(x) {									
									bpdat <- melt(bpDAT[bpDAT$ADIV %in% x,])
									bpMEANdat <- bpdat[bpdat$stat %in% "MEAN",]
									bpSEMdat <- bpdat[bpdat$stat %in% "SEM",]
									xpos <- as.character(bpMEANdat$variable)
									vtop <- bpMEANdat$value + bpSEMdat$value
									vbot <- bpMEANdat$value - bpSEMdat$value	
									xposhor1 <- paste0(as.character(bpMEANdat$variable), ":0.3")
									xposhor2 <- paste0(as.character(bpMEANdat$variable), ":0.7")							
									figure(data=bpMEANdat, legend_location=NULL) %>% ly_bar(variable, value, color=variable, hover = TRUE) %>%
										set_palette(discrete_color = pal_color(c("#c41e3a"))) %>% theme_axis("x", major_label_orientation = 45) %>%
										ly_segments(x0 = xpos, y0 = vtop, x1 = xpos, y1 = vbot, width = 1.5, color="#696969") %>%
										ly_segments(x0 = rep(xposhor1, 2), y0 = c(vtop,vbot), x1 = rep(xposhor2,2), y1 = c(vtop,vbot), width = 1.5, color="#696969")
								})
							} else {
								if(input$adivPLOTtype %in% "qq") {
									plotfigs <- lapply(sort(adivADIV()), function(x) {
										normdat <- normDAT[normDAT$ADIV %in% x,]
										qqlineres <- qqlineRESULTS(normdat$value)
										figure(data = normdat, xlab = "Theoretical Quantiles", ylab = "Sample Quartiles") %>% 
											ly_points(QQx, QQy, color="#c41e3a", hover = list(QQx, QQy),xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>%
											ly_abline(qqlineres$intercept, qqlineres$slope, color="#696969")
									})
								} else {
									NULL
								}
							}
						}
					} else {
						plotfigs <- lapply(sort(adivADIV()), function(x) {
							dat <- DFmelt[DFmelt$variable %in% x,]   
							figure(data = dat, legend_location=NULL) %>%
								ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="Order Alpha Diversity") %>%
								ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
									set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
						}) 
					}
				}
				names(plotfigs) <- sort(adivADIV())
				if(length(adivADIV()) == 1) {
					grid_plot(plotfigs, ncol=1)
				} else {
					grid_plot(plotfigs, ncol=2)
				}
			} else {
				return(NULL)
			}
		} else {
			NULL
		}
	})

	########################################################################		
	## Create DataTable object with Order alpha diversity data
	########################################################################	
	output$ADIVorderaovtableRENDER <- DT::renderDataTable({
		if(input$goADIV){
			if("Order" %in% adivTAXA()){		
			DT_DAT <- phylo_TAX_aDIV_testDAT()[[which(names(phylo_TAX_aDIV_testDAT()) %in% "Order")]]
			datatable(DT_DAT, extensions='Buttons', rownames = FALSE,				
						options = list(
							dom = 'lf<"floatright"B>rtip',
							buttons = c('excel', 'pdf', 'csv'),
							searching = TRUE,
							pageLength = 5,
							lengthMenu = c(5, nrow(DT_DAT))
				)
			)
				
			} else {
				return(NULL)
			}
		} else {
			NULL
		}				
	})	
	
	output$ADIVorderplottextRENDER <- renderUI({
		req(phyloseqFINAL())	
		if(input$goADIV){	
			switch(input$adivPLOTtype, 
				"box" = HTML("
							<p>Box-and-whisker plot that displays distribution of data between or among groups. Calculation of box plot statistics can be 
							found <a href=\"https://www.rdocumentation.org/packages/grDevices/versions/3.4.1/topics/boxplot.stats\" target=\"_blank\">here</a>.

							</p>
							"
						),
				"bar" = HTML("
							<p>Barplot displays the mean of all groups.  Error bars are standard error of the means.</p>
							"
						),	
				"qq" = HTML("
							<p>Q-Q plot helps to determine whether the data is normally distributed. The points on a Q-Q plot should follow a diagonal straight line if the data is normally distributed.
							Deviations from the line may suggest that assumptions are not met and non-parametric tests may be an alternative option.</p>
							"
						),	
				"fvsr" = HTML("
							<p>Fited vs Residuals plot helps to determine whether an ANOVA meets the heterogeneity of variance assumptions.  Points on the plot should appear at 
							random over and under the 0 value on the y axis.  Visual identification of a pattern (e.g., close clustering of low values on the x axis vs wide clustering of 
							greater x values may suggest heteroscedasticity. Presence of heteroscedasticity may suggest the use of a non-parametric alternative.</p>
							"
						)	
			)
		} else {
			NULL
		}		
	})
	
	########################################################################		
	## Render UI Order alpha diversity graphic/table
	########################################################################	
	output$ADIVordergraphicsRENDER <- renderUI({
		if(input$goADIV){
			if("Order" %in% adivTAXA()){		
	
				list(
					fluidPage(
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity Graphics at <em>Order</em> level.</strong></h4>
								 "
							),
							HTML(ADIVplottextRENDER()
							)
						),
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity", PHYLUMdatatypeRENDER()[[3]], "Results at <em>Order</em> level.</strong></h4>
								 "
							)
						)	
					),
					fluidPage(
						column(6,
							rbokehOutput("ORDERadivRbokeh_RENDER", height="200px", width="100px")
						),
						column(6,
							HTML("
								 <p>Data are presented as ", PHYLUMdatatypeRENDER()[[1]], " (", PHYLUMdatatypeRENDER()[[2]],").  <em>P</em>-value(s) derived from", PHYLUMdatatypeRENDER()[[3]],".  </p>
								 "
							),						
							DT::dataTableOutput("ADIVorderaovtableRENDER", width = "100%", height = "800px"),
							br()
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
	## Create rbokeh object with Family barplot data
	########################################################################	
	output$FAMILYadivRbokeh_RENDER <- renderRbokeh({
		if(input$goADIV){
			if("Family" %in% adivTAXA()){	
				DFmelt <- melt(phylumDF <- phylo_TAX_aDIV()[[which(names(phylo_TAX_aDIV()) %in% "Family")]])
				if(length(adivGROUP()) == 1) {				
					DFmelt$Level <- factor(DFmelt[,adivGROUP()])
				} else {
					DFmelt$Level <- factor(apply(DFmelt[,adivGROUP()], 1, paste, collapse = "_"))
				}
				
				normDAT <- phylo_TAX_aDIV_normDATA()[[which(names(phylo_TAX_aDIV_normDATA()) %in% "Family")]]	
				if(adivTEST() %in% c("anova", "MFanova")) {
					if(input$adivPLOTtype %in% "box") {
						plotfigs <- lapply(sort(adivADIV()), function(x) {
							dat <- DFmelt[DFmelt$variable %in% x,]   
							figure(data = dat, legend_location=NULL) %>%
								ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="Family Alpha Diversity") %>%
								ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
									set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
						}) 
					} else {
						if(input$adivPLOTtype %in% "bar") {
							bpDAT <- phylo_TAX_aDIV_barplotDAT()[[which(names(phylo_TAX_aDIV_barplotDAT()) %in% "Family")]]
							plotfigs <- lapply(sort(adivADIV()), function(x) {
								bpdat <- melt(bpDAT[bpDAT$ADIV %in% x,])
								bpMEANdat <- bpdat[bpdat$stat %in% "MEAN",]
								bpSEMdat <- bpdat[bpdat$stat %in% "SEM",]
								xpos <- as.character(bpMEANdat$variable)
								vtop <- bpMEANdat$value + bpSEMdat$value
								vbot <- bpMEANdat$value - bpSEMdat$value	
								xposhor1 <- paste0(as.character(bpMEANdat$variable), ":0.3")
								xposhor2 <- paste0(as.character(bpMEANdat$variable), ":0.7")							
								figure(data=bpMEANdat, legend_location=NULL) %>% ly_bar(variable, value, color=variable, hover = TRUE) %>%
									set_palette(discrete_color = pal_color(c("#c41e3a"))) %>% theme_axis("x", major_label_orientation = 45) %>%
									ly_segments(x0 = xpos, y0 = vtop, x1 = xpos, y1 = vbot, width = 1.5, color="#696969") %>%
									ly_segments(x0 = rep(xposhor1, 2), y0 = c(vtop,vbot), x1 = rep(xposhor2,2), y1 = c(vtop,vbot), width = 1.5, color="#696969")
							})
						} else {
							if(input$adivPLOTtype %in% "qq") {
								plotfigs <- lapply(sort(adivADIV()), function(x) {
									normdat <- normDAT[normDAT$ADIV %in% x,]
									qqlineres <- qqlineRESULTS(normdat$SResids)
									figure(data = normdat, xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>% 
										ly_points(QQx, QQy, color="#c41e3a", hover = list(QQx, QQy),xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>%
										ly_abline(qqlineres$intercept, qqlineres$slope, color="#696969")
								})
							} else {
								if(input$adivPLOTtype %in% "fvsr") {
									plotfigs <- lapply(sort(adivADIV()), function(x) {
										normdat <- normDAT[normDAT$ADIV %in% x,]
										figure(data = normdat, title = "Residuals vs Fitted") %>%
										ly_points(Fitted, Residuals,color="#c41e3a", xlab = "Fitted Values", ylab = "Residuals",
											hover = c("Fitted" = Fitted, "Residuals" = Residuals, "Alpha Diversity"= ADIV)) %>%
											ly_abline(h=0, type=2, color="black", alpha=0.5)
									})
								}
							}
						}
					}
				} else {
					if(adivTEST() %in% c("tt")) {
						if(input$adivPLOTtype %in% "box") {
							plotfigs <- lapply(sort(adivADIV()), function(x) {
								dat <- DFmelt[DFmelt$variable %in% x,]   
								figure(data = dat, legend_location=NULL) %>%
									ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="Family Alpha Diversity") %>%
									ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
										set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
							}) 
						} else {
							if(input$adivPLOTtype %in% "bar") {
								bpDAT <- phylo_TAX_aDIV_barplotDAT()[[which(names(phylo_TAX_aDIV_barplotDAT()) %in% "Family")]]
								plotfigs <- lapply(sort(adivADIV()), function(x) {									
									bpdat <- melt(bpDAT[bpDAT$ADIV %in% x,])
									bpMEANdat <- bpdat[bpdat$stat %in% "MEAN",]
									bpSEMdat <- bpdat[bpdat$stat %in% "SEM",]
									xpos <- as.character(bpMEANdat$variable)
									vtop <- bpMEANdat$value + bpSEMdat$value
									vbot <- bpMEANdat$value - bpSEMdat$value	
									xposhor1 <- paste0(as.character(bpMEANdat$variable), ":0.3")
									xposhor2 <- paste0(as.character(bpMEANdat$variable), ":0.7")							
									figure(data=bpMEANdat, legend_location=NULL) %>% ly_bar(variable, value, color=variable, hover = TRUE) %>%
										set_palette(discrete_color = pal_color(c("#c41e3a"))) %>% theme_axis("x", major_label_orientation = 45) %>%
										ly_segments(x0 = xpos, y0 = vtop, x1 = xpos, y1 = vbot, width = 1.5, color="#696969") %>%
										ly_segments(x0 = rep(xposhor1, 2), y0 = c(vtop,vbot), x1 = rep(xposhor2,2), y1 = c(vtop,vbot), width = 1.5, color="#696969")
								})
							} else {
								if(input$adivPLOTtype %in% "qq") {
									plotfigs <- lapply(sort(adivADIV()), function(x) {
										normdat <- normDAT[normDAT$ADIV %in% x,]
										qqlineres <- qqlineRESULTS(normdat$value)
										figure(data = normdat, xlab = "Theoretical Quantiles", ylab = "Sample Quartiles") %>% 
											ly_points(QQx, QQy, color="#c41e3a", hover = list(QQx, QQy),xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>%
											ly_abline(qqlineres$intercept, qqlineres$slope, color="#696969")
									})
								} else {
									NULL
								}
							}
						}
					} else {
						plotfigs <- lapply(sort(adivADIV()), function(x) {
							dat <- DFmelt[DFmelt$variable %in% x,]   
							figure(data = dat, legend_location=NULL) %>%
								ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="Family Alpha Diversity") %>%
								ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
									set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
						}) 
					}
				}
				names(plotfigs) <- sort(adivADIV())
				if(length(adivADIV()) == 1) {
					grid_plot(plotfigs, ncol=1)
				} else {
					grid_plot(plotfigs, ncol=2)
				}
			} else {
				return(NULL)
			}
		} else {
			NULL
		}
	})

	########################################################################		
	## Create DataTable object with Family alpha diversity data
	########################################################################	
	output$ADIVfamilyaovtableRENDER <- DT::renderDataTable({
		if(input$goADIV){
			if("Family" %in% adivTAXA()){		
			DT_DAT <- phylo_TAX_aDIV_testDAT()[[which(names(phylo_TAX_aDIV_testDAT()) %in% "Family")]]
			datatable(DT_DAT, extensions='Buttons', rownames = FALSE,				
						options = list(
							dom = 'lf<"floatright"B>rtip',
							buttons = c('excel', 'pdf', 'csv'),
							searching = TRUE,
							pageLength = 5,
							lengthMenu = c(5, nrow(DT_DAT))
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
	## Render UI Family alpha diversity graphic/table
	########################################################################	
	output$ADIVfamilygraphicsRENDER <- renderUI({
		if(input$goADIV){
			if("Family" %in% adivTAXA()){	
				
				list(
					fluidPage(
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity Graphics at <em>Family</em> level.</strong></h4>
								 "
							),
							HTML(ADIVplottextRENDER()
							)
						),
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity", PHYLUMdatatypeRENDER()[[3]], "Results at <em>Family</em> level.</strong></h4>
								 "
							)
						)	
					),
					fluidPage(
						column(6,
							rbokehOutput("FAMILYadivRbokeh_RENDER", height="200px", width="100px")
						),
						column(6,
							HTML("
								 <p>Data are presented as ", PHYLUMdatatypeRENDER()[[1]], " (", PHYLUMdatatypeRENDER()[[2]],").  <em>P</em>-value(s) derived from", PHYLUMdatatypeRENDER()[[3]],".  </p>
								 "
							),					
							DT::dataTableOutput("ADIVfamilyaovtableRENDER", width = "100%", height = "800px"),
							br()
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
	## Create rbokeh object with Genus barplot data
	########################################################################	
	output$GENUSadivRbokeh_RENDER <- renderRbokeh({
		if(input$goADIV){
			if("Genus" %in% adivTAXA()){	
				DFmelt <- melt(phylumDF <- phylo_TAX_aDIV()[[which(names(phylo_TAX_aDIV()) %in% "Genus")]])
				if(length(adivGROUP()) == 1) {				
					DFmelt$Level <- factor(DFmelt[,adivGROUP()])
				} else {
					DFmelt$Level <- factor(apply(DFmelt[,adivGROUP()], 1, paste, collapse = "_"))
				}
				
				normDAT <- phylo_TAX_aDIV_normDATA()[[which(names(phylo_TAX_aDIV_normDATA()) %in% "Genus")]]	
				if(adivTEST() %in% c("anova", "MFanova")) {
					if(input$adivPLOTtype %in% "box") {
						plotfigs <- lapply(sort(adivADIV()), function(x) {
							dat <- DFmelt[DFmelt$variable %in% x,]   
							figure(data = dat, legend_location=NULL) %>%
								ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="Genus Alpha Diversity") %>%
								ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
									set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
						}) 
					} else {
						if(input$adivPLOTtype %in% "bar") {
							bpDAT <- phylo_TAX_aDIV_barplotDAT()[[which(names(phylo_TAX_aDIV_barplotDAT()) %in% "Genus")]]
							plotfigs <- lapply(sort(adivADIV()), function(x) {
								bpdat <- melt(bpDAT[bpDAT$ADIV %in% x,])
								bpMEANdat <- bpdat[bpdat$stat %in% "MEAN",]
								bpSEMdat <- bpdat[bpdat$stat %in% "SEM",]
								xpos <- as.character(bpMEANdat$variable)
								vtop <- bpMEANdat$value + bpSEMdat$value
								vbot <- bpMEANdat$value - bpSEMdat$value	
								xposhor1 <- paste0(as.character(bpMEANdat$variable), ":0.3")
								xposhor2 <- paste0(as.character(bpMEANdat$variable), ":0.7")							
								figure(data=bpMEANdat, legend_location=NULL) %>% ly_bar(variable, value, color=variable, hover = TRUE) %>%
									set_palette(discrete_color = pal_color(c("#c41e3a"))) %>% theme_axis("x", major_label_orientation = 45) %>%
									ly_segments(x0 = xpos, y0 = vtop, x1 = xpos, y1 = vbot, width = 1.5, color="#696969") %>%
									ly_segments(x0 = rep(xposhor1, 2), y0 = c(vtop,vbot), x1 = rep(xposhor2,2), y1 = c(vtop,vbot), width = 1.5, color="#696969")
							})
						} else {
							if(input$adivPLOTtype %in% "qq") {
								plotfigs <- lapply(sort(adivADIV()), function(x) {
									normdat <- normDAT[normDAT$ADIV %in% x,]
									qqlineres <- qqlineRESULTS(normdat$SResids)
									figure(data = normdat, xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>% 
										ly_points(QQx, QQy, color="#c41e3a", hover = list(QQx, QQy),xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>%
										ly_abline(qqlineres$intercept, qqlineres$slope, color="#696969")
								})
							} else {
								if(input$adivPLOTtype %in% "fvsr") {
									plotfigs <- lapply(sort(adivADIV()), function(x) {
										normdat <- normDAT[normDAT$ADIV %in% x,]
										figure(data = normdat, title = "Residuals vs Fitted") %>%
										ly_points(Fitted, Residuals,color="#c41e3a", xlab = "Fitted Values", ylab = "Residuals",
											hover = c("Fitted" = Fitted, "Residuals" = Residuals, "Alpha Diversity"= ADIV)) %>%
											ly_abline(h=0, type=2, color="black", alpha=0.5)
									})
								}
							}
						}
					}
				} else {
					if(adivTEST() %in% c("tt")) {
						if(input$adivPLOTtype %in% "box") {
							plotfigs <- lapply(sort(adivADIV()), function(x) {
								dat <- DFmelt[DFmelt$variable %in% x,]   
								figure(data = dat, legend_location=NULL) %>%
									ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="Genus Alpha Diversity") %>%
									ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
										set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
							}) 
						} else {
							if(input$adivPLOTtype %in% "bar") {
								bpDAT <- phylo_TAX_aDIV_barplotDAT()[[which(names(phylo_TAX_aDIV_barplotDAT()) %in% "Genus")]]
								plotfigs <- lapply(sort(adivADIV()), function(x) {									
									bpdat <- melt(bpDAT[bpDAT$ADIV %in% x,])
									bpMEANdat <- bpdat[bpdat$stat %in% "MEAN",]
									bpSEMdat <- bpdat[bpdat$stat %in% "SEM",]
									xpos <- as.character(bpMEANdat$variable)
									vtop <- bpMEANdat$value + bpSEMdat$value
									vbot <- bpMEANdat$value - bpSEMdat$value	
									xposhor1 <- paste0(as.character(bpMEANdat$variable), ":0.3")
									xposhor2 <- paste0(as.character(bpMEANdat$variable), ":0.7")							
									figure(data=bpMEANdat, legend_location=NULL) %>% ly_bar(variable, value, color=variable, hover = TRUE) %>%
										set_palette(discrete_color = pal_color(c("#c41e3a"))) %>% theme_axis("x", major_label_orientation = 45) %>%
										ly_segments(x0 = xpos, y0 = vtop, x1 = xpos, y1 = vbot, width = 1.5, color="#696969") %>%
										ly_segments(x0 = rep(xposhor1, 2), y0 = c(vtop,vbot), x1 = rep(xposhor2,2), y1 = c(vtop,vbot), width = 1.5, color="#696969")
								})
							} else {
								if(input$adivPLOTtype %in% "qq") {
									plotfigs <- lapply(sort(adivADIV()), function(x) {
										normdat <- normDAT[normDAT$ADIV %in% x,]
										qqlineres <- qqlineRESULTS(normdat$value)
										figure(data = normdat, xlab = "Theoretical Quantiles", ylab = "Sample Quartiles") %>% 
											ly_points(QQx, QQy, color="#c41e3a", hover = list(QQx, QQy),xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>%
											ly_abline(qqlineres$intercept, qqlineres$slope, color="#696969")
									})
								} else {
									NULL
								}
							}
						}
					} else {
						plotfigs <- lapply(sort(adivADIV()), function(x) {
							dat <- DFmelt[DFmelt$variable %in% x,]   
							figure(data = dat, legend_location=NULL) %>%
								ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="Genus Alpha Diversity") %>%
								ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
									set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
						}) 
					}
				}
				names(plotfigs) <- sort(adivADIV())
				if(length(adivADIV()) == 1) {
					grid_plot(plotfigs, ncol=1)
				} else {
					grid_plot(plotfigs, ncol=2)
				}
			} else {
				return(NULL)
			}
		} else {
			NULL
		}
	})

	########################################################################		
	## Create DataTable object with Genus alpha diversity data
	########################################################################
	output$ADIVgenusaovtableRENDER <- DT::renderDataTable({
		if(input$goADIV){
			if("Genus" %in% adivTAXA()){		
			DT_DAT <- phylo_TAX_aDIV_testDAT()[[which(names(phylo_TAX_aDIV_testDAT()) %in% "Genus")]]
			datatable(DT_DAT, extensions='Buttons', rownames = FALSE,				
						options = list(
							dom = 'lf<"floatright"B>rtip',
							buttons = c('excel', 'pdf', 'csv'),
							searching = TRUE,
							pageLength = 5,
							lengthMenu = c(5, nrow(DT_DAT))
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
	## Render UI Genus alpha diversity graphic/table
	########################################################################	
	output$ADIVgenusgraphicsRENDER <- renderUI({
		if(input$goADIV){
			if("Genus" %in% adivTAXA()){		
				
				list(
					fluidPage(
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity Graphics at <em>Genus</em> level.</strong></h4>
								 "
							),
							HTML(ADIVplottextRENDER()
							)
						),
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity", PHYLUMdatatypeRENDER()[[3]], "Results at <em>Genus</em> level.</strong></h4>
								 "
							)
						)	
					),
					fluidPage(
						column(6,
							rbokehOutput("GENUSadivRbokeh_RENDER", height="200px", width="100px")
						),
						column(6,
							HTML("
								 <p>Data are presented as ", PHYLUMdatatypeRENDER()[[1]], " (", PHYLUMdatatypeRENDER()[[2]],").  <em>P</em>-value(s) derived from", PHYLUMdatatypeRENDER()[[3]],".  </p>
								 "
							),					
							DT::dataTableOutput("ADIVgenusaovtableRENDER", width = "100%", height = "800px"),
							br()
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
	## Create rbokeh object with OTU barplot data
	########################################################################	
	output$OTUadivRbokeh_RENDER <- renderRbokeh({
		if(input$goADIV){
			if("OTU" %in% adivTAXA()){	
				DFmelt <- melt(phylumDF <- phylo_TAX_aDIV()[[which(names(phylo_TAX_aDIV()) %in% "OTU")]])
				if(length(adivGROUP()) == 1) {				
					DFmelt$Level <- factor(DFmelt[,adivGROUP()])
				} else {
					DFmelt$Level <- factor(apply(DFmelt[,adivGROUP()], 1, paste, collapse = "_"))
				}
				
				normDAT <- phylo_TAX_aDIV_normDATA()[[which(names(phylo_TAX_aDIV_normDATA()) %in% "OTU")]]	
				if(adivTEST() %in% c("anova", "MFanova")) {
					if(input$adivPLOTtype %in% "box") {
						plotfigs <- lapply(sort(adivADIV()), function(x) {
							dat <- DFmelt[DFmelt$variable %in% x,]   
							figure(data = dat, legend_location=NULL) %>%
								ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="OTU Alpha Diversity") %>%
								ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
									set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
						}) 
					} else {
						if(input$adivPLOTtype %in% "bar") {
							bpDAT <- phylo_TAX_aDIV_barplotDAT()[[which(names(phylo_TAX_aDIV_barplotDAT()) %in% "OTU")]]
							plotfigs <- lapply(sort(adivADIV()), function(x) {
								bpdat <- melt(bpDAT[bpDAT$ADIV %in% x,])
								bpMEANdat <- bpdat[bpdat$stat %in% "MEAN",]
								bpSEMdat <- bpdat[bpdat$stat %in% "SEM",]
								xpos <- as.character(bpMEANdat$variable)
								vtop <- bpMEANdat$value + bpSEMdat$value
								vbot <- bpMEANdat$value - bpSEMdat$value	
								xposhor1 <- paste0(as.character(bpMEANdat$variable), ":0.3")
								xposhor2 <- paste0(as.character(bpMEANdat$variable), ":0.7")							
								figure(data=bpMEANdat, legend_location=NULL) %>% ly_bar(variable, value, color=variable, hover = TRUE) %>%
									set_palette(discrete_color = pal_color(c("#c41e3a"))) %>% theme_axis("x", major_label_orientation = 45) %>%
									ly_segments(x0 = xpos, y0 = vtop, x1 = xpos, y1 = vbot, width = 1.5, color="#696969") %>%
									ly_segments(x0 = rep(xposhor1, 2), y0 = c(vtop,vbot), x1 = rep(xposhor2,2), y1 = c(vtop,vbot), width = 1.5, color="#696969")
							})
						} else {
							if(input$adivPLOTtype %in% "qq") {
								plotfigs <- lapply(sort(adivADIV()), function(x) {
									normdat <- normDAT[normDAT$ADIV %in% x,]
									qqlineres <- qqlineRESULTS(normdat$SResids)
									figure(data = normdat, xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>% 
										ly_points(QQx, QQy, color="#c41e3a", hover = list(QQx, QQy),xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>%
										ly_abline(qqlineres$intercept, qqlineres$slope, color="#696969")
								})
							} else {
								if(input$adivPLOTtype %in% "fvsr") {
									plotfigs <- lapply(sort(adivADIV()), function(x) {
										normdat <- normDAT[normDAT$ADIV %in% x,]
										figure(data = normdat, title = "Residuals vs Fitted") %>%
										ly_points(Fitted, Residuals,color="#c41e3a", xlab = "Fitted Values", ylab = "Residuals",
											hover = c("Fitted" = Fitted, "Residuals" = Residuals, "Alpha Diversity"= ADIV)) %>%
											ly_abline(h=0, type=2, color="black", alpha=0.5)
									})
								}
							}
						}
					}
				} else {
					if(adivTEST() %in% c("tt")) {
						if(input$adivPLOTtype %in% "box") {
							plotfigs <- lapply(sort(adivADIV()), function(x) {
								dat <- DFmelt[DFmelt$variable %in% x,]   
								figure(data = dat, legend_location=NULL) %>%
									ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="OTU Alpha Diversity") %>%
									ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
										set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
							}) 
						} else {
							if(input$adivPLOTtype %in% "bar") {
								bpDAT <- phylo_TAX_aDIV_barplotDAT()[[which(names(phylo_TAX_aDIV_barplotDAT()) %in% "OTU")]]
								plotfigs <- lapply(sort(adivADIV()), function(x) {									
									bpdat <- melt(bpDAT[bpDAT$ADIV %in% x,])
									bpMEANdat <- bpdat[bpdat$stat %in% "MEAN",]
									bpSEMdat <- bpdat[bpdat$stat %in% "SEM",]
									xpos <- as.character(bpMEANdat$variable)
									vtop <- bpMEANdat$value + bpSEMdat$value
									vbot <- bpMEANdat$value - bpSEMdat$value	
									xposhor1 <- paste0(as.character(bpMEANdat$variable), ":0.3")
									xposhor2 <- paste0(as.character(bpMEANdat$variable), ":0.7")							
									figure(data=bpMEANdat, legend_location=NULL) %>% ly_bar(variable, value, color=variable, hover = TRUE) %>%
										set_palette(discrete_color = pal_color(c("#c41e3a"))) %>% theme_axis("x", major_label_orientation = 45) %>%
										ly_segments(x0 = xpos, y0 = vtop, x1 = xpos, y1 = vbot, width = 1.5, color="#696969") %>%
										ly_segments(x0 = rep(xposhor1, 2), y0 = c(vtop,vbot), x1 = rep(xposhor2,2), y1 = c(vtop,vbot), width = 1.5, color="#696969")
								})
							} else {
								if(input$adivPLOTtype %in% "qq") {
									plotfigs <- lapply(sort(adivADIV()), function(x) {
										normdat <- normDAT[normDAT$ADIV %in% x,]
										qqlineres <- qqlineRESULTS(normdat$value)
										figure(data = normdat, xlab = "Theoretical Quantiles", ylab = "Sample Quartiles") %>% 
											ly_points(QQx, QQy, color="#c41e3a", hover = list(QQx, QQy),xlab = "Theoretical Quantiles", ylab = "Standardized residuals") %>%
											ly_abline(qqlineres$intercept, qqlineres$slope, color="#696969")
									})
								} else {
									NULL
								}
							}
						}
					} else {
						plotfigs <- lapply(sort(adivADIV()), function(x) {
							dat <- DFmelt[DFmelt$variable %in% x,]   
							figure(data = dat, legend_location=NULL) %>%
								ly_points(catjitter(Level), value, color = "black", hover = list(Level, value), xlab = "", ylab="OTU Alpha Diversity") %>%
								ly_boxplot(Level, value, color = Level, outlier_glyph = NA, line_color="#696969") %>%
									set_palette(discrete_color = pal_color(c("red"))) %>% theme_axis("x", major_label_orientation = 45)
						}) 
					}
				}
				names(plotfigs) <- sort(adivADIV())
				if(length(adivADIV()) == 1) {
					grid_plot(plotfigs, ncol=1)
				} else {
					grid_plot(plotfigs, ncol=2)
				}
			} else {
				return(NULL)
			}
		} else {
			NULL
		}
	})

	########################################################################		
	## Create DataTable object with OTU alpha diversity data
	########################################################################	
	output$ADIVotuaovtableRENDER <- DT::renderDataTable({
		if(input$goADIV){
			if("OTU" %in% adivTAXA()){		
			DT_DAT <- phylo_TAX_aDIV_testDAT()[[which(names(phylo_TAX_aDIV_testDAT()) %in% "OTU")]]
			datatable(DT_DAT, extensions='Buttons', rownames = FALSE,				
						options = list(
							dom = 'lf<"floatright"B>rtip',
							buttons = c('excel', 'pdf', 'csv'),
							searching = TRUE,
							pageLength = 5,
							lengthMenu = c(5, nrow(DT_DAT))
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
	## Render UI OTU alpha diversity graphic/table
	########################################################################	
	output$ADIVotugraphicsRENDER <- renderUI({
		if(input$goADIV){
			if("OTU" %in% adivTAXA()){		
				
				list(
					fluidPage(
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity Graphics at <em>OTU</em> level.</strong></h4>
								 "
							),
							HTML(ADIVplottextRENDER()
							)
						),
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity", PHYLUMdatatypeRENDER()[[3]], "Results at <em>OTU</em> level.</strong></h4>
								 "
							)
						)	
					),
					fluidPage(
						column(6,
							rbokehOutput("OTUadivRbokeh_RENDER", height="200px", width="100px")
						),
						column(6,	
							HTML("
								 <p>Data are presented as ", PHYLUMdatatypeRENDER()[[1]], " (", PHYLUMdatatypeRENDER()[[2]],").  <em>P</em>-value(s) derived from", PHYLUMdatatypeRENDER()[[3]],".  </p>
								 "
							),			
							DT::dataTableOutput("ADIVotuaovtableRENDER", width = "100%", height = "800px"),
							br()
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
	## hide/show plot alpha diversity data download 
	########################################################################				
    shinyjs::onclick("toggleADIV_DataDownload",
        shinyjs::toggle(id = "ADIV_DataDownload", anim = TRUE)
	)  	

	########################################################################		
	## Download option for alpha diversity data download 
	########################################################################		
	output$ADIVData_download <- downloadHandler(
		
		filename = function() { 
			paste(input$ADIV_DataDownloadfilename, '.csv', sep='') 
		},
		content = function(file) {
			write.csv(phylo_TAX_aDIV_DATADOWN(), file)
		}
	)
		