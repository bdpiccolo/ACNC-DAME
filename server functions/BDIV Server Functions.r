
	########################################################################	
	## disable beta-diversity tab on page load
	########################################################################
    js$disableTab("Beta-Diversity")

	########################################################################		
	## Open beta-diversity tab when input$goIMPORT is observed
	########################################################################	
    observeEvent(input$goIMPORT, {
		# enable Beta-Diversity when clicking the button
		js$enableTab("Beta-Diversity")
		# switch to Beta-Diversity
		updateTabsetPanel(session, "adivpage", "Beta-Diversity")
    })



	######################################################################
	## Toggle regular/larger text 
	########################################################################		
	observe({
		shinyjs::toggleClass("text_bdiv", "big", input$bDIVbig)
    })

	########################################################################	
	## Render Taxa Selections Experimental Groups options
	########################################################################				
	output$BDIVtaxaselectRENDER <- renderUI({
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
				selectizeInput("bdivTAXAselect", label="", selected="Phylum",
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
				selectizeInput("bdivTAXAselect", label="", selected="Phylum",
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
		
	######################################################################
	## Render beta-diversity indices
	########################################################################	
	output$BDIVindexRENDER <- renderUI({	
		if(is.null(TRE_DAT())) {
			
				selectInput("bdivINDEXselect", label="", choices = list(
					"Dissimilarity Based Indices" = Dissimilarity,
					"Distance Based Indices" = Distance
					), 
					selectize = FALSE
				)
		} else {

				selectInput("bdivINDEXselect", label="", choices = list(
					"Phylogenetic Tree Based Indices" = phylotreeDIST,
					"Dissimilarity Based Indices" = Dissimilarity,
					"Distance Based Indices" = Distance
					), 
					selectize = FALSE
				)

			
		}
	})

	######################################################################
	## Render beta diversity text
	########################################################################	
	output$BDIVbdivindicetextRENDER <- renderUI({
		req(phyloseqFINAL())	
		if(input$bdivINDEXselect %in% phylotreeDIST){
			list(
				
				HTML("
					<p>&#946;-diversity is an estimate of biodiversity, also referred to as between sample diversity. Dissimilarity and distance indices 
					are commonly used to estimate &#946;-diversity.  Phylogenetic (Unifrac and unweighted uUnifrac) and dissimilarity (Bray-Curtis and Jaccard) 
					based measurements are commonly used in microbial analyses. More in depth review can be found 
					<a href=\"http://www.sciencedirect.com/science/article/pii/S0092867414008642?via%3Dihub\" target=\"_blank\">here</a> and 
					<a href=\"https://academic.oup.com/femsre/article/32/4/557/1812950\" target=\"_blank\">here</a>.</p>
					"
				),
				HTML("
					<p>Tree based &#946;-diversity parameters, especially DPCOA, require longer computational time.</p>
					"
				)
			)
		} else {
				
				HTML("
					<p>&#946;-diversity is an estimate of biodiversity, also referred to as between sample diversity. Dissimilarity and distance indices 
					are commonly used to estimate &#946;-diversity.  Dissimilarity based measurements (Bray-Curtis and Jaccard) are commonly used in microbial analyses. More in depth review can be found 
					<a href=\"http://www.sciencedirect.com/science/article/pii/S0092867414008642?via%3Dihub\" target=\"_blank\">here</a>.</p>
					"
				)
		}
	})

	######################################################################
	## Render ordination text
	########################################################################	
	output$BDIVbdivordinationtextRENDER <- renderUI({
		req(phyloseqFINAL())	
		HTML("
			<p>Ordinations are data reduction techniques that can summarize the inherent variance found within a dataset.  The results can then be projected into
			a two dimensional plot where similar samples will cluster together and dissimilar samples will distance themselves from one another.  Principal Co-ordinate Analysis (PCoA),
			and Non-multi Dimensional Scaling (NMDS) are commonly used in microbial analyses. More in depth review can be found 
			<a href=\"http://www.sciencedirect.com/science/article/pii/S0092867414008642?via%3Dihub\" target=\"_blank\">here.</p>
			"
		)
			
	})

	######################################################################
	## Render Experimental Groups for PERMANOVA
	########################################################################		
	output$BDIVgroupselectRENDER <- renderUI({
		req(phyloseqFINAL())	
		list(
			HTML("
				<h4><strong>Select Group(s) for PERMANOVA Analyses:</strong></h4>
				"
			),	
			selectizeInput("bdivGROUPselect", label="", selected=CompGroups()$Groups[1],
				choices=CompGroups()$Groups, options = list(minItems = 1, maxItems = 2), 
					multiple=TRUE),
			HTML("
				<p>Selecting groups to be assessed by PERMANOVA.  PERMANOVA stands for PERMutational Multivariate Analysis of Variance 
				and is similar to ANOVA, but assess group differences on distance or dissimilarity matrices rather than group averages.</p>
				"
			)
		)
	})

	######################################################################
	## Isolate TAXA selections 
	########################################################################		
	BDIVtaxa <- reactive({
		if(input$goBDIV){
			isolate({
				c(taxaL,"OTU")[which(c(taxaL,"OTU") %in% input$bdivTAXAselect)]
			})
		} else {
			NULL
		}
	})

	######################################################################
	## Isolate Experimental Group selections 
	########################################################################		
	BDIVgroup <- reactive({
		if(input$goBDIV){
			isolate({
				input$bdivGROUPselect
			})
		} else {
			NULL
		}
	})

	########################################################################
	## Agglomerate reads according to TAXA selections
	########################################################################				
	bdiversity <- reactive({
		if(input$goBDIV){
			isolate({
				
				BDIVtaxa <- BDIVtaxa()				
				## OTU does not require to be agglomerated 
				## Needs to be handled by itself if selected
				if("OTU" %in% BDIVtaxa){
					## If only OTU is selected then move object into a list, name list element, and return
					if(setequal(BDIVtaxa == "OTU", TRUE)) {
						phylo_TAX_LIST <- list("OTU" = phyloseqFINAL())
						names(phylo_TAX_LIST) <- BDIVtaxa()
						phylo_TAX_LIST
					} else {
						## Run phyloseq::tax_glom() function on selected TAXA
						## using pblapply() to render status boxes
						BDIVtaxa <- BDIVtaxa[!(BDIVtaxa %in% "OTU")]
						withProgress(message = "Agglomerating Taxa", value=0, { 
							percentage <- 0 
							phylo_TAX_LIST <- pblapply(BDIVtaxa, function(x) {
								Sys.sleep(0.05); 
								percentage <<- percentage + 1/length(BDIVtaxa())*100
								incProgress(1/length(BDIVtaxa()), detail = paste0("Progress: ", round(percentage,2),"%"))
								tax_glom(phyloseqFINAL(), x)
							})
						})
						phylo_TAX_LIST[[length(phylo_TAX_LIST) + 1]] <- phyloseqFINAL()
						names(phylo_TAX_LIST) <- BDIVtaxa()
						phylo_TAX_LIST
					}
				} else {
				## If OTU is NOT selected then run phyloseq::tax_glom() function on selected TAXA
				## using pblapply() to render status boxes
					BDIVtaxa <- BDIVtaxa
					withProgress(message = "Agglomerating Taxa", value=0, { 
						percentage <- 0 
						phylo_TAX_LIST <- pblapply(BDIVtaxa, function(x) {
							Sys.sleep(0.05); 
							percentage <<- percentage + 1/length(BDIVtaxa())*100
							incProgress(1/length(BDIVtaxa()), detail = paste0("Progress: ", round(percentage,2),"%"))
							
							tax_glom(phyloseqFINAL(), x)
						})						
					})
					## Set list element names and return
					names(phylo_TAX_LIST) <- BDIVtaxa()
					phylo_TAX_LIST
				}
				
			})
		} else {
			NULL
		}
	})

	########################################################################
	## Calculate Ordinations
	########################################################################	
	ordinationDF <- reactive({
		
		## Select TAXA
		ordDF <- pblapply(BDIVtaxa(), function(x) {
			## Ordinate phyloseq object with method and distance selections
			Ord <- ordinate(bdiversity()[[x]], 
				method = input$bdivORDINATEselect,
				distance = input$bdivINDEXselect
			)
			## Extract first 2 components based on ordination method and add SAMPLE data
			Ord_DF <- switch(input$bdivORDINATEselect,
				"DCA" = data.frame(sample_data(phyloseqFINAL()),Ord$rproj[,1:2]),
				"CCA" = data.frame(sample_data(phyloseqFINAL()),Ord$CA$u[,1:2]),
				"NMDS" = data.frame(sample_data(phyloseqFINAL()),Ord$points[,1:2]),
				"MDS" = data.frame(sample_data(phyloseqFINAL()),Ord$vectors[,1:2]),
				"PCoA" = data.frame(sample_data(phyloseqFINAL()),Ord$vectors[,1:2]),
				"RDA" = data.frame(sample_data(phyloseqFINAL()),Ord$CA$u[,1:2]),
				"DPCoA" = data.frame(sample_data(phyloseqFINAL()),Ord$li[,1:2])
			)
			## Make similar column names and return
			colnames(Ord_DF)[!(colnames(Ord_DF) %in% colnames(sample_data(phyloseqFINAL())))] <- 
				c("Component1", "Component2")
			Ord_DF
		})
		## Set names and return
		names(ordDF) <- BDIVtaxa()
		ordDF		
	})
	

	########################################################################
	## Create ScatterD3 object with Phylum Ordination data
	########################################################################
	output$PHYLUMbdivSCATD3_RENDER <- renderScatterD3({
		if(input$goBDIV){
			if("Phylum" %in% BDIVtaxa()){	
				scatD3_DF <- ordinationDF()[[which(names(ordinationDF()) %in% "Phylum")]]
				scatD3_DF$IDs <- rownames(scatD3_DF)
				col_var <- if (input$bdiv_PCA_col == "None") {
					NULL 
				} else {
					scatD3_DF[,input$bdiv_PCA_col]
				}
				symbol_var <- if (input$bdiv_PCA_shape == "None") {
					NULL 
				} else {
					scatD3_DF[,input$bdiv_PCA_shape]
				}				
				lab_var <- if (input$bdiv_PCA_labels == "FALSE") {
					NULL 
				} else {
					scatD3_DF[,input$bdiv_PCA_labeltype]
				}
				scatterD3(x = scatD3_DF[,"Component1"],
						  y = scatD3_DF[,"Component2"],
						  lab = lab_var,
						  xlab = "Component1",
						  ylab = "Component2",
						  col_var = col_var,
						  col_lab = input$bdiv_PCA_col,
						  point_size = input$bdiv_PCA_size,
						  ellipses = input$bdiv_PCA_ellipses,
						  symbol_var = symbol_var,
						  symbol_lab = input$bdiv_PCA_shape,
						  ####size_var = input$bdiv_PCA_size,
						  ####size_lab = input$bdiv_PCA_size,
						  ####key_var = rownames(scatterD3_DAT()),
						  point_opacity = input$bdiv_PCA_opacity,
						  labels_size = input$bdiv_PCA_labsize,
						  transitions = input$bdiv_PCA_transitions,
						  dom_id_svg_export = "PHYLUMscatD3export",
						  dom_id_reset_zoom = "PHYLUMscatD3resetzoom",
						  # dom_id_lasso_toggle = "PHYLUMscatD3lassotoggle"
						  ####lasso = TRUE,
						  ####lasso_callback = "function(sel) {alert(sel.data().map(function(d) {return d.lab}).join('\\n'));}"
				 )
			} else {
				NULL
			}
		} else {
			NULL
		}
	})

	########################################################################
	## Create ScatterD3 object with Class Ordination data
	########################################################################	
	output$CLASSbdivSCATD3_RENDER <- renderScatterD3({
		if(input$goBDIV){
			if("Class" %in% BDIVtaxa()){	
				scatD3_DF <- ordinationDF()[[which(names(ordinationDF()) %in% "Class")]]
				scatD3_DF$IDs <- rownames(scatD3_DF)
				col_var <- if (input$bdiv_PCA_col == "None") {
					NULL 
				} else {
					scatD3_DF[,input$bdiv_PCA_col]
				}
				symbol_var <- if (input$bdiv_PCA_shape == "None") {
					NULL 
				} else {
					scatD3_DF[,input$bdiv_PCA_shape]
				}			
				lab_var <- if (input$bdiv_PCA_labels == "FALSE") {
					NULL 
				} else {
					scatD3_DF[,input$bdiv_PCA_labeltype]
				}
				scatterD3(x = scatD3_DF[,"Component1"],
						  y = scatD3_DF[,"Component2"],
						  lab = lab_var,
						  xlab = "Component1",
						  ylab = "Component2",
						  col_var = col_var,
						  col_lab = input$bdiv_PCA_col,
						  point_size = input$bdiv_PCA_size,
						  ellipses = input$bdiv_PCA_ellipses,
						  symbol_var = symbol_var,
						  symbol_lab = input$bdiv_PCA_shape,
						  # size_var = input$bdiv_PCA_size,
						  # size_lab = input$bdiv_PCA_size,
						  # key_var = rownames(scatterD3_DAT()),
						  point_opacity = input$bdiv_PCA_opacity,
						  labels_size = input$bdiv_PCA_labsize,
						  transitions = input$bdiv_PCA_transitions,
						  dom_id_svg_export = "CLASSscatD3export",
						  dom_id_reset_zoom = "CLASSscatD3resetzoom"
						  # dom_id_lasso_toggle = "CLASSscatD3lassotoggle"
						  # lasso = TRUE,
						  # lasso_callback = "function(sel) {alert(sel.data().map(function(d) {return d.lab}).join('\\n'));}"
				 )
			} else {
				NULL
			}
		} else {
			NULL
		}
	})

	########################################################################
	## Create ScatterD3 object with Order Ordination data
	########################################################################		
	output$ORDERbdivSCATD3_RENDER <- renderScatterD3({
		if(input$goBDIV){
			if("Order" %in% BDIVtaxa()){	
				scatD3_DF <- ordinationDF()[[which(names(ordinationDF()) %in% "Order")]]
				scatD3_DF$IDs <- rownames(scatD3_DF)
				col_var <- if (input$bdiv_PCA_col == "None") {
					NULL 
				} else {
					scatD3_DF[,input$bdiv_PCA_col]
				}
				symbol_var <- if (input$bdiv_PCA_shape == "None") {
					NULL 
				} else {
					scatD3_DF[,input$bdiv_PCA_shape]
				}			
				lab_var <- if (input$bdiv_PCA_labels == "FALSE") {
					NULL 
				} else {
					scatD3_DF[,input$bdiv_PCA_labeltype]
				}
				scatterD3(x = scatD3_DF[,"Component1"],
						  y = scatD3_DF[,"Component2"],
						  lab = lab_var,
						  xlab = "Component1",
						  ylab = "Component2",
						  col_var = col_var,
						  col_lab = input$bdiv_PCA_col,
						  point_size = input$bdiv_PCA_size,
						  ellipses = input$bdiv_PCA_ellipses,
						  symbol_var = symbol_var,
						  symbol_lab = input$bdiv_PCA_shape,
						  # size_var = input$bdiv_PCA_size,
						  # size_lab = input$bdiv_PCA_size,
						  # key_var = rownames(scatterD3_DAT()),
						  point_opacity = input$bdiv_PCA_opacity,
						  labels_size = input$bdiv_PCA_labsize,
						  transitions = input$bdiv_PCA_transitions,
						  dom_id_svg_export = "ORDERscatD3export",
						  dom_id_reset_zoom = "ORDERscatD3resetzoom"
						  # dom_id_lasso_toggle = "ORDERscatD3lassotoggle"
						  # lasso = TRUE,
						  # lasso_callback = "function(sel) {alert(sel.data().map(function(d) {return d.lab}).join('\\n'));}"
				 )
			} else {
				NULL
			}
		} else {
			NULL
		}
	})

	########################################################################
	## Create ScatterD3 object with Family Ordination data
	########################################################################	
	output$FAMILYbdivSCATD3_RENDER <- renderScatterD3({
		if(input$goBDIV){
			if("Family" %in% BDIVtaxa()){	
				scatD3_DF <- ordinationDF()[[which(names(ordinationDF()) %in% "Family")]]
				scatD3_DF$IDs <- rownames(scatD3_DF)
				col_var <- if (input$bdiv_PCA_col == "None") {
					NULL 
				} else {
					scatD3_DF[,input$bdiv_PCA_col]
				}
				symbol_var <- if (input$bdiv_PCA_shape == "None") {
					NULL 
				} else {
					scatD3_DF[,input$bdiv_PCA_shape]
				}			
				lab_var <- if (input$bdiv_PCA_labels == "FALSE") {
					NULL 
				} else {
					scatD3_DF[,input$bdiv_PCA_labeltype]
				}
				scatterD3(x = scatD3_DF[,"Component1"],
						  y = scatD3_DF[,"Component2"],
						  lab = lab_var,
						  xlab = "Component1",
						  ylab = "Component2",
						  col_var = col_var,
						  col_lab = input$bdiv_PCA_col,
						  point_size = input$bdiv_PCA_size,
						  ellipses = input$bdiv_PCA_ellipses,
						  symbol_var = symbol_var,
						  symbol_lab = input$bdiv_PCA_shape,
						  # size_var = input$bdiv_PCA_size,
						  # size_lab = input$bdiv_PCA_size,
						  # key_var = rownames(scatterD3_DAT()),
						  point_opacity = input$bdiv_PCA_opacity,
						  labels_size = input$bdiv_PCA_labsize,
						  transitions = input$bdiv_PCA_transitions,
						  dom_id_svg_export = "FAMILYscatD3export",
						  dom_id_reset_zoom = "FAMILYscatD3resetzoom"
						  # dom_id_lasso_toggle = "FAMILYscatD3lassotoggle"
						  # lasso = TRUE,
						  # lasso_callback = "function(sel) {alert(sel.data().map(function(d) {return d.lab}).join('\\n'));}"
				 )
			} else {
				NULL
			}
		} else {
			NULL
		}
	})

	########################################################################
	## Create ScatterD3 object with Genus Ordination data
	########################################################################	
	output$GENUSbdivSCATD3_RENDER <- renderScatterD3({
		if(input$goBDIV){
			if("Genus" %in% BDIVtaxa()){	
				scatD3_DF <- ordinationDF()[[which(names(ordinationDF()) %in% "Genus")]]
				scatD3_DF$IDs <- rownames(scatD3_DF)
				col_var <- if (input$bdiv_PCA_col == "None") {
					NULL 
				} else {
					scatD3_DF[,input$bdiv_PCA_col]
				}
				symbol_var <- if (input$bdiv_PCA_shape == "None") {
					NULL 
				} else {
					scatD3_DF[,input$bdiv_PCA_shape]
				}			
				lab_var <- if (input$bdiv_PCA_labels == "FALSE") {
					NULL 
				} else {
					scatD3_DF[,input$bdiv_PCA_labeltype]
				}
				scatterD3(x = scatD3_DF[,"Component1"],
						  y = scatD3_DF[,"Component2"],
						  lab = lab_var,
						  xlab = "Component1",
						  ylab = "Component2",
						  col_var = col_var,
						  col_lab = input$bdiv_PCA_col,
						  point_size = input$bdiv_PCA_size,
						  ellipses = input$bdiv_PCA_ellipses,
						  symbol_var = symbol_var,
						  symbol_lab = input$bdiv_PCA_shape,
						  # size_var = input$bdiv_PCA_size,
						  # size_lab = input$bdiv_PCA_size,
						  # key_var = rownames(scatterD3_DAT()),
						  point_opacity = input$bdiv_PCA_opacity,
						  labels_size = input$bdiv_PCA_labsize,
						  transitions = input$bdiv_PCA_transitions,
						  dom_id_svg_export = "GENUSscatD3export",
						  dom_id_reset_zoom = "GENUSscatD3resetzoom"
						  # dom_id_lasso_toggle = "GENUSscatD3lassotoggle"
						  # lasso = TRUE,
						  # lasso_callback = "function(sel) {alert(sel.data().map(function(d) {return d.lab}).join('\\n'));}"
				 )
			} else {
				NULL
			}
		} else {
			NULL
		}
	})

	########################################################################
	## Create ScatterD3 object with OTU Ordination data
	########################################################################		
	output$OTUbdivSCATD3_RENDER <- renderScatterD3({
		if(input$goBDIV){
			if("OTU" %in% BDIVtaxa()){	
				scatD3_DF <- ordinationDF()[[which(names(ordinationDF()) %in% "OTU")]]
				scatD3_DF$IDs <- rownames(scatD3_DF)
				if (input$bdiv_PCA_col == "None") {
					col_var <- NULL 
				} else {
					col_var <- scatD3_DF[,input$bdiv_PCA_col]
				}
				if (input$bdiv_PCA_shape == "None") {
					symbol_var <- NULL 
				} else {
					symbol_var <- scatD3_DF[,input$bdiv_PCA_shape]
				}			
				lab_var <- if (input$bdiv_PCA_labels == "FALSE") {
					NULL 
				} else {
					scatD3_DF[,input$bdiv_PCA_labeltype]
				}
				scatterD3(x = scatD3_DF[,"Component1"],
						  y = scatD3_DF[,"Component2"],
						  lab = lab_var,
						  xlab = "Component1",
						  ylab = "Component2",
						  col_var = col_var,
						  col_lab = input$bdiv_PCA_col,
						  point_size = input$bdiv_PCA_size,
						  ellipses = input$bdiv_PCA_ellipses,
						  symbol_var = symbol_var,
						  symbol_lab = input$bdiv_PCA_shape,
						  # size_var = input$bdiv_PCA_size,
						  # size_lab = input$bdiv_PCA_size,
						  # key_var = rownames(scatterD3_DAT()),
						  point_opacity = input$bdiv_PCA_opacity,
						  labels_size = input$bdiv_PCA_labsize,
						  transitions = input$bdiv_PCA_transitions,
						  dom_id_svg_export = "OTUscatD3export",
						  dom_id_reset_zoom = "OTUscatD3resetzoom"
						  # dom_id_lasso_toggle = "GENUSscatD3lassotoggle"
						  # lasso = TRUE,
						  # lasso_callback = "function(sel) {alert(sel.data().map(function(d) {return d.lab}).join('\\n'));}"
				 )
			} else {
				NULL
			}
		} else {
			NULL
		}
	})

	########################################################################
	## Toggle hidden advanced ScatterD3 options
	########################################################################			
    shinyjs::onclick("toggleBDIV_SCATD3_Advanced",
        shinyjs::toggle(id = "BDIV_SCATD3advanced", anim = TRUE)
	)  	

	########################################################################
	## Calculate distance matrix 
	########################################################################		
	bdiversity_dist <- reactive({
		req(bdiversity())
		bdiv <- pblapply(BDIVtaxa(), function(x) {
			## Select phyloseq object by TAXA and use phyloseq::distance function to calculate distance
			pob <- bdiversity()[[x]] 
			phyloseq::distance(pob, input$bdivINDEXselect)
		})
		names(bdiv) <- BDIVtaxa()
		bdiv		
	})
	
	# output$bdivTEXT <- renderPrint({
	
	# })	
	
	########################################################################
	## Calculate PERMANOVA
	########################################################################			
	bdiversity_PERMANOVA <- reactive({
		req(bdiversity_dist())
		bpermA <- pblapply(BDIVtaxa(), function(x) {
			## Select phyloseq and distance objects by TAXA
			pob <- bdiversity()[[x]]
			pdist <- bdiversity_dist()[[x]]
			## Extract SAMPLE data
			sampleDF <- as(sample_data(pob), "data.frame")		
			## If 1 Experimental Group
			if(length(BDIVgroup()) == 1) {
				## Extract Group, and run PERMANOVA
				BDIVdf <- data.frame(list(sampleDF[,BDIVgroup()]))
				colnames(BDIVdf) <- "A"
				ppermA <- adonis2(pdist ~ A, data=BDIVdf, permutations=as.numeric(input$BDIVpermcut), by="margin")
				# ppermA <- adonis(pdist ~ A, data=BDIVdf, permutations=as.numeric(input$BDIVpermcut))
			} else {
				## If 2 Experimental Groups
				## Extract both groups separately, and run PERMANOVA
				BDIVdf <- sampleDF[,BDIVgroup()[c(1,2)]]
				colnames(BDIVdf) <- c("A", "B")
				ppermA <- adonis2(pdist ~ A + B, data=BDIVdf, permutations=as.numeric(input$BDIVpermcut), by="margin")
				# ppermA <- adonis(pdist ~ A + B, data=BDIVdf, permutations=as.numeric(input$BDIVpermcut))
			}
			ppermA
		})	
		## Set names and return
		names(bpermA) <- BDIVtaxa()
		bpermA
	})

	########################################################################
	## Set PERMANOVA results into data frame
	########################################################################	
	bdiversity_PERMANOVAtable <- reactive({
		req(bdiversity_PERMANOVA())
		
		perm <- pblapply(BDIVtaxa(), function(x) {
			## Select PERMANOVA results by TAXA, extract results, make column of row names,
			## round numeric results to 5 decimal places, reset Parameter column and return
			permres <- bdiversity_PERMANOVA()[[x]]
			data.frame(Parameter = c(BDIVgroup(), "Residuals", "Total"), 
					DF = permres$Df,
					SS = round(permres$SumOfSqs, 5),
					F = round(permres$F, 5),
					P = round(permres[,"Pr(>F)"], 5)
			)
		})
		## Set names and return
		names(perm) <- BDIVtaxa()
		perm
		
	})	

	########################################################################
	## Create DataTable object with Phylum PERMANOVA results
	########################################################################	
	output$BDIVphylumPERMANOVAtableRENDER <- DT::renderDataTable({
		if(input$goBDIV){
			t1 <- as.data.frame( bdiversity_PERMANOVAtable()[[which(names(bdiversity_PERMANOVAtable()) %in% "Phylum")]])
				t1[,!(colnames(t1) %in% "Parameter")] <- 
					apply(t1[,!(colnames(t1) %in% "Parameter")], 2, as.numeric)
				datatable(t1, extensions='Buttons', rownames = FALSE,
					container = htmltools::withTags(table(
						class = 'display',
							thead(
							tr(
								th(colspan = 1, 'Parameter'),
								th(colspan = 1, 'Degrees of Freedom'),
								th(colspan = 1, 'Sequential Sums of Squares'),
								th(colspan = 1, 'F Statistic'),
								th(colspan = 1, 'P')
							)
						)
					)
				),
					options = list(
						dom = 'lf<"floatright"B>rtip',
						buttons = c('excel', 'pdf', 'csv'),
						searching = TRUE,
						pageLength = 5,
						lengthMenu = c(5, nrow(t1))
					)
				) 
	
		}				
	})	


	########################################################################
	## Create DataTable object with Class PERMANOVA results
	########################################################################		
	output$BDIVclassPERMANOVAtableRENDER <- DT::renderDataTable({
		if(input$goBDIV){
			t1 <- as.data.frame( bdiversity_PERMANOVAtable()[[which(names(bdiversity_PERMANOVAtable()) %in% "Class")]])
				t1[,!(colnames(t1) %in% "Parameter")] <- 
					apply(t1[,!(colnames(t1) %in% "Parameter")], 2, as.numeric)
				datatable(t1, extensions='Buttons', rownames = FALSE,
					container = htmltools::withTags(table(
						class = 'display',
							thead(
							tr(
								th(colspan = 1, 'Parameter'),
								th(colspan = 1, 'Degrees of Freedom'),
								th(colspan = 1, 'Sequential Sums of Squares'),
								th(colspan = 1, 'F Statistic'),
								th(colspan = 1, 'P')
							)
						)
					)
				),
					options = list(
						dom = 'lf<"floatright"B>rtip',
						buttons = c('excel', 'pdf', 'csv'),
						searching = TRUE,
						pageLength = 5,
						lengthMenu = c(5, nrow(t1))
					)
				) 
	
		}				
	})	


	########################################################################
	## Create DataTable object with Order PERMANOVA results
	########################################################################			
	output$BDIVorderPERMANOVAtableRENDER <- DT::renderDataTable({
		if(input$goBDIV){
			t1 <- as.data.frame( bdiversity_PERMANOVAtable()[[which(names(bdiversity_PERMANOVAtable()) %in% "Order")]])
				t1[,!(colnames(t1) %in% "Parameter")] <- 
					apply(t1[,!(colnames(t1) %in% "Parameter")], 2, as.numeric)
				datatable(t1, extensions='Buttons', rownames = FALSE,
					container = htmltools::withTags(table(
						class = 'display',
							thead(
							tr(
								th(colspan = 1, 'Parameter'),
								th(colspan = 1, 'Degrees of Freedom'),
								th(colspan = 1, 'Sequential Sums of Squares'),
								th(colspan = 1, 'F Statistic'),
								th(colspan = 1, 'P')
							)
						)
					)
				),
					options = list(
						dom = 'lf<"floatright"B>rtip',
						buttons = c('excel', 'pdf', 'csv'),
						searching = TRUE,
						pageLength = 5,
						lengthMenu = c(5, nrow(t1))
					)
				) 
	
		}				
	})	
	
	########################################################################
	## Create DataTable object with Family PERMANOVA results
	########################################################################				
	output$BDIVfamilyPERMANOVAtableRENDER <- DT::renderDataTable({
		if(input$goBDIV){
			t1 <- as.data.frame( bdiversity_PERMANOVAtable()[[which(names(bdiversity_PERMANOVAtable()) %in% "Family")]])
				t1[,!(colnames(t1) %in% "Parameter")] <- 
					apply(t1[,!(colnames(t1) %in% "Parameter")], 2, as.numeric)
				datatable(t1, extensions='Buttons', rownames = FALSE,
					container = htmltools::withTags(table(
						class = 'display',
							thead(
							tr(
								th(colspan = 1, 'Parameter'),
								th(colspan = 1, 'Degrees of Freedom'),
								th(colspan = 1, 'Sequential Sums of Squares'),
								th(colspan = 1, 'F Statistic'),
								th(colspan = 1, 'P')
							)
						)
					)
				),
					options = list(
						dom = 'lf<"floatright"B>rtip',
						buttons = c('excel', 'pdf', 'csv'),
						searching = TRUE,
						pageLength = 5,
						lengthMenu = c(5, nrow(t1))
					)
				) 
	
		}				
	})	

	########################################################################
	## Create DataTable object with Genus PERMANOVA results
	########################################################################				
	output$BDIVgenusPERMANOVAtableRENDER <- DT::renderDataTable({
		if(input$goBDIV){
			t1 <- as.data.frame( bdiversity_PERMANOVAtable()[[which(names(bdiversity_PERMANOVAtable()) %in% "Genus")]])
				t1[,!(colnames(t1) %in% "Parameter")] <- 
					apply(t1[,!(colnames(t1) %in% "Parameter")], 2, as.numeric)
				datatable(t1, extensions='Buttons', rownames = FALSE,
					container = htmltools::withTags(table(
						class = 'display',
							thead(
							tr(
								th(colspan = 1, 'Parameter'),
								th(colspan = 1, 'Degrees of Freedom'),
								th(colspan = 1, 'Sequential Sums of Squares'),
								th(colspan = 1, 'F Statistic'),
								th(colspan = 1, 'P')
							)
						)
					)
				),
					options = list(
						dom = 'lf<"floatright"B>rtip',
						buttons = c('excel', 'pdf', 'csv'),
						searching = TRUE,
						pageLength = 5,
						lengthMenu = c(5, nrow(t1))
					)
				) 
	
		}				
	})	

	########################################################################
	## Create DataTable object with OTU PERMANOVA results
	########################################################################	
	output$BDIVotuPERMANOVAtableRENDER <- DT::renderDataTable({
		if(input$goBDIV){
			t1 <- as.data.frame( bdiversity_PERMANOVAtable()[[which(names(bdiversity_PERMANOVAtable()) %in% "OTU")]])
				t1[,!(colnames(t1) %in% "Parameter")] <- 
					apply(t1[,!(colnames(t1) %in% "Parameter")], 2, as.numeric)
				datatable(t1, extensions='Buttons', rownames = FALSE,
					container = htmltools::withTags(table(
						class = 'display',
							thead(
							tr(
								th(colspan = 1, 'Parameter'),
								th(colspan = 1, 'Degrees of Freedom'),
								th(colspan = 1, 'Sequential Sums of Squares'),
								th(colspan = 1, 'F Statistic'),
								th(colspan = 1, 'P')
							)
						)
					)
				),
					options = list(
						dom = 'lf<"floatright"B>rtip',
						buttons = c('excel', 'pdf', 'csv'),
						searching = TRUE,
						pageLength = 5,
						lengthMenu = c(5, nrow(t1))
					)
				) 
	
		}				
	})	

	########################################################################
	## Render Phylum UI after goBDIV observed
	########################################################################
	output$BDIVphylumgraphicsRENDER <- renderUI({
		if(input$goBDIV){
			if("Phylum" %in% BDIVtaxa()){	
				list(
					hr(class="hr_SINGLE_sep"),
					fluidPage(
						column(6,
							HTML("
								<h4><strong>&#946;-Diversity Graphics at <em>Phylum</em> level.</strong></h4>
								"
							),
							scatterD3::scatterD3Output("PHYLUMbdivSCATD3_RENDER", height="600px", width="750px"),
							tags$p(
								actionButton("PHYLUMscatD3resetzoom", "Reset Zoom", icon("icon-zoom-in"), 
										style="color: #fff; background-color: #2c2cad; border-color: #000"),
								# actionButton("PHYLUMscatD3lassotoggle", "Toggle Lasso", icon("crosshairs"), 
										# style="color: #fff; background-color: #2c2cad; border-color: #000"),	
								tags$a(id = "PHYLUMscatD3export", href = "#", class = "btn btn-default", 
									style="color: #fff; background-color: #2c2cad; border-color: #000;",
									HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download SVG")
								)
							)
						),
						column(6,
							HTML("
								<h4><strong>&#946;-Diversity PERMANOVA at <em>Phylum</em> level.</strong></h4>
								"
							),						
							DT::dataTableOutput("BDIVphylumPERMANOVAtableRENDER")
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
	## Render Class UI after goBDIV observed
	########################################################################		
	output$BDIVclassgraphicsRENDER <- renderUI({
		if(input$goBDIV){
			if("Class" %in% BDIVtaxa()){	
				list(
					hr(class="hr_SINGLE_sep"),
					fluidPage(
						column(6,
							HTML("
								<h4><strong>&#946;-Diversity Graphics at <em>Class</em> level.</strong></h4>
								"
							),
							scatterD3::scatterD3Output("CLASSbdivSCATD3_RENDER", height="600px", width="750px"),
							tags$p(
								actionButton("CLASSscatD3resetzoom", "Reset Zoom", icon("icon-zoom-in"), 
										style="color: #fff; background-color: #2c2cad; border-color: #000"),
								# actionButton("CLASSscatD3lassotoggle", "Toggle Lasso", icon("crosshairs"), 
										# style="color: #fff; background-color: #2c2cad; border-color: #000"),	
								tags$a(id = "CLASSscatD3export", href = "#", class = "btn btn-default", 
									style="color: #fff; background-color: #2c2cad; border-color: #000",
									HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download SVG")
								)
							)
						),
						column(6,
							HTML("
								<h4><strong>&#946;-Diversity PERMANOVA at <em>Class</em> level.</strong></h4>
								"
							),						
							DT::dataTableOutput("BDIVclassPERMANOVAtableRENDER")
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
	## Render Order UI after goBDIV observed
	########################################################################		
	output$BDIVordergraphicsRENDER <- renderUI({
		if(input$goBDIV){
			if("Order" %in% BDIVtaxa()){	
				list(
					hr(class="hr_SINGLE_sep"),
					fluidPage(
						column(6,
							HTML("
								<h4><strong>&#946;-Diversity Graphics at <em>Order</em> level.</strong></h4>
								"
							),
							scatterD3::scatterD3Output("ORDERbdivSCATD3_RENDER", height="500px", width="750px"),
							tags$p(
								actionButton("ORDERscatD3resetzoom", "Reset Zoom", icon("icon-zoom-in"), 
										style="color: #fff; background-color: #2c2cad; border-color: #000"),
								# actionButton("ORDERscatD3lassotoggle", "Toggle Lasso", icon("crosshairs"), 
										# style="color: #fff; background-color: #2c2cad; border-color: #000"),	
								tags$a(id = "ORDERscatD3export", href = "#", class = "btn btn-default", 
									style="color: #fff; background-color: #2c2cad; border-color: #000",
									HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download SVG")
								)
							)
						),
						column(6,
							HTML("
								<h4><strong>&#946;-Diversity PERMANOVA at <em>Order</em> level.</strong></h4>
								"
							),						
							DT::dataTableOutput("BDIVorderPERMANOVAtableRENDER")
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
	## Render Family UI after goBDIV observed
	########################################################################			
	output$BDIVfamilygraphicsRENDER <- renderUI({
		if(input$goBDIV){
			if("Family" %in% BDIVtaxa()){	
				list(
					hr(class="hr_SINGLE_sep"),
					fluidPage(
						column(6,
							HTML("
								<h4><strong>&#946;-Diversity Graphics at <em>Family</em> level.</strong></h4>
								"
							),
							scatterD3::scatterD3Output("FAMILYbdivSCATD3_RENDER", height="500px", width="750px"),
							tags$p(
								actionButton("FAMILYscatD3resetzoom", "Reset Zoom", icon("icon-zoom-in"), 
										style="color: #fff; background-color: #2c2cad; border-color: #000"),
								# actionButton("FAMILYscatD3lassotoggle", "Toggle Lasso", icon("crosshairs"), 
										# style="color: #fff; background-color: #2c2cad; border-color: #000"),	
								tags$a(id = "FAMILYscatD3export", href = "#", class = "btn btn-default", 
									style="color: #fff; background-color: #2c2cad; border-color: #000",
									HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download SVG")
								)
							)
						),
						column(6,
							HTML("
								<h4><strong>&#946;-Diversity PERMANOVA at <em>Family</em> level.</strong></h4>
								"
							),						
							DT::dataTableOutput("BDIVfamilyPERMANOVAtableRENDER")
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
	## Render Genus UI after goBDIV observed
	########################################################################					
	output$BDIVgenusgraphicsRENDER <- renderUI({
		if(input$goBDIV){
			if("Genus" %in% BDIVtaxa()){	
				list(
					hr(class="hr_SINGLE_sep"),
					fluidPage(
						column(6,
							HTML("
								<h4><strong>&#946;-Diversity Graphics at <em>Genus</em> level.</strong></h4>
								"
							),
							scatterD3::scatterD3Output("GENUSbdivSCATD3_RENDER", height="500px", width="750px"),
							tags$p(
								actionButton("GENUSscatD3resetzoom", "Reset Zoom", icon("icon-zoom-in"), 
										style="color: #fff; background-color: #2c2cad; border-color: #000"),
								# actionButton("GENUSscatD3lassotoggle", "Toggle Lasso", icon("crosshairs"), 
										# style="color: #fff; background-color: #2c2cad; border-color: #000"),	
								tags$a(id = "GENUSscatD3export", href = "#", class = "btn btn-default", 
									style="color: #fff; background-color: #2c2cad; border-color: #000",
									HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download SVG")
								)
							)
						),
						column(6,
							HTML("
								<h4><strong>&#946;-Diversity PERMANOVA at <em>Genus</em> level.</strong></h4>
								"
							),						
							DT::dataTableOutput("BDIVgenusPERMANOVAtableRENDER")
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
	## Render OTU UI after goBDIV observed
	########################################################################						
	output$BDIVotugraphicsRENDER <- renderUI({
		if(input$goBDIV){
			if("OTU" %in% BDIVtaxa()){	
				list(
					hr(class="hr_SINGLE_sep"),

					fluidPage(
						column(6,
							HTML("
								<h4><strong>&#946;-Diversity Graphics at <em>OTU</em> level.</strong></h4>
								"
							),
							scatterD3::scatterD3Output("OTUbdivSCATD3_RENDER", height="500px", width="750px"),
							tags$p(
								actionButton("OTUscatD3resetzoom", "Reset Zoom", icon("icon-zoom-in"), 
										style="color: #fff; background-color: #2c2cad; border-color: #000"),
								# actionButton("OTUscatD3lassotoggle", "Toggle Lasso", icon("crosshairs"), 
										# style="color: #fff; background-color: #2c2cad; border-color: #000"),	
								tags$a(id = "OTUscatD3export", href = "#", class = "btn btn-default", 
									style="color: #fff; background-color: #2c2cad; border-color: #000",
									HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download SVG")
								)
							)
						),
						column(6,
							HTML("
								<h4><strong>&#946;-Diversity PERMANOVA at <em>OTU</em> level.</strong></h4>
								"
							),						
							DT::dataTableOutput("BDIVotuPERMANOVAtableRENDER")
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
	## Render Ordination UI options after goBDIV observed
	########################################################################	
	output$BDIVordinateselectRENDER <- renderUI({
		list(
				
				HTML("
					<h3><strong><em>Select the desired graphical inputs. The \"Finalize &#946;-Diversity\" does NOT 
					have to be pressed to update graphical inputs.</em></strong></h3>
					"
				),
				fluidPage(
					column(3, 
						HTML("
							  <h4><strong>Select &#946;-Diversity Parameter(s):</strong></h4>
							 "
						),	 
						uiOutput("BDIVindexRENDER"),					
						uiOutput("BDIVbdivindicetextRENDER")	
					),
					column(3, 
						HTML("
							<h4><strong>Select Ordination Method:</strong></h4>
							"
						),	
						{
							if(is.null(TRE_DAT())) {
								selectInput("bdivORDINATEselect", label="", selected=Ordinate[1],
									choices=Ordinate)
							} else {
								selectInput("bdivORDINATEselect", label="", selected=Ordinate[1],
									choices=c(Ordinate,TreeOrdinate))
							}
						},
						
						uiOutput("BDIVbdivordinationtextRENDER")	
					),
					column(3,
						HTML("
							<h4><strong>Color Mapping Variable:</strong></h4>
							"
						),		
						selectInput("bdiv_PCA_col", label="", choices = c("None", CompGroups()$Groups),
							selected = CompGroups()$Groups[1])
					),
					column(3,
						HTML("
							<h4><strong>Shape Mapping Variable:</strong></h4>
							"
						),
						{
							if(length(BDIVgroup()) == 1) {	
								selectInput("bdiv_PCA_shape", label="", choices = c("None", CompGroups()$Groups),
									selected = "None")
							} else {
								selectInput("bdiv_PCA_shape", label="", choices = CompGroups()$Groups,
									selected = CompGroups()$Groups[2])
							}
						}
						
					)
				),
				fluidPage(
					column(3,
						HTML("
							<h4><strong>Display/Toggle Confidence Ellipses:</strong></h4>
							"
						),	
						checkboxInput("bdiv_PCA_ellipses", label="Display", value = TRUE)
					)
				)
			)
			
		# }
	})	

	########################################################################
	## Render plotting UI after goBDIV observed
	########################################################################			
	output$BDIVplottableRENDER <- renderUI({
		if(input$goBDIV){
			list(
				fluidPage(
					a(id = "toggleBDIV_SCATD3_Advanced", "Show/hide advanced ScatterD3 options", href = "#"),
					p(),
					shinyjs::hidden(
						div(id = "BDIV_SCATD3advanced",	
							column(3, 
								HTML("
									<h4><strong>Size Mapping Variable:</strong></h4>
									"
								),
								sliderInput("bdiv_PCA_size", "", min=0, max=500, value=100, step=20)
							),
							column(3, 
								HTML("
									<h4><strong>Points Opacity:</strong></h4>
									"
								),
								sliderInput("bdiv_PCA_opacity", label="", min = 0, max = 1, value = 1, step = 0.05)
							),
							column(3, 
								HTML("
									<h4><strong>Add Labels:</strong></h4>
									"
								),
								checkboxInput("bdiv_PCA_labels", "Label Points", value = FALSE),
								selectInput(inputId="bdiv_PCA_labeltype", label = HTML("<h4><strong>Labels:</strong></h4>"), 
										choices = c("IDs", CompGroups()$Groups), selected = "IDs"),
								sliderInput("bdiv_PCA_labsize", HTML("<h4><strong>Label Size:</strong></h4>"), min = 5, max = 25, value = 11)		
							),
							column(3, 
								HTML("
									<h4><strong>Use Transitions:</strong></h4>
									"
								),
								checkboxInput("bdiv_PCA_transitions", "Smooth transitions", value = TRUE)
							)
						)
					)
				),

				hr(),

				uiOutput("BDIVphylumgraphicsRENDER"), 
				uiOutput("BDIVclassgraphicsRENDER"), 
				uiOutput("BDIVordergraphicsRENDER"), 
				uiOutput("BDIVfamilygraphicsRENDER"), 
				uiOutput("BDIVgenusgraphicsRENDER"),
				uiOutput("BDIVotugraphicsRENDER")
			)
		}
	})
