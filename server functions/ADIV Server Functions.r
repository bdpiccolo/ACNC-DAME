
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
			uiOutput("ADIVanovatypeRENDER")
		)
	})
	
	########################################################################	
	## Render ANOVA options if Experimental Groups > 1
	########################################################################	
	output$ADIVanovatypeRENDER <- renderUI({
		req(phyloseqFINAL())	
		if(length(input$adivGROUPselect) > 1) {
			list(
				HTML("
					<h4><strong>Select Type of ANOVA:</strong></h4>
					"
				),	
				selectInput(inputId="adivANOVAtype", label = "", 
					choices = c("1-way", "Multi-factor"), selected = "1-way"),
				HTML("
					<h4><strong>Dispersion:</strong></h4>
					"
				),	
				selectInput(inputId="adivDISPERSIONtype", label = "", 
					choices = c("Standard Error Mean" = "SEM", "Standard Deviation"= "SD"), selected = "SEM")
			)
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
								 <h4><strong><em>Options to download sample-wise a-Diversity data and/or plotting data.</em></strong></h4>
								"
							)
						)
					),
					fluidPage(
						column(3,

							a(id = "toggleADIV_PlotDownload", "Show/hide Plotting Data Downloading Options", href = "#"),
							p(),
							shinyjs::hidden(
								div(id = "ADIV_PlotDownload",	
									textInput("ADIV_PlotDownloadfilename", label = "Choose File Name", value = "Plotting Data"),
									tags$p(
										tags$a(id = "ADIVPlottingData_download", href = "#", class = "shiny-download-link", target="_blank",
											style="color: #fff; background-color: #2c2cad; border-color: #000; padding-top: 8px;
											padding-right: 8px; padding-bottom: 8px; padding-left: 8px; border-radius: 5px;",
											HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download Plotting Data")
										)
									)	
								)
							)
						),
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
					),
					hr(),
					fluidPage(
						column(3,
							a(id = "toggleADIV_HC_Advanced", "Show/hide advanced highcharter options", href = "#"),
							p(),
							shinyjs::hidden(
								div(id = "ADIV_HCadvanced",	
									selectInput("type", label = "Type", 
										choices = c("Vertical" = "line", "Horizontal" = "bar")
									), 
											
									selectInput("theme", label = "Theme", selected="default",
										choices = c("Default" = "default", "Fivethirtyeight" = "fivethirtyeight", "Economist" = "economist",
											"Dark Unica" = "darkunica", "Grid Light" = "gridlight", "Sand Signika" = "sandsignika",
										    "Hand Drawn" = "handdrwran", "Chalk" = "chalk", "NULL" = "null")
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
	adivANOVA <- reactive({
		if(input$goADIV){
			isolate({
				input$adivANOVAtype
			})
		} else {
			NULL
		}
	})
	
	########################################################################	
	## Isolate dispersion selection 
	########################################################################					
	adivDISPERSION <- reactive({
		if(input$goADIV){
			isolate({
				input$adivDISPERSIONtype
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
	phylo_TAX_aDIVaovTEST <- reactive({	
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
						if(is.null(adivANOVA())){							
							## Melt data based on Experimental group and alpha diversity measurements and
							## run ANOVA
							ERDfmelt <- as_tibble(melt(ERDf[,c(adivGROUP(),adivADIV())]))
							aovP <- ERDfmelt %>% group_by(variable) %>% 
								do(aovresults = aov(formula(paste("value ~ ", paste(adivGROUP(), collapse=" * "))), data=.))
						} else {
							## If 1-way ANOVA type selected
							if(length(adivGROUP()) == 1) {							
							## Melt data based on Experimental group and alpha diversity measurements and
							## run ANOVA 
								ERDfmelt <- as_tibble(melt(ERDf[,c(adivGROUP(),adivADIV())]))
								aovP <- ERDfmelt %>% group_by(variable) %>% 
								do(aovresults = aov(formula(paste("value ~ ", paste(adivGROUP(), collapse=" * "))), data=.))
							} else {
								## If 1-way ANOVA type selected	and there are > 2 groups						
								if(adivANOVA() == "1-way"){
									## Concatenate Experimental groups
									ERDf$ANOVAP <- factor(apply(ERDf[, which(colnames(ERDf) %in% adivGROUP())], 1, paste, collapse="_"))
								} else {
									## Factor ANOVA and keep groups the same
									ERDf
								}
								phyDATmelt <- as_tibble(melt(ERDf))
								## If 1-way ANOVA type selected	and there are > 2 groups	
								if(adivANOVA() == "1-way"){
									## run ANOVA with concatenated groups
									aovP <- phyDATmelt %>% group_by(variable) %>% 
										do(aovresults = aov(formula("value ~ ANOVAP"), data=.))
								} else {
									## run ANOVA with unaltered groups
									aovP <- phyDATmelt %>% group_by(variable) %>% 
										do(aovresults = aov(formula(paste("value ~ ", paste(adivGROUP(), collapse=" * "))), data=.))
								}
							}
						}
						
						aovP
					})
				})
				## set names and return
				names(phylo_TAX_aDIVaovTEST) <- adivTAXA()	
				phylo_TAX_aDIVaovTEST
	})			

	########################################################################
	## Calculate ANOVA results for data download
	########################################################################		
	phylo_TAX_aDIVaovDOWN <- reactive({
		req(pyloseqTAXA)			
		req(phylo_TAX_aDIVaovTEST())				
		withProgress(message = "Running ANOVA(s)", value=0, { 				
			percentage2 <- 0 
			phylo_TAX_aDIVaovDOWN <- pblapply(adivTAXA(), function(x) {
				Sys.sleep(0.05); 
				percentage2 <<- percentage2 + 1/length(adivTAXA())*100
				incProgress(1/length(adivTAXA()), detail = paste0("Progress: ", round(percentage2,2),"%"))
				## Select ANOVA results by TAXA						
				aovP <- phylo_TAX_aDIVaovTEST()[[x]]
				## Extract result table amd set grpi[
				aovpL <- apply(aovP, 1, function(x) summary(x[[2]]))
				names(aovpL) <- as.character(aovP$variable)
				names(aovpL) <- aovP$variable
				## If only one Experimental Group is selected
				if(length(adivGROUP()) ==1) {
					## format 1-way anova results and remove Residuals row.
					aovpvec <- sapply(adivADIV(), function(x) {
						datf <- aovpL[[x]][[1]]
						pval <- round(datf[!(grepl("Residuals", rownames(datf))),"Pr(>F)"],5)
					})
					## Combine group names and pvalues
					aovpdf <- data.frame(variable=names(aovpvec), P=aovpvec)
				} else {
				## If > 1 groups are selected
					aovpdftall <- do.call("rbind", 
						## Format data, remove residuals, combine group names, pvalue
						lapply(adivADIV(), function(x) {
							datf <- aovpL[[x]][[1]]					
							pval <- round(datf[!(grepl("Residuals", rownames(datf))),"Pr(>F)"],5)
							aovpdf <- data.frame(adiv=rownames(datf)[!(grepl("Residuals", rownames(datf)))], P=pval, variable=rep(x, length(pval)))
						})
					)
					## reformat to wide format and remove excess spaces in strings
					aovpdf <- dcast(aovpdftall, variable ~ adiv, value.var="P")
					colnames(aovpdf) <- gsub(" ", "", colnames(aovpdf))
				}
				aovpdf		
			})						
					
		})
		## Set names and return
		names(phylo_TAX_aDIVaovDOWN) <- adivTAXA()	
		phylo_TAX_aDIVaovDOWN
	})

	########################################################################
	## Set Mean and SD/SEM data frame
	########################################################################	
	phylo_TAX_aDIV_MEANSDSEM <- reactive({
		req(pyloseqTAXA)
		req(phylo_TAX_aDIV())			
		withProgress(message = "Setting Plot Data", value=0, { 	
			percentage3 <- 0 
			phylo_TAX_aDIV_MEANSDSEM <- pblapply(adivTAXA(), function(x) {
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
				MEANSEMtall <- as.data.frame(ERDfmelt %>% group_by_(.dots=dots) %>% 
					summarise(MEAN=mean(value), SD=sd(value), SEM=sd(value)/sqrt(length(value))))
				## Reformat to tall foramt, change 2nd 'variable' column to 'stat', round to the 1st decimal, and return
				MEANSEMtallmelt <- melt(MEANSEMtall)
				colnames(MEANSEMtallmelt)[which(colnames(MEANSEMtallmelt) %in% "variable")[2]] <- "stat"
				MEANSEMtallmelt$value <- round(MEANSEMtallmelt$value,1)
				MEANSEMtallmelt
			})
		})
		## set names and return
		names(phylo_TAX_aDIV_MEANSDSEM) <- adivTAXA()	
		phylo_TAX_aDIV_MEANSDSEM
	})			

	########################################################################
	## Set plotting data for alpha diversity
	########################################################################		
	phylo_TAX_aDIV_plotDAT <- reactive({
		req(pyloseqTAXA)		
		req(phylo_TAX_aDIV_MEANSDSEM())
		withProgress(message = "Setting Plot Data", value=0, { 	
			percentage4 <- 0 	
			phylo_TAX_aDIV_plotDAT <- pblapply(adivTAXA(), function(x) {
				Sys.sleep(0.05); 
				percentage4 <<- percentage4 + 1/length(adivTAXA())*100
				incProgress(1/length(adivTAXA()), detail = paste0("Progress: ", round(percentage4,2),"%"))
				## Select alpha diversity mean/sd/sem return by TAXA						
				MEANSEMtallmelt <- phylo_TAX_aDIV_MEANSDSEM()[[x]]
				## subset mean
				MEANtallmelt <- MEANSEMtallmelt[MEANSEMtallmelt$stat %in% "MEAN",]
				## Reformat to wide data based on how many experimental groups selected	
				if(length(adivGROUP()) ==1) {
					MEANwide <- dcast(MEANtallmelt, paste("variable + stat ~ ", adivGROUP()))
				} else {
					MEANwide <- dcast(MEANtallmelt, paste("variable + stat ~ ", paste(adivGROUP(), collapse=" + ")))

				}
				MEANwide		
			})
		})
		## Set names and return
		names(phylo_TAX_aDIV_plotDAT) <- adivTAXA()
		phylo_TAX_aDIV_plotDAT
	})			

	########################################################################
	## Merge Alpha diversity MEAN/SD/SEM return with ANOVA returns
	########################################################################	
	phylo_TAX_aDIV_MERGE <- reactive({
		req(pyloseqTAXA)		
		req(phylo_TAX_aDIVaovDOWN())
		req(phylo_TAX_aDIV_MEANSDSEM())		
		withProgress(message = "Merging Data", value=0, { 	
			percentage5 <- 0 	
			phylo_TAX_aDIV_MERGE <- pblapply(adivTAXA(), function(x) {
				Sys.sleep(0.05); 
				percentage5 <<- percentage5 + 1/length(adivTAXA())*100
				incProgress(1/length(adivTAXA()), detail = paste0("Progress: ", round(percentage5,2),"%"))
				## Select alpha diversity mean/sd/sem return by TAXA						
				MEANSEMtallmelt <- phylo_TAX_aDIV_MEANSDSEM()[[x]]
				## Reformat to wide format
				MEANSEMwide <- dcast(MEANSEMtallmelt, paste("variable ~ ", paste(c(adivGROUP(),"stat"), collapse=" + ")))
				## Merge with ANOVA result by TAXA
				FINALDAT <- merge(MEANSEMwide, phylo_TAX_aDIVaovDOWN()[[x]], by="variable")
				## Add TAXA column and return
				FINALDAT$TAXA <- rep(x, nrow(FINALDAT))
				FINALDAT
			})
		})
		## If only one TAXA selected return data, if >1 then bind and return		
		if(length(adivTAXA()) == 1) {		
			phylo_TAX_aDIV_MERGE
		} else {
			phylo_TAX_aDIV_MERGE <- do.call("rbind", phylo_TAX_aDIV_MERGE)
		}
		phylo_TAX_aDIV_MERGE
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
			
	# output$adivTEXT <- renderPrint({
		
	# })	

	########################################################################
	## Set ANOVA data for DataTable
	########################################################################						
	phylo_TAX_aDIV_aovDAT <- reactive({
		req(pyloseqTAXA)
		req(phylo_TAX_aDIVaovDOWN())
		req(phylo_TAX_aDIV_MEANSDSEM())		
		withProgress(message = "Merging Data", value=0, { 	
			percentage7 <- 0 	
			phylo_TAX_aDIV_aovDAT <- pblapply(adivTAXA(), function(x) {
				Sys.sleep(0.05); 
				percentage7 <<- percentage7 + 1/length(adivTAXA())*100
				incProgress(1/length(adivTAXA()), detail = paste0("Progress: ", round(percentage7,2),"%"))
				## Select alpha diversity mean/sd/sem return by TAXA
				MEANSEMtallmelt <- phylo_TAX_aDIV_MEANSDSEM()[[x]]
				## If no selection for sd/sem
				if(is.null(adivDISPERSION())){		
					## Return data with SEM
					MEANtallmelt <- MEANSEMtallmelt[MEANSEMtallmelt$stat %in% c("MEAN","SEM"),]
				} else {
					## Return data with selection
					MEANtallmelt <- MEANSEMtallmelt[MEANSEMtallmelt$stat %in% c("MEAN",adivDISPERSION()),]
				}
				## formula for data transformation is different 
				## Based on number of experimental groups selected
				if(length(adivGROUP()) ==1) {
					MEANwide <- dcast(MEANtallmelt, paste("variable + stat ~ ", adivGROUP()))
				} else {
					MEANwide <- dcast(MEANtallmelt, paste("variable + stat ~ ", paste(adivGROUP(), collapse=" + ")))
				}
				## melt results, change 2nd 'variable' column to 'group' column, and format back to wide format
				MEANwidemelt <- melt(MEANwide)
				colnames(MEANwidemelt)[which(colnames(MEANwidemelt) %in% "variable")[2]] <- "Group"
				MeanW <- dcast(MEANwidemelt, variable ~ Group + stat)
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
				# MeanW2 <- MeanW
				## coerce mean/dispersion DF into list 
				MeanW2 <- as.list(MeanW)
				for(i in (1:length(yvars)) + ncol(MeanW)) {
					## Add list elements to the end of the new list 		
					MeanW2[[i]] <- rep(NA, nrow(MeanW))
					MeanW2
				}
				## If spaces in factor levels
				if(grepl(" ", yvars)){
					## Remove spaces from list names
					names(MeanW2) <- gsub(" ", "", names(MeanW2))
					## Remove spaces from factor name vector
					yvarsnopace <- gsub(" ", "", yvars)
				} else {
					yvarsnopace <- yvars
				}
								
				## Set new list elements with experimental group names
				names(MeanW2)[(1:length(yvars)) + ncol(MeanW)] <- yvarsnopace
				## Coerce list back into a data frame
				MeanFINAL <- as.data.frame(MeanW2)
				## For each experimental group
				for(i in yvarsnopace) {
					## Determine the data frame column positions needed to extract mean and dispersion data
					## Based on groups
					colsLOGIC <- captureCN(colnames(MeanFINAL), i)
					## Remove data
					yvDF <- MeanFINAL[,colsLOGIC]
					## make object with mean data
					yvMEAN <- yvDF[,1]
					## make object with dispersion data
					yvSEM <- yvDF[,2]
					## paste mean and dispersion data so presented as MEAN (SEM)
					yvFinal <- paste0(yvMEAN, " (", yvSEM, ")")
					## Deposit results in new Experimental Group column
					MeanFINAL[,colsLOGIC][,3] <- yvFinal
				}
				## Subset variable and experimental group columns presented as 'MEAN (SEM)'
				## Leave MEAN and SEM data behind...
				MF <- MeanFINAL[,c("variable", yvarsnopace)]
				# MF
				## If spaces in factor levels
				if(grepl(" ", yvars)){
					# Replace column names without spaces to original
					colnames(MF)[!(colnames(MF) %in% "variable")] <- yvars
				} else {
					NULL
				}
				## merge mean/sem data with anova return by TAXA
				MFaov <- merge(MF, phylo_TAX_aDIVaovDOWN()[[x]], by="variable")
				MFaov
				
				
			})
		})	
		## set names and return
		names(phylo_TAX_aDIV_aovDAT) <- adivTAXA()
		phylo_TAX_aDIV_aovDAT
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
	## Create highcharter object with Phylum barplot data
	########################################################################	
	output$PHYLUMadivHC_RENDER <- renderHighchart({
		if(input$goADIV){
			if("Phylum" %in% adivTAXA()){	
				phylumDF <- melt(phylo_TAX_aDIV_plotDAT()[[which(names(phylo_TAX_aDIV_plotDAT()) %in% "Phylum")]])
				colnames(phylumDF)[which(colnames(phylumDF) %in% "variable")[2]] <- "groupvar"
				
				hc <- hchart(phylumDF, "column", hcaes(x = variable, y = value, group = groupvar)) %>% 

				  hc_legend(verticalAlign = "top") %>% 
				  hc_exporting(enabled = TRUE) %>% 
				  # hc_title(text = "Phylum",
						   # margin = 20, align = "left",
						   # style = list(color = "#000000", useHTML = TRUE)) %>% 
				  hc_xAxis(title = NULL) %>%
				  hc_yAxis(title = list(text = "Phylum alpha-Diversity")) %>%
				  hc_chart(type = input$type, zoomType = "xy")	
				if (input$theme != "default") {
					theme <- switch(input$theme,
							  null = hc_theme_null(),
							  darkunica = hc_theme_darkunica(),
							  gridlight = hc_theme_gridlight(),
							  sandsignika = hc_theme_sandsignika(),
							  fivethirtyeight = hc_theme_538(),
							  economist = hc_theme_economist(),
							  chalk = hc_theme_chalk(),
							  handdrwran = hc_theme_handdrawn()
					)			  
					hc <- hc %>% hc_add_theme(theme)		  
				}
				hc  
				
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
				DT_DAT <- phylo_TAX_aDIV_aovDAT()[[which(names(phylo_TAX_aDIV_aovDAT()) %in% "Phylum")]]
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

	########################################################################		
	## Render UI Phylum alpha diversity graphic/table
	########################################################################	
	output$ADIVphylumgraphicsRENDER <- renderUI({
		if(input$goADIV){
			if("Phylum" %in% adivTAXA()){		
				if(is.null(adivDISPERSION())) {
					disp <- "SEM"
				} else {
					disp <- adivDISPERSION()
				}	
				if(is.null(adivANOVA())) {
					ANOVA <- "1-way ANOVA"
				} else {
					if(adivANOVA() == "1-way"){
						ANOVA <- "1-way ANOVA"
					} else {
						ANOVA <- paste(length(adivGROUP()), "way ANOVA", sep="-")
					}
				}
				list(
					fluidPage(
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity Graphics at <em>Phylum</em> level.</strong></h4>
								 "
							)
						),
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity ANOVA Results at <em>Phylum</em> level.</strong></h4>
								 "
							)
						)
					),
					fluidPage(
						column(6,
							highchartOutput(outputId = "PHYLUMadivHC_RENDER", height="500px", width="600px")
						),
						column(6,						
							DT::dataTableOutput("ADIVphylumaovtableRENDER"),
							br(),
							HTML("
								 <p>Data are presented as MEAN (",disp,").  <em>P</em>-value(s) derived from", ANOVA,".  </p>
								 "
							)
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
	## Create highcharter object with Class barplot data
	########################################################################	
	output$CLASSadivHC_RENDER <- renderHighchart({
		if(input$goADIV){
			if("Class" %in% adivTAXA()){	
				phylumDF <- melt(phylo_TAX_aDIV_plotDAT()[[which(names(phylo_TAX_aDIV_plotDAT()) %in% "Class")]])
				colnames(phylumDF)[which(colnames(phylumDF) %in% "variable")[2]] <- "groupvar"
				hc <- hchart(phylumDF, "column", hcaes(x = variable, y = value, group = groupvar)) %>% 

				  hc_legend(verticalAlign = "top") %>% 
				  hc_exporting(enabled = TRUE) %>% 
				  # hc_title(text = "Class",
						   # margin = 20, align = "left",
						   # style = list(color = "#000000", useHTML = TRUE)) %>% 
				  hc_xAxis(title = NULL) %>%
				  hc_yAxis(title = list(text = "Class Alpha Diversity")) %>%
				  hc_chart(type = input$type, zoomType = "xy")		
				  
				if (input$theme != "default") {
					theme <- switch(input$theme,
							  null = hc_theme_null(),
							  darkunica = hc_theme_darkunica(),
							  gridlight = hc_theme_gridlight(),
							  sandsignika = hc_theme_sandsignika(),
							  fivethirtyeight = hc_theme_538(),
							  economist = hc_theme_economist(),
							  chalk = hc_theme_chalk(),
							  handdrwran = hc_theme_handdrawn()
					)			  
					hc <- hc %>% hc_add_theme(theme)		  
				}
				hc  
				
				
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
			DT_DAT <- phylo_TAX_aDIV_aovDAT()[[which(names(phylo_TAX_aDIV_aovDAT()) %in% "Class")]]
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
				if(is.null(adivDISPERSION())) {
					disp <- "SEM"
				} else {
					disp <- adivDISPERSION()
				}	
				if(is.null(adivANOVA())) {
					ANOVA <- "1-way ANOVA"
				} else {
					if(adivANOVA() == "1-way"){
						ANOVA <- "1-way ANOVA"
					} else {
						ANOVA <- paste(length(adivGROUP()), "way ANOVA", sep="-")
					}
				}			
				list(
					fluidPage(
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity Graphics at <em>Class</em> level.</strong></h4>
								 "
							)
						),
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity ANOVA Results at <em>Class</em> level.</strong></h4>
								 "
							)
						)					
					),
					fluidPage(
						column(6,
							highchartOutput(outputId = "CLASSadivHC_RENDER", height="500px", width="600px")
						),
						column(6,						
							DT::dataTableOutput("ADIVclassaovtableRENDER"),
							br(),
							HTML("
								 <p>Data are presented as MEAN (",disp,").  <em>P</em>-value(s) derived from", ANOVA,".  </p>
								 "
							)
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
	## Create highcharter object with Order barplot data
	########################################################################	
	output$ORDERadivHC_RENDER <- renderHighchart({
		if(input$goADIV){
			if("Order" %in% adivTAXA()){	
				phylumDF <- melt(phylo_TAX_aDIV_plotDAT()[[which(names(phylo_TAX_aDIV_plotDAT()) %in% "Order")]])
				colnames(phylumDF)[which(colnames(phylumDF) %in% "variable")[2]] <- "groupvar"
				hc <- hchart(phylumDF, "column", hcaes(x = variable, y = value, group = groupvar)) %>% 

				  hc_legend(verticalAlign = "top") %>% 
				  hc_exporting(enabled = TRUE) %>% 
				  # hc_title(text = "Order",
						   # margin = 20, align = "left",
						   # style = list(color = "#000000", useHTML = TRUE)) %>% 
				  hc_xAxis(title = NULL) %>%
				  hc_yAxis(title = list(text = "Order Alpha Diversity")) %>%
				  hc_chart(type = input$type, zoomType = "xy")	
				  
				if (input$theme != "default") {
					theme <- switch(input$theme,
							  null = hc_theme_null(),
							  darkunica = hc_theme_darkunica(),
							  gridlight = hc_theme_gridlight(),
							  sandsignika = hc_theme_sandsignika(),
							  fivethirtyeight = hc_theme_538(),
							  economist = hc_theme_economist(),
							  chalk = hc_theme_chalk(),
							  handdrwran = hc_theme_handdrawn()
					)			  
					hc <- hc %>% hc_add_theme(theme)		  
				}
				hc  
					
				
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
			DT_DAT <- phylo_TAX_aDIV_aovDAT()[[which(names(phylo_TAX_aDIV_aovDAT()) %in% "Order")]]
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
	## Render UI Order alpha diversity graphic/table
	########################################################################	
	output$ADIVordergraphicsRENDER <- renderUI({
		if(input$goADIV){
			if("Order" %in% adivTAXA()){		
				if(is.null(adivDISPERSION())) {
					disp <- "SEM"
				} else {
					disp <- adivDISPERSION()
				}	
				if(is.null(adivANOVA())) {
					ANOVA <- "1-way ANOVA"
				} else {
					if(adivANOVA() == "1-way"){
						ANOVA <- "1-way ANOVA"
					} else {
						ANOVA <- paste(length(adivGROUP()), "way ANOVA", sep="-")
					}
				}		
				list(
					fluidPage(
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity Graphics at <em>Order</em> level.</strong></h4>
								 "
							)
						),
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity ANOVA Results at <em>Order</em> level.</strong></h4>
								 "
							)
						)	
					),
					fluidPage(
						column(6,
							highchartOutput(outputId = "ORDERadivHC_RENDER", height="500px", width="600px")
						),
						column(6,						
							DT::dataTableOutput("ADIVorderaovtableRENDER"),
							br(),
							HTML("
								 <p>Data are presented as MEAN (",disp,").  <em>P</em>-value(s) derived from", ANOVA,".  </p>
								 "
							)
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
	## Create highcharter object with Family barplot data
	########################################################################		
	output$FAMILYadivHC_RENDER <- renderHighchart({
		if(input$goADIV){
			if("Family" %in% adivTAXA()){	
				phylumDF <- melt(phylo_TAX_aDIV_plotDAT()[[which(names(phylo_TAX_aDIV_plotDAT()) %in% "Family")]])
				colnames(phylumDF)[which(colnames(phylumDF) %in% "variable")[2]] <- "groupvar"
				hc <- hchart(phylumDF, "column", hcaes(x = variable, y = value, group = groupvar)) %>% 

				  hc_legend(verticalAlign = "top") %>% 
				  hc_exporting(enabled = TRUE) %>% 
				  # hc_title(text = "Family",
						   # margin = 20, align = "left",
						   # style = list(color = "#000000", useHTML = TRUE)) %>% 
				  hc_xAxis(title = NULL) %>%
				  hc_yAxis(title = list(text = "Family Alpha Diversity")) %>%
				  hc_chart(type = input$type, zoomType = "xy")	
				  
				if (input$theme != "default") {
					theme <- switch(input$theme,
							  null = hc_theme_null(),
							  darkunica = hc_theme_darkunica(),
							  gridlight = hc_theme_gridlight(),
							  sandsignika = hc_theme_sandsignika(),
							  fivethirtyeight = hc_theme_538(),
							  economist = hc_theme_economist(),
							  chalk = hc_theme_chalk(),
							  handdrwran = hc_theme_handdrawn()
					)			  
					hc <- hc %>% hc_add_theme(theme)		  
				}
				hc  
					
				
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
			DT_DAT <- phylo_TAX_aDIV_aovDAT()[[which(names(phylo_TAX_aDIV_aovDAT()) %in% "Family")]]
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
				if(is.null(adivDISPERSION())) {
					disp <- "SEM"
				} else {
					disp <- adivDISPERSION()
				}	
				if(is.null(adivANOVA())) {
					ANOVA <- "1-way ANOVA"
				} else {
					if(adivANOVA() == "1-way"){
						ANOVA <- "1-way ANOVA"
					} else {
						ANOVA <- paste(length(adivGROUP()), "way ANOVA", sep="-")
					}
				}			
				list(
					fluidPage(
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity Graphics at <em>Family</em> level.</strong></h4>
								 "
							)
						),
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity ANOVA Results at <em>Family</em> level.</strong></h4>
								 "
							)
						)	
					),
					fluidPage(
						column(6,
							highchartOutput(outputId = "FAMILYadivHC_RENDER", height="500px", width="600px")
						),
						column(6,						
							DT::dataTableOutput("ADIVfamilyaovtableRENDER"),
							br(),
							HTML("
								 <p>Data are presented as MEAN (",disp,").  <em>P</em>-value(s) derived from", ANOVA,".  </p>
								 "
							)
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
	## Create highcharter object with Genus barplot data
	########################################################################		
	output$GENUSadivHC_RENDER <- renderHighchart({
		if(input$goADIV){
			if("Genus" %in% adivTAXA()){	
				phylumDF <- melt(phylo_TAX_aDIV_plotDAT()[[which(names(phylo_TAX_aDIV_plotDAT()) %in% "Genus")]])
				colnames(phylumDF)[which(colnames(phylumDF) %in% "variable")[2]] <- "groupvar"
				
				
				hc<- hchart(phylumDF, "column", hcaes(x = variable, y = value, group = groupvar)) %>% 

				  hc_legend(verticalAlign = "top") %>% 
				  hc_exporting(enabled = TRUE) %>% 
				  # hc_title(text = "Genus",
						   # margin = 20, align = "left",
						   # style = list(color = "#000000", useHTML = TRUE)) %>% 
				  hc_xAxis(title = NULL) %>%
				  hc_yAxis(title = list(text = "Genus Alpha Diversity")) %>%
				  hc_chart(type = input$type, zoomType = "xy")	
				  
				if (input$theme != "default") {
					theme <- switch(input$theme,
							  null = hc_theme_null(),
							  darkunica = hc_theme_darkunica(),
							  gridlight = hc_theme_gridlight(),
							  sandsignika = hc_theme_sandsignika(),
							  fivethirtyeight = hc_theme_538(),
							  economist = hc_theme_economist(),
							  chalk = hc_theme_chalk(),
							  handdrwran = hc_theme_handdrawn()
					)			  
					hc <- hc %>% hc_add_theme(theme)		  
				}
				hc  
					
				
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
			DT_DAT <- phylo_TAX_aDIV_aovDAT()[[which(names(phylo_TAX_aDIV_aovDAT()) %in% "Genus")]]
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
				if(is.null(adivDISPERSION())) {
					disp <- "SEM"
				} else {
					disp <- adivDISPERSION()
				}	
				if(is.null(adivANOVA())) {
					ANOVA <- "1-way ANOVA"
				} else {
					if(adivANOVA() == "1-way"){
						ANOVA <- "1-way ANOVA"
					} else {
						ANOVA <- paste(length(adivGROUP()), "way ANOVA", sep="-")
					}
				}		
				list(
					fluidPage(
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity Graphics at <em>Genus</em> level.</strong></h4>
								 "
							)
						),
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity ANOVA Results at <em>Genus</em> level.</strong></h4>
								 "
							)
						)	
					),
					fluidPage(
						column(6,
							highchartOutput(outputId = "GENUSadivHC_RENDER", height="500px", width="600px")
						),
						column(6,						
							DT::dataTableOutput("ADIVgenusaovtableRENDER"),
							br(),
							HTML("
								 <p>Data are presented as MEAN (",disp,").  <em>P</em>-value(s) derived from", ANOVA,".  </p>
								 "
							)
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
	## Create highcharter object with OTU barplot data
	########################################################################	
	output$OTUadivHC_RENDER <- renderHighchart({
		if(input$goADIV){
			if("OTU" %in% adivTAXA()){	
				phylumDF <- melt(phylo_TAX_aDIV_plotDAT()[[which(names(phylo_TAX_aDIV_plotDAT()) %in% "OTU")]])
				colnames(phylumDF)[which(colnames(phylumDF) %in% "variable")[2]] <- "groupvar"
				
				
				hc<- hchart(phylumDF, "column", hcaes(x = variable, y = value, group = groupvar)) %>% 

				  hc_legend(verticalAlign = "top") %>% 
				  hc_exporting(enabled = TRUE) %>% 
				  # hc_title(text = "OTU",
						   # margin = 20, align = "left",
						   # style = list(color = "#000000", useHTML = TRUE)) %>% 
				  hc_xAxis(title = NULL) %>%
				  hc_yAxis(title = list(text = "OTU Level Alpha Diversity")) %>%
				  hc_chart(type = input$type, zoomType = "xy")	
				  
				if (input$theme != "default") {
					theme <- switch(input$theme,
							  null = hc_theme_null(),
							  darkunica = hc_theme_darkunica(),
							  gridlight = hc_theme_gridlight(),
							  sandsignika = hc_theme_sandsignika(),
							  fivethirtyeight = hc_theme_538(),
							  economist = hc_theme_economist(),
							  chalk = hc_theme_chalk(),
							  handdrwran = hc_theme_handdrawn()
					)			  
					hc <- hc %>% hc_add_theme(theme)		  
				}
				hc  
					
				
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
			DT_DAT <- phylo_TAX_aDIV_aovDAT()[[which(names(phylo_TAX_aDIV_aovDAT()) %in% "OTU")]]
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
				if(is.null(adivDISPERSION())) {
					disp <- "SEM"
				} else {
					disp <- adivDISPERSION()
				}	
				if(is.null(adivANOVA())) {
					ANOVA <- "1-way ANOVA"
				} else {
					if(adivANOVA() == "1-way"){
						ANOVA <- "1-way ANOVA"
					} else {
						ANOVA <- paste(length(adivGROUP()), "way ANOVA", sep="-")
					}
				}		
				list(
					fluidPage(
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity Graphics at <em>OTU</em> level.</strong></h4>
								 "
							)
						),
						column(6,
							hr(class="hr_SINGLE_sep"),
							HTML("
								 <h4><strong>&#945;-Diversity ANOVA Results at <em>OTU</em> level.</strong></h4>
								 "
							)
						)	
					),
					fluidPage(
						column(6,
							highchartOutput(outputId = "OTUadivHC_RENDER", height="500px", width="600px")
						),
						column(6,						
							DT::dataTableOutput("ADIVotuaovtableRENDER"),
							br(),
							HTML("
								 <p>Data are presented as MEAN (",disp,").  <em>P</em>-value(s) derived from", ANOVA,".  </p>
								 "
							)
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
	## hide/show plot data download options
	########################################################################		
    shinyjs::onclick("toggleADIV_PlotDownload",
        shinyjs::toggle(id = "ADIV_PlotDownload", anim = TRUE)
	)  	

	########################################################################		
	## Download option for plot data
	########################################################################			
	output$ADIVPlottingData_download <- downloadHandler(
		
		filename = function() { 
			paste(input$ADIV_PlotDownloadfilename, '.csv', sep='') 
		},
		content = function(file) {
			write.csv(phylo_TAX_aDIV_MERGE(), file)
		}
	)

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
		