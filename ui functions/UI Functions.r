

importTAB <- function() {

            tabPanel(title = "Import Data", id = "importdatpage",
				div(id = "text_import",
					checkboxInput("IMPORTbig", "Bigger text", FALSE),
					uiOutput("BIOMMETArowlengthmatch"),
					uiOutput("BIOMMETAnomatch"),
					# uiOutput("BIOMMETAnomatchingIDs"),
					uiOutput("BIOMMETAduplicateIDs"),
					uiOutput("BIOMMETAnumberstartgroup"),
					uiOutput("importDIFFBIOMMETA"),
					hr(),
					fluidPage(
						column(3,
							HTML("
							  <h4><strong>Import Biom Data:</strong></h4>
							 "
							),
							fileInput(inputId='biomINPUT', label='', accept=c('.biom')),
							HTML("
							  <p>Expect upwards of 20 second wait for files >10 MB.</p>
							 "
							)
						),
						column(3,
							HTML("
							  <h4><strong>Import Biom Metadata:</strong></h4>
							 "
							),
							fileInput(inputId='biommetaINPUT', label='', accept=c('.csv'))
						),
						column(3,
							HTML("
							  <h4><strong>Import TRE File (OPTIONAL):</strong></h4>
							 "
							),
							fileInput(inputId='treINPUT', label='', accept=c('.tre')),
							HTML("
							  <p>Expect upwards of 15 second wait for files >2 MB.</p>
							 "
							)
						
							
						),
						uiOutput("PHYLOSEQloadexampleRENDER")

					),
					hr(),

					# fluidPage(
						# column(12,
							# verbatimTextOutput("importTEXT")
						# )
					# ),

					
					uiOutput("PHYLOSEQtabledescTEXT"),
					uiOutput("PHYLOSEQrawoutputRENDER"),
					uiOutput("PHYLOSEQrawfilterRENDER"),
					uiOutput("PHYLOSEQfiltereddataRENDER")

					


					
				)
			)


}

adivTAB <- function() {
			tabPanel(title = "Alpha-Diversity", id = "adivpage",
				div(id = "text_adiv",
					checkboxInput("aDIVbig", "Bigger text", FALSE),
					HTML("
						<h4><strong><em>Select the desired inputs and press the \"Finalize &#945;-Diversity\" button.</em></strong></h4>
						"
					),
					# fluidPage(
						# column(12,
							# verbatimTextOutput("adivTEXT")						
						# )
					# ),					
					
					fluidPage(
						column(3,
							HTML("
							  <h4><strong>Select Taxonomic Level(s):</strong></h4>
							 "
							),	 
							selectizeInput("adivTAXAselect", label="", selected="Phylum",
								choices=c(taxaL, "OTU"), options=list(placeholder = 'Click box to select parameters'), 
								multiple=TRUE),
							HTML("
							  <p>Selecting multiple taxonomic levels will require longer computation time.</p>
							 "
							)
						),
						column(3,
							HTML("
							  <h4><strong>Select &#945;-Diversity Parameter(s):</strong></h4>
							 "
							),	 
							selectizeInput("adivADIVselect", label="", selected="Observed",
								choices=adivL, options = list(minItems = 1), 
								multiple=TRUE
							)
						),
						column(3,
							uiOutput("ADIVgroupselectRENDER")
						),
						column(3,
							actionButton("goADIV", label=HTML("Finalize &#945;-Diversity"),
							# "Finalize Alpha-Diversity", 
							icon("bicycle"), 
								style="color: #fff; background-color: #2c2cad; border-color: #000")
							
						)
					),
					uiOutput("ADIVdownloadRENDER"),
					
					uiOutput("ADIVphylumgraphicsRENDER"),
					uiOutput("ADIVclassgraphicsRENDER"),
					uiOutput("ADIVordergraphicsRENDER"),
					uiOutput("ADIVfamilygraphicsRENDER"),
					uiOutput("ADIVgenusgraphicsRENDER"),
					uiOutput("ADIVotugraphicsRENDER")
											
				)
			)

}

bdivTAB <- function() {
			tabPanel(title = "Beta-Diversity", id = "adivpage",
				div(id = "text_bdiv",
					checkboxInput("bDIVbig", "Bigger text", FALSE),
					HTML("
						<h4><strong><em>Select the desired inputs and press the \"Finalize &#946;-Diversity\" button.</em></strong></h4>
						"
					),
					fluidPage(
						column(3,
							HTML("
							  <h4><strong>Select Taxonomic Level(s):</strong></h4>
							 "
							),	 
							br(),
							selectizeInput("bdivTAXAselect", label="", selected="Phylum",
								choices=c(taxaL, "OTU"), options=list(placeholder = 'Click box to select parameters'), 
								multiple=TRUE),
							HTML("
							  <p>Selecting multiple taxonomic levels will require longer computation time.</p>
							 "
							)
						),
						column(3,
							uiOutput("BDIVgroupselectRENDER")
						),
						column(3,
							HTML("
								<p id=\"filterheader\"><strong>Choose number of permuations for PERMANOVA</strong></p>
								"
							),
							selectInput(inputId="BDIVpermcut", label="", 
								choices=seq(100, 1000, 100), selected=500)							
						),
						column(3,
							actionButton("goBDIV", label=HTML("Finalize &#946;-Diversity"),
							icon("bicycle"), 
								style="color: #fff; background-color: #2c2cad; border-color: #000")
							
						)
					),
					hr(),
					# fluidPage(
						# column(12,
							# verbatimTextOutput("bdivTEXT")						
						# )
					# ),						
					
					uiOutput("BDIVordinateselectRENDER"),
					uiOutput("BDIVplottableRENDER")
					
				)
			)

}

rabundTAB <- function() {
			tabPanel(title = "Differential-Abundance", id = "adivpage",
				div(id = "text_dabund",
					checkboxInput("dABUNDbig", "Bigger text", FALSE),	
				
					fluidPage(
						column(3,
							HTML("
							  <h4><strong>Select the desired inputs and press the \"Select TAXA\" button.</strong></h4>
							 "
							),	 
							selectizeInput("dabundTAXAselect", label="", selected="Phylum",
								choices=c(taxaL, "OTU"), options=list(placeholder = 'Click box to select parameters'), 
								multiple=TRUE),
							HTML("
								<p>Selecting multiple taxonomic levels will require longer computation time.</p>
								"
							)
						),
						column(3,
							HTML("
								<h4><strong>Select Test for Differential Abundance Analyses:</strong></h4>
								"
							),			

							selectInput(inputId="dabundNBMtest", label="", choices=c("Wald Test" = "Wald", "Likelihood Ratio Test" = "LRT"), 
								selected="Wald")
						),
						column(2,
							actionButton("goDABUND", label=HTML("Select Taxa"),
							icon("bicycle"), 
								style="color: #fff; background-color: #2c2cad; border-color: #000")
						),
						# column(3,
							# uiOutput("DABUNDtestselectRENDER")					
						# ),
						column(3,
							uiOutput("DABUNDgroupselectRENDER"),
							uiOutput("DABUNDpaircompRENDER")
						)
						
						
					),
					# hr(),
					# fluidPage(
						# column(12,
							# verbatimTextOutput("dabundTEXT")						
						# )
					# ),
					uiOutput("DABUNDphylumBPoptionsRENDER"),
					uiOutput("DABUNDclassBPoptionsRENDER"),
					uiOutput("DABUNDorderBPoptionsRENDER"),
					uiOutput("DABUNDfamilyBPoptionsRENDER"),
					uiOutput("DABUNDgenusBPoptionsRENDER"),
					uiOutput("DABUNDotuBPoptionsRENDER")			
				)
			)

}