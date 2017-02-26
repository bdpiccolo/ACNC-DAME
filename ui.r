library(scatterD3)
# library(rAmCharts)
library(DT)
library(highcharter)
library(shinyjs)
library(V8) 


jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

source("ui functions/UI Functions.r", local=TRUE)
tabPanelimport <- importTAB
tabPaneladiv <- adivTAB
tabPanelbdiv <- bdivTAB
tabPanelrabund <- rabundTAB

shinyUI(
	fluidPage(
		shinyjs::useShinyjs(),
		shinyjs::extendShinyjs(text = jscode),
        tags$head(
			tags$link(
				rel="stylesheet", type="text/css", href="custom.css"
			)
		), 
		navbarPage("DAME", id="navbar_title",
			tabPanel(title = "Introduction", id = "intropage",
				div(id = "text_intro",
					checkboxInput("INTRObig", "Bigger text", FALSE),
					fixedRow(
						HTML("
							  <center><h2><strong style=\"font: Impact, Charcoal, sans-serif;font-size: 150%;\">D</strong>yanmic 
							  <strong style=\"font: Impact, Charcoal, sans-serif;font-size: 150%;\">A</strong>ssessment
							  of <strong style=\"font: Impact, Charcoal, sans-serif;font-size: 150%;\">M</strong>icrobial 
							  <strong style=\"font: Impact, Charcoal, sans-serif;font-size: 150%;\">E</strong>cology 
							  <strong style=\"font: Impact, Charcoal, sans-serif;font-size: 150%;\">(DAME)</strong></h2></center>
							 "
						)
					),
					hr(),
					fluidPage(
						HTML("
							  <p>This app is an open source platform that uses the <a href=\"https://www.r-project.org/\" target=\"_blank\">R environment</a> 
							  to perform microbial ecology data analyses. It was specifically designed to work directly with output files from the 
							  <a href=\"http://qiime.org/\" target=\"_blank\">QIIME 1</a> with as minimal file processing as possible.</p>
							  <p></p>
							  <p> The current release (v0.1) assesses &#945;-Diversity and &#946;-Diversity measurements, and differential expression analyses 
							  of count data.  DAME requires the .BIOM file from QIIME and a .CSV file containing the .BIOM sample labels and metadata (experimental 
							  grouping data) associated with each sample.  This app utilizes the <a href=\"http://shiny.rstudio.com/\" target=\"_blank\">Shiny</a> 
							  framework to allow for dynamic and real-time interaction with virtually all aspects of the data workflow.  Where possible, table and 
							  graphic outputs utilize <a href=\"https://d3js.org/\" target=\"_blank\">D3</a> for a fully interactive experience.</p>
							 "
						)
					),
					hr(),
					fluidPage(
						column(6, 
							# HTML("
								# <h4><strong>Getting Started:</strong></h4>
								# <p>Requires two files, with an option for a .TRE file.</p>
								# <ol>
								  # <li>BIOM File - Typically found in XXXX folder from QIIME output</li>
								  # <li>BIOM Metadata File - .CSV file containing a column with exact samples labels used in QIIME analysis and experimental groupings</li>
								  # <li>TRE File (optional) - .TRE file, typically found in XXXX folder from QIIME output</li>
								# </ol>
								# <p>Files must .CSV at this time.</p>
								# <p>Examples of the spreadsheet format are shown to the right.</p>
								# "
								# <div style = \"background-color: #fcc; width:200%; height: 200px; border-radius: 3px;\">
								# </div>
							# ),
						 includeMarkdown("./Markdown/include.md")
						), 
						column(6,
							h4(strong("Example of BIOM Metadata")),
							# tags$div(img(src = "www/MetadataPic.png"))
							# tags$div(img(src = "www/ACNCLOGO.png"))
							HTML('	
								<p><p>
								<a href="" target="_blank">
								<img id="metaD" alt="metaD Excel" src="MetadataPic.png" width="700"/>
								</a>
								<p><p>
								'
							)
						)
					)
				)	
			),
			
			
			tabPanelimport(), 
			tabPaneladiv(),
			tabPanelbdiv(),
			tabPanelrabund(),	
			
			
			tabPanel(title = "About", id = "aboutpage",
				div(id = "text_about",
					checkboxInput("ABOUTbig", "Bigger text", FALSE),
					hr(),
				# p(id="about","About"),			
					HTML(
					'	
						<p><p>
						<a href="http://acnc.uamsweb.com/" target="_blank">
						<img id="logo" alt="ACNC Logo" src="ACNCLOGO.png" width="250"/>
						</a>
						<p><p>
						<a href="https://cran.r-project.org/" target="_blank">
							<img id="logo" alt="R Logo" src="Rlogo.png" width="75"/>
						</a>
						<p><p>
						<a href="https://www.rstudio.com/" target="_blank">
							<img id="logo" alt="RStudio Logo" src="blue-250.png" width="75"/>
						</a>
						<p><p>
						<a href="http://shiny.rstudio.com/" target="_blank">
							<img id="logo" alt="Shiny Logo" src="RStudio_Hex_shiny.png" width="75"/>
						</a>
						
						<p><p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:125px;left:300px;"> This web application was built using 
						<a href="http://shiny.rstudio.com/", target="_blank"> Shiny </a> by 
						<a href="https://www.rstudio.com/", target="_blank"> RStudio, </a> using open source software.  It heavily relies upon functions from 
						<a href="https://joey711.github.io/phyloseq/", target="_blank"> phyloseq, </a> 
						<a href="https://github.com/vegandevs/vegan", target="_blank"> vegan, </a> and 
						<a href="https://bioconductor.org/packages/release/bioc/html/DESeq2.html", target="_blank"> DESeq2. </a>  We highly endorse and
						encourage visiting the websites associated with these packages.  

						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:225px;left:300px;"><strong> Author:</strong></p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:250px;left:300px;"> Brian D. Piccolo </p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:275px;left:300px;"> Assistant Professor </p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:300px;left:300px;"> Arkansas Children\'s Nutrition Center </p>
						
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:350px;left:300px;">The following R packages were utilized in
						no particular order of importance</p>
						
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:375px;left:300px;"> <a href="http://shiny.rstudio.com/", target="_blank"> shiny </a> </p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:400px;left:300px;"> <a href="https://github.com/daattali/shinyjs", target="_blank"> shinyjs </a> </p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:425px;left:300px;"> <a href="https://rstudio.github.io/DT/", target="_blank"> DT </a> </p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:450px;left:300px;"> <a href="http://jkunst.com/highcharter/", target="_blank"> highcharter </a> *</p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:475px;left:300px;"> <a href="https://cran.r-project.org/web/packages/V8/vignettes/v8_intro.html", target="_blank"> V8 </a> </p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:500px;left:300px;"> <a href="https://www.bioconductor.org/packages/release/bioc/html/biomformat.html", target="_blank"> biomformat </a> </p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:525px;left:300px;"> <a href="http://ape-package.ird.fr/", target="_blank"> ape </a> </p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:550px;left:300px;"> <a href="https://github.com/psolymos/pbapply", target="_blank"> pbapply </a> </p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:575px;left:300px;"> <a href="https://github.com/tidyverse/tibble", target="_blank"> tibble </a> </p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:600px;left:300px;"> <a href="https://github.com/hadley/reshape", target="_blank"> reshape2 </a> </p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:625px;left:300px;"> <a href="https://joey711.github.io/phyloseq/", target="_blank"> phyloseq </a> </p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:650px;left:300px;"> <a href="https://github.com/hadley/dplyr", target="_blank"> dplyr </a> </p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:675px;left:300px;"> <a href="https://github.com/vegandevs/vegan", target="_blank"> vegan </a> </p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:700px;left:300px;"> <a href="https://bioconductor.org/packages/release/bioc/html/DESeq2.html", target="_blank"> DESeq2 </a> </p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:725px;left:300px;"> <a href="https://github.com/juba/scatterD3", target="_blank"> scatterD3 </a> </p>
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:750px;left:300px;"> <a href="http://colorbrewer2.org/", target="_blank"> RColorBrewer </a> </p>
						
						
						<p class="about_text" style="font: Tahoma;font-size: 130%;position:absolute;top:800px;left:300px;"> *This app uses 
							<a href="https://shop.highsoft.com/faq/non-commercial", target="_blank"> Highsoft </a> software with non-commercial packages.  
							Highsoft software product is not free for commercial use.</p>
						
						
						
					'
					)
				)
			)
		)					
    )	
)	
				