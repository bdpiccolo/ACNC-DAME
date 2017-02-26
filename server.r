library(shiny)
library(shinyjs)
library(DT)
library(scatterD3)
library(highcharter)

source("global.r")

options(shiny.maxRequestSize=5000*1024^2)

shinyServer(function(input, output, session) {

	######################################################################
	## Toggle regular/larger text 
	########################################################################	
	observe({
		shinyjs::toggleClass("text_intro", "big", input$INTRObig)
    })
	

	source("server functions/Import Server Functions.r", local=TRUE)
	
	source("server functions/ADIV Server Functions.r", local=TRUE)

	source("server functions/BDIV Server Functions.r", local=TRUE)

	source("server functions/DABUND Server Functions.r", local=TRUE)


	######################################################################
	## Toggle regular/larger text 
	########################################################################	
	observe({
		shinyjs::toggleClass("text_about", "big", input$ABOUTbig)
    })
		
	
})


