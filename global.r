 
install_packages <- c("shiny","shinyjs","DT", "highcharter", "V8","ape","pbapply",
            "tibble","reshape2", "dplyr", "vegan", "scatterD3","RColorBrewer", "markdown")
if (length(setdiff(install_packages, rownames(installed.packages()))) > 0) {
            install.packages(setdiff(install_packages, rownames(installed.packages())))
}
 
bioconductor_packages <- c("biomformat", "phyloseq", "DESeq2")
if (length(setdiff(bioconductor_packages, rownames(installed.packages()))) > 0) {
            source("https://bioconductor.org/biocLite.R")
            biocLite(setdiff(bioconductor_packages, rownames(installed.packages())), suppressUpdates = TRUE)
}
 

library(biomformat)
library(ape)
library(pbapply)
library(tibble)
library(reshape2)
library(phyloseq)
library(DT)
library(dplyr)
library(vegan)
library(DESeq2)
library(RColorBrewer)
library(tidyr)

## Functions


fixUploadedFilesNames <- function(x) {
	if (is.null(x)) {
		return()
	}
 
	oldNames = x$datapath
	newNames = file.path(dirname(x$datapath), x$name)
	file.rename(from = oldNames, to = newNames)
	x$datapath <- newNames
	x
}


taxaconvert <- function(phyloO, label_UNK=FALSE) {
	taxT <- tax_table(phyloO)
	taxT[,"Domain"] <- gsub("k__", "", taxT[,"Domain"])
	taxT[,"Phylum"] <- gsub("p__", "", taxT[,"Phylum"])
	taxT[,"Class"] <- gsub("c__", "", taxT[,"Class"])
	taxT[,"Order"] <- gsub("o__", "", taxT[,"Order"])
	taxT[,"Family"] <- gsub("f__", "", taxT[,"Family"])
	taxT[,"Genus"] <- gsub("g__", "", taxT[,"Genus"])
	taxT[,"Species"] <- gsub("s__", "", taxT[,"Species"])
	if(label_UNK == TRUE) {
		taxT[,"Domain"][which(taxT[,"Domain"] %in% "")] <- "Unassigned"
		taxT[,"Phylum"][which(taxT[,"Phylum"] %in% "")] <- "Unassigned"
		taxT[,"Class"][which(taxT[,"Class"] %in% "")] <- "Unassigned"
		taxT[,"Order"][which(taxT[,"Order"] %in% "")] <- "Unassigned"
		taxT[,"Family"][which(taxT[,"Family"] %in% "")] <- "Unassigned"
		taxT[,"Genus"][which(taxT[,"Genus"] %in% "")] <- "Unassigned"
		taxT[,"Species"][which(taxT[,"Species"] %in% "")] <- "Unassigned"
	}
	taxT
}

phylotreeDIST <- c("Unweighted Unifrac" = "unifrac", "Weighted Unifrac"	= "wunifrac", "DPCOA" = "dpcoa")
Dissimilarity <- c("Bray-Curtis" = "bray", "Jaccard" = "jaccard", "Jensen-Shannon" = "jsd", "Mountford" = "mountford", "Gower" = "gower",
	"Morisita" = "morisita", "Horn" = "horn", "Kulczynski" = "kulczynski", "Horn" = "horn", "Raup" = "raup", "Binomial" = "binomial", 
		"Maximum" = "maximum", "Binary" = "binary")
Distance <- c("Euclidean" = "euclidean", "Manhattan" = "manhattan", "Canberra" = "canberra", "Minkowski" = "minkowski")

Ordinate <- c("Principal Coordinate Analysis" = "PCoA", "Non-metric MultiDimensional Scaling" = "NMDS",
	"Multideminsional Scaling" = "MDS", "Constrained Correspondence Analysis" = "CCA", 
	"Redundancy Analysis" = "RDA", "Detrended Correspondence Analysis" = "DCA")
	
TreeOrdinate <- c("Double Principle Coordinate Analysis" = "DPCoA")


taxaL <- c("Phylum", "Class","Order","Family","Genus")
adivL <- c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")


factorSPLIT <- function(factr, char=NULL, side=1) {
	if(is.null(char)) stop("char must be defined")
	sapply(strsplit(as.character(factr), char), function(x) x[side])
}


captureCN <- function(colNMS, capture) {
	split_capture <- strsplit(capture, "_")[[1]]

	logicMAT <- sapply(1:length(split_capture), function(x) {
		factorSPLIT(colNMS[2:length(colNMS)], "_", x) %in% split_capture[x]
	})

	MAT_sum <- rowSums(logicMAT)
	logicVEC <- ifelse(MAT_sum == length(split_capture), TRUE, FALSE)
	c(FALSE, logicVEC)
}


gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}


options(shiny.maxRequestSize=5000*1024^2)

 
get_box_values <- function(x = rt(1000, df = 10)){ 
    boxplot.stats(x)$stats %>% 
		t() %>% 
		as.data.frame() %>% 
		setNames(c("low", "q1", "median", "q3", "high"))
}
  
get_outliers_values <- function(x = rt(1000, df = 10)) {
	boxplot.stats(x)$out
}
  
 		  
parseboxFUN <- function(datav, metav) {
	parseDAT <- lapply(levels(metav), function(x) { 
		Fdat <- datav[metav %in% x]
		as.list(c(name = x, get_box_values(Fdat)))
	})
	list(	
		list(g1 = as.logical("NA"),
			data= parseDAT,
			type = "boxplot",
			id = as.logical("NA")
		)
	)	
} 

parseoutFUN <- function(datav, metav) {
	
	parseDAT <- lapply(levels(metav), function(x) {
		Fdat <- datav[metav %in% x]
		out <- get_outliers_values(Fdat)
		if(length(out) == 0){
			out <- list(list(name=x, y=out))
		} else {
			if(length(out) == 1) {
				out <- list(list(name=x, y=out))
			} else {
				out <- lapply(1:length(out), function(k) list(name=x, y=out[k]))
			}
		}
	})	
	parseDAT <- do.call(list, unlist(parseDAT, recursive=FALSE))
	

	listlength <- 0 %in% sapply(parseDAT, function(x) length(x[[2]]))
	
	if(listlength){
		parseDAT <- parseDAT[sapply(parseDAT, function(x) length(x[[2]])) %in% 1]
		
	} else {
		parseDAT <- parseDAT
	}

	list(
		list(name = as.logical(NA),
			data = parseDAT,
			type = "scatter",
			linkedTo=  as.character(NA)
		)
	)
}
	
hcboxplot_v3 <- function(x = NULL, var = NULL, outliers = TRUE, ...) {
  
  stopifnot(!is.null(x))
  

  series_box <- parseboxFUN(x, var)
  series_out <- parseoutFUN(x, var)
  

 
  hc <- highchart() %>% 
    hc_chart(type = "bar") %>% 
    # hc_colors(colors) %>% 
    hc_xAxis(type = "category") %>% 
    hc_plotOptions(series = list(
      marker = list(
        symbol = "circle"
      )
    )) 
  
  hc <- hc_add_series_list(hc, series_box)
  
  # if(is.na(var2) || is.na(var)) {
    # hc <- hc %>% 
      # hc_xAxis(categories = "") %>% 
      # hc_plotOptions(series = list(showInLegend = FALSE))
  # }
    
  
  if(outliers)
    hc <- hc_add_series_list(hc, series_out)
  
  hc
}






