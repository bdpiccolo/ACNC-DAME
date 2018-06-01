 
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

taxaFIX <- function(biom) {
	# TAXAres <- observation_metadata(biom)
	if(class(biom) == "data.frame") {
		TAXAvec <- biom[,sapply(biom, function(x) setequal(grepl("k__", x), TRUE))]
		TAXA <- strsplit(TAXAvec, "; ")
		names(TAXA) <- rownames(biom)
	} else {
		TAXA <- biom
	}	
	TAXAMAT <- as.data.frame(matrix(NA, ncol=8, nrow=length(TAXA)))
	colnames(TAXAMAT) <- c("d__", "k__", "p__", "c__", "o__", "f__", "g__", "s__")
	rownames(TAXAMAT) <- names(TAXA)
	for(i in 1:length(TAXA)){
		## Domain
		if(!(setequal(grepl("d__", TAXA[[i]]), FALSE))){
			TAXAMAT[i,colnames(TAXAMAT) %in% "d__"] <- TAXA[[i]][grepl("d__", TAXA[[i]])]
			rownames(TAXAMAT)[i] <- names(TAXA)[i]
		} else {
			NULL
		}	
		## Kingdom
		if(!(setequal(grepl("k__", TAXA[[i]]), FALSE))){
			TAXAMAT[i,colnames(TAXAMAT) %in% "k__"] <- TAXA[[i]][grepl("k__", TAXA[[i]])]
			rownames(TAXAMAT)[i] <- names(TAXA)[i]
		} else {
			NULL
		}
		## Phylum
		if(!(setequal(grepl("p__", TAXA[[i]]), FALSE))){
			TAXAMAT[i,colnames(TAXAMAT) %in% "p__"] <- TAXA[[i]][grepl("p__", TAXA[[i]])]
			rownames(TAXAMAT)[i] <- names(TAXA)[i]
		} else {
			NULL
		}
		## Class
		if(!(setequal(grepl("c__", TAXA[[i]]), FALSE))){
			TAXAMAT[i,colnames(TAXAMAT) %in% "c__"] <- TAXA[[i]][grepl("c__", TAXA[[i]])]
			rownames(TAXAMAT)[i] <- names(TAXA)[i]
		} else {
			NULL
		}
		## Order
		if(!(setequal(grepl("o__", TAXA[[i]]), FALSE))){
			TAXAMAT[i,colnames(TAXAMAT) %in% "o__"] <- TAXA[[i]][grepl("o__", TAXA[[i]])]
			rownames(TAXAMAT)[i] <- names(TAXA)[i]
		} else {
			NULL
		}
		## Family
		if(!(setequal(grepl("f__", TAXA[[i]]), FALSE))){
			TAXAMAT[i,colnames(TAXAMAT) %in% "f__"] <- TAXA[[i]][grepl("f__", TAXA[[i]])]
			rownames(TAXAMAT)[i] <- names(TAXA)[i]
		} else {
			NULL
		}
		## Genus
		if(!(setequal(grepl("g__", TAXA[[i]]), FALSE))){
			TAXAMAT[i,colnames(TAXAMAT) %in% "g__"] <- TAXA[[i]][grepl("g__", TAXA[[i]])]
			rownames(TAXAMAT)[i] <- names(TAXA)[i]
		} else {
			NULL
		}
		## Species
		if(!(setequal(grepl("s__", TAXA[[i]]), FALSE))){
			TAXAMAT[i,colnames(TAXAMAT) %in% "s__"] <- TAXA[[i]][grepl("s__", TAXA[[i]])]
			rownames(TAXAMAT)[i] <- names(TAXA)[i]
		} else {
			NULL
		}

	}
	TAXAMAT <- apply(TAXAMAT, 2, function(x) ifelse(x %in% NA, "", x))
	colnames(TAXAMAT) <- c("d__", "k__", "p__", "c__", "o__", "f__", "g__", "s__")
	rownames(TAXAMAT) <- names(TAXA)
	TAXAMAT
}


taxaconvert <- function(phyloO, label_UNK=FALSE) {
	taxT <- tax_table(phyloO)
	for(i in colnames(taxT)) for(i in colnames(taxT)) taxT[,i] <- gsub("k__", "", taxT[,i])
	if(label_UNK == TRUE) {
		for(i in colnames(taxT)) for(i in colnames(taxT)) taxT[,i][which(taxT[,i] %in% "")] <- "Unassigned"
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
		
qqlineRESULTS <- function (y, datax = FALSE, distribution = qnorm, probs = c(0.25, 
    0.75), qtype = 7, ...) 
{
    stopifnot(length(probs) == 2, is.function(distribution))
    y <- quantile(y, probs, names = FALSE, type = qtype, na.rm = TRUE)
    x <- distribution(probs)
    if (datax) {
        slope <- diff(x)/diff(y)
        int <- x[1L] - slope * y[1L]
    }
    else {
        slope <- diff(y)/diff(x)
        int <- y[1L] - slope * x[1L]
    }
	list(intercept = int, slope = slope)
}






