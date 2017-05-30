
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

hcboxplot.2 <- function (x = NULL, var = NULL, var2 = NULL, outliers = TRUE, 
    ...) 
{
    stopifnot(is.numeric(x))
    if (is.null(var)) 
        var <- NA
    if (is.null(var2)) 
        var2 <- NA
    df <- data_frame(x, g1 = var, g2 = var2)
    get_box_values <- function(x) {
        boxplot.stats(x)$stats %>% t() %>% as.data.frame() %>% 
            setNames(c("low", "q1", "median", "q3", "high"))
    }
    get_outliers_values <- function(x) {
        boxplot.stats(x)$out
    }
    series_box <- df %>% group_by_("g1", "g2") %>% do(data = get_box_values(.$x)) %>% 
        tidyr::unnest() %>% group_by_("g2") %>% do(data = list_parse(rename_(select_(., 
        "-g2"), name = "g1"))) %>% mutate(type = "boxplot") %>% 
        mutate_(id = "as.character(g2)")
    # if (length(list(...)) > 0) 
        # series_box <- add_arg_to_df(series_box, ...)
    series_out <- df %>% group_by_("g1", "g2") %>% do(data = get_outliers_values(.$x)) %>% 
        tidyr::unnest() %>% group_by_("g2") %>% do(data = list_parse(select_(., 
        name = "g1", y = "data"))) %>% mutate(type = "scatter") %>% 
        mutate(linkedTo = "as.character(g2)")
    # if (length(list(...)) > 0) 
        # series_out <- add_arg_to_df(series_out, ...)
    if (!has_name(list(...), "color")) {
        colors <- colorize(seq(1, nrow(series_box)))
        colors <- hex_to_rgba(colors, alpha = 0.75)
    }
    if (!has_name(list(...), "name")) {
        series_box <- rename_(series_box, name = "g2")
        series_out <- rename_(series_out, name = "g2")
    }
	
	
    hc <- highchart() %>% hc_chart(type = "bar") %>% hc_xAxis(type = "category") %>% 
        hc_plotOptions(series = list(marker = list(symbol = "circle")))
    hc <- hc_add_series_list(hc, list_parse(series_box))
    if (is.na(var2) || is.na(var)) {
        hc <- hc %>% hc_xAxis(categories = "") %>% hc_plotOptions(series = list(showInLegend = FALSE))
    }
    if (outliers) 
        hc <- hc_add_series_list(hc, list_parse(series_out))
    hc
}

MS <- function() {
	metaselections <- reactive({ 
		req(PHYLOSEQ())
		meta <- BIOMmetad_DAT()
		## Remove KEY column
		# metakey <- meta[,!(colnames(meta) %in% KEY())]
		## Remove columns that have the same number of factor levels as number of rows in data frame
		# if object is still a data frame, ie, multiple columns remain
		if(class(meta) != "data.frame") {
			## make a list with factor levels
			singleLEVELS <- list(levels(meta))
			## Get name of remaining column
			# Metacolumns <- colnames(meta)[!(colnames(meta) %in% KEY())]
			## update name of list with name of remaining column
			names(singleLEVELS) <- colnames(meta)				
			# names(singleLEVELS) <- colnames(meta)[!(colnames(meta) %in% KEY())]				
			# print("single columns after key filter")
		} else {
			metaFINAL <- meta[,sapply(meta, nlevels) != nrow(meta)]
			if(class(metaFINAL) != "data.frame") {
			# If not a data frame, ie, only one column left
				# make a list with factor levels
				singleLEVELS <- list(levels(metaFINAL))
				# Get name of remaining column
				# Metacolumns <- colnames(meta)
				Metacolumns <- colnames(meta)[sapply(meta, nlevels) != nrow(meta)]
				# update name of list with name of remaining column
				names(singleLEVELS) <- colnames(meta)[sapply(meta, nlevels) != nrow(meta)]
				# print("single columns after nrow filter")
			} else {
				# Get names of remaining columns 
				# Metacolumns <- colnames(meta)
				Metacolumns <- colnames(meta)[sapply(meta, nlevels) != nrow(meta)]
				# Get factor levesl across all columns and turn it into a list
				singleLEVELS <- lapply(metaFINAL[,sapply(metaFINAL, nlevels) != nrow(metaFINAL)], levels)
				# print("multiple columns after key and nrow filter")
			}
			
		}
		list(Groups = Metacolumns, Levels=singleLEVELS)
		# metaFINAL

	})
}	


hcboxplot.2 <- function (x = NULL, var = NULL, var2 = NULL, outliers = TRUE, 
    ...) 
{
    stopifnot(is.numeric(x))
    if (is.null(var)) 
        var <- NA
    if (is.null(var2)) 
        var2 <- NA
    df <- data_frame(x, g1 = var, g2 = var2)
    get_box_values <- function(x) {
        boxplot.stats(x)$stats %>% t() %>% as.data.frame() %>% 
            setNames(c("low", "q1", "median", "q3", "high"))
    }
    get_outliers_values <- function(x) {
        boxplot.stats(x)$out
    }
    series_box <- df %>% group_by_("g1", "g2") %>% do(data = get_box_values(.$x)) %>% 
        tidyr::unnest() %>% group_by_("g2") %>% do(data = list_parse(rename_(select_(., 
        "-g2"), name = "g1"))) %>% mutate(type = "boxplot") %>% 
        mutate_(id = "as.character(g2)")
    # if (length(list(...)) > 0) 
        # series_box <- add_arg_to_df(series_box, ...)
    series_out <- df %>% group_by_("g1", "g2") %>% do(data = get_outliers_values(.$x)) %>% 
        tidyr::unnest() %>% group_by_("g2") %>% do(data = list_parse(select_(., 
        name = "g1", y = "data"))) %>% mutate(type = "scatter") %>% 
        mutate(linkedTo = "as.character(g2)")
    # if (length(list(...)) > 0) 
        # series_out <- add_arg_to_df(series_out, ...)
    if (!has_name(list(...), "color")) {
        colors <- colorize(seq(1, nrow(series_box)))
        colors <- hex_to_rgba(colors, alpha = 0.75)
    }
    if (!has_name(list(...), "name")) {
        series_box <- rename_(series_box, name = "g2")
        series_out <- rename_(series_out, name = "g2")
    }
	
	
    hc <- highchart() %>% hc_chart(type = "bar") %>% hc_xAxis(type = "category") %>% 
        hc_plotOptions(series = list(marker = list(symbol = "circle")))
    hc <- hc_add_series_list(hc, list_parse(series_box))
    if (is.na(var2) || is.na(var)) {
        hc <- hc %>% hc_xAxis(categories = "") %>% hc_plotOptions(series = list(showInLegend = FALSE))
    }
    if (outliers) 
        hc <- hc_add_series_list(hc, list_parse(series_out))
    hc
}








