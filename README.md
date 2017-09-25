# Dynamic Assessment of Microbial Ecology (DAME)

A shiny app using the R environment to perform microbial analysis of phylogenetic sequencing data.  Dame was specifically designed to work directly with output files from QIIME 1 with as minimal file processing as possible.

The current release (v0.1) assesses α- and β-Diversity measurements, and differential expression analyses of sequencing count data. DAME requires the .BIOM file from QIIME and a .CSV file containing the .BIOM sample labels and metadata (experimental grouping data) associated with each sample. This app utilizes the Shiny framework to allow for dynamic and real-time interaction with virtually all aspects of the data workflow. Where possible, table and graphic outputs utilize D3 for a fully interactive experience.

---

## Installation

*DAME* can be downloaded and run locally providing that the latest version of R and shiny are installed.  Github launch will install all required packages. Cut and paste the following script into the R console and *DAME* will launch and open in browser.

```r
library(shiny)
runGitHub("ACNC-DAME", "bdpiccolo")
```

## Getting Started

*DAME* requires two files to operate:

1. BIOM file - QIIME output file typically found in the folder (OTU) during otu picking method using either of the programs **(pick_open_reference_otus.py, pick_closed_reference_otus.py or pick_de_novo_otus.py)**.

    * Use the OTU generated file that has taxonomy details (e.g. **otu_table_mc3_w_tax.biom**). DAME will fail to recognize OTU table without taxonomy details.

    * DAME can accept BIOM files in HDF5 and JSON formats.

2. BIOM Metadata File - .CSV file containing a column with exact sample labels used in QIIME analysis and experimental groupings. 

    * It is recommended to re-purpose the original map file used in QIIME analysis.

    * The current version of DAME does not recognize continuous data and will either remove continuous data or treat the data as classifiers.  

    * The number of sample IDs in the Metadata file does not have to match the number of sample IDs in the BIOM file.  DAME will subset matching sample IDs between the two files and use those as the imported data.

3. TRE file (optional) - .TRE file, typically found in the same folder (OTU) from QIIME output.

    * Currently, DAME only utilizes a TRE file for β-Diversity calculations (Weighted and Unweighted Unifrac, and DPCoA distances).

## Example of BIOM Metadata 

![](https://github.com/bdpiccolo/ACNC-DAME/blob/master/www/MetadataPic.png?raw=true)

### Features 

1. Utilizes BIOM file to minimize potential loss of data or errors associated with converting and/or changing file formats.

2. Can select/deselect experimental groups and samples for downstream analysis based on research question and statistical findings.  
    
3. Can customize thresholds to filter samples with low total counts and low abundant OTUs.

4. Utilizes Shiny for interactive input selections (widgets).

    * Widgets are dynamically updated based on experimental group selections.
	
	* Tables and graphics are reactive to widget inputs.
	
5. Linear workflow managed by control buttons

6. Analyze 1 or more taxonomic levels simultaneously

7. All tables and graphics are interactive.

### Analyses 

1. Summary statistics before and after filters:

	* Sample prevalence
	
	* Total Reads
   
	* Total OTUs 
   
2. α-diversity statistics by taxonomic levels:

	* Calculates observed, chao1, ACE, Shannon, Simpson, Inverse Simpson, and Fisher indices.
   
	* Calculates 1-way or multifactor ANOVAs, t-tests, Kruskal Wallis, and Mann Whitney U tests based on meta-data.
	
	* Provides QQ plots and Fitted vs Residual plots to identify behaviors of parametric tests.
   
    * Output tables are rendered with the [DT](https://rstudio.github.io/DT/) package.
   
    * Barplots rendered with the [rbokeh](http://hafen.github.io/rbokeh/) package.
   
    * All data (α-diversity calculations and statistics) are downloadable.
   
3. β-diversity statistics by taxonomic levels:

    * Calculates multiple dissimilarity, distance, and tree based parameters.
   
    * Calculates several ordination methods, including Principal Co-ordinate Analysis, Non-Metric Multidimensional Scaling, and others.
   
    * Ordination plots rendered with the [scatterD3](https://cran.r-project.org/web/packages/scatterD3/vignettes/introduction.html) package.
   
    * Tables are rendered with the [DT](https://rstudio.github.io/DT/) package.
   
4. Differential abundance analysis using Negative Binomial Regression by taxonomic levels:

    * Pairwise comparisons of meta-data using [DESeq2](http://genomebiology.biomedcentral.com/articles/10.1186/s13059-014-0550-8) workflow.
   
    * DESeq2 result table rendered with [DT](https://rstudio.github.io/DT/) package.
   
    * Boxplots displayed with either Total Reads or Percent Abundance and rendered with the [rbokeh](http://hafen.github.io/rbokeh/) package.
   
    * All results are downloadable.

### Instructions/Manual

Detailed instructions can be found at the [DAME Wiki](https://github.com/bdpiccolo/ACNC-DAME/wiki) and [Github repository](https://github.com/bdpiccolo/ACNC-DAME/tree/master/Instructions)

### About 

This web application was built using [Shiny](http://shiny.rstudio.com/) by [RStudio](https://www.rstudio.com/) using open source software.  It heavily relies upon functions from [phyloseq](https://joey711.github.io/phyloseq/), [vegan](https://github.com/vegandevs/vegan), and [DESeq2](https://bioconductor.org/packages/release/bioc/html/DESeq2.html).  We highly endorse and encourage visiting the websites associated with these packages.

#### Created by: 

  [Brian Piccolo](https://www.ncbi.nlm.nih.gov/myncbi/browse/collection/47610545/?sort=date&direction=descending)  
  Assistant Professor  
  [Arkansas Nutrition Research Center](http://acnc.uamsweb.com/home/faculty-listing/brian-piccolo/)  
  University of Arkansas for Medical Sciences  

#### Special thanks to:

  [Kartik Shankar](http://acnc.uamsweb.com/home/faculty-listing/kartik-shankar/)  
  [Sree Chintapalli](http://acnc.uamsweb.com/home/faculty-listing/sree-v-chintapalli/)  
  Umesh Wankhade
  Sudeepa Bhattacharyya
  Chunqiao Luo

The following R packages were utilized in no particular order of importance:

  [shiny](http://shiny.rstudio.com/)  
  [shinyjs](https://github.com/daattali/shinyjs)  
  [DT](https://rstudio.github.io/DT/)   
  [V8](https://cran.r-project.org/web/packages/V8/vignettes/v8_intro.html)  
  [biomformat](https://www.bioconductor.org/packages/release/bioc/html/biomformat.html)  
  [ape](http://ape-package.ird.fr/)  
  [pbapply](https://github.com/psolymos/pbapply)  
  [tibble](https://github.com/tidyverse/tibble)  
  [reshape2](https://github.com/hadley/reshape)  
  [phyloseq](https://joey711.github.io/phyloseq/)  
  [dplyr](https://github.com/hadley/dplyr)  
  [vegan](https://github.com/vegandevs/vegan)  
  [DESeq2](https://bioconductor.org/packages/release/bioc/html/DESeq2.html)  
  [scatterD3](https://github.com/juba/scatterD3)  
  [RColorBrewer](http://colorbrewer2.org/)  
  [rbokeh](http://hafen.github.io/rbokeh/)
						
