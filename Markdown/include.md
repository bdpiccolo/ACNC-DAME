### **Getting Started:**

*DAME* requires two files to operate:

* BIOM file - QIIME output file typically found in the folder (OTU) during otu picking method using either of the programs **(pick_open_reference_otus.py, pick_closed_reference_otus.py or pick_de_novo_otus.py)**.

	* Use the OTU generated file that has taxonomy details (e.g. **otu_table_mc3_w_tax.biom**). DAME will fail to recognize OTU table without taxonomy details.

	* DAME can accept BIOM files in HDF5 and JSON formats.

* BIOM Metadata File - .CSV file containing a column with exact sample labels used in QIIME analysis and experimental groupings. 

	* It is recommended to re-purpose the original map file used in QIIME analysis.

	* The current version of DAME does not recognize continuous data and will either remove continuous data or treat the data as classifiers.  

	* The number of sample IDs in the Metadata file does not have to match the number of sample IDs in the BIOM file.  DAME will subset matching sample IDs between the two files and use those as the imported data.

* TRE file (optional) - .TRE file, typically found in the same folder (OTU) from QIIME output.

	* Currently, DAME only utilizes a TRE file for β-Diversity calculations (Weighted and Unweighted Unifrac, and DPCoA distances).


### Features 

* Utilizes BIOM file to minimize potential loss of data or errors associated with converting and/or changing file formats.

* Can select/deselect experimental groups and samples for downstream analysis based on research question and statistical findings.  
    
* Can customize thresholds to filter samples with low total counts and low abundant OTUs.

* Utilizes Shiny for interactive input selections (widgets).

    * Widgets are dynamically updated based on experimental group selections.
	
	* Tables and graphics are reactive to widget inputs.
	
* Linear workflow managed by control buttons

* Analyze 1 or more taxonomic levels simultaneously

* All tables and graphics are interactive.

### Analyses 

* Summary statistics before and after filters:

	* Sample prevalence
	
	* Total Reads
   
	* Total OTUs 
   
* α-diversity statistics by taxonomic levels:

	* Calculates observed, chao1, ACE, Shannon, Simpson, Inverse Simpson, and Fisher indices.
   
	* Calculates 1-way or multifactor ANOVAs, t-tests, Kruskal Wallis, and Mann Whitney U tests based on meta-data.
	
	* Provides QQ plots and Fitted vs Residual plots to identify behaviors of parametric tests.
   
    * Output tables are rendered with the [DT](https://rstudio.github.io/DT/) package.
   
    * Barplots rendered with the [rbokeh](http://hafen.github.io/rbokeh/) package.
   
    * All data (α-diversity calculations and statistics) are downloadable.
   
* β-diversity statistics by taxonomic levels:

    * Calculates multiple dissimilarity, distance, and tree based parameters.
   
    * Calculates several ordination methods, including Principal Co-ordinate Analysis, Non-Metric Multidimensional Scaling, and others.
   
    * Ordination plots rendered with the [scatterD3](https://cran.r-project.org/web/packages/scatterD3/vignettes/introduction.html) package.
   
    * Tables are rendered with the [DT](https://rstudio.github.io/DT/) package.
   
* Differential abundance analysis using Negative Binomial Regression by taxonomic levels:

    * Pairwise comparisons of meta-data using [DESeq2](http://genomebiology.biomedcentral.com/articles/10.1186/s13059-014-0550-8) workflow.
   
    * DESeq2 result table rendered with [DT](https://rstudio.github.io/DT/) package.
   
    * Boxplots displayed with either Total Reads or Percent Abundance and rendered with the [rbokeh](http://hafen.github.io/rbokeh/) package.
   
    * All results are downloadable.

### Instructions/Manual

Detailed instructions can be found at the [DAME Wiki](https://github.com/bdpiccolo/ACNC-DAME/wiki) and [Github repository](https://github.com/bdpiccolo/ACNC-DAME/tree/master/Instructions)
