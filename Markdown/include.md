### **Getting Started:**

*DAME* requires two files to operate:

1. BIOM file - QIIME output file typically found in the folder (**OTU**) during otu picking method using either of the programs (**pick_open_reference_otus.py**, **pick_closed_reference_otus.py** or **pick_de_novo_otus.py**).

  Note: Use the OTU generated file that has taxonomy details (e.g. **otu_table_mc3_w_tax.biom**).  DAME will fail to recognize OTU table without taxonomy details.

2. BIOM Metadata File - .CSV file containing a column with exact sample labels used in QIIME analysis and experimental groupings.  It is recommended to re-purpose the original map file used in QIIME analysis.

3. TRE file (optional) - .TRE file, typically found in the same folder (**OTU**) from QIIME output.

BIOM files that are generated through other pipelines (which are in JSON format) must be converted to HDF5 format before loading into *DAME*.

* Convert from biom to txt:

```python
biom convert –i table.biom –o otu_table.txt –to-tsv –header-key taxonomy
```

*	Convert back to biom:

```python
biom convert –i otu_table.txt –o new_ otu_table.biom –to-hdf5 –table-type=”OTU table”
–process-obs-metadata taxonomy
```

### Features 

- Utilizes BIOM file to minimize data manipulation steps
- Experimental group selections automatically render 
- Allows user to filter data based on meta-data and/or low abundant sequence reads
- Utilizes Shiny for interactive inputs and reactive tables and graphics
- All tables and graphics are interactive

### Analyses 

* Summary statistics before and after filters:

   * Sample prevalence
   * Total Reads
   * Total OTUs 
   
* α-diversity statistics by taxonomic levels:

   * Calculates observed, chao1, ACE, Shannon, Simpson, Inverse Simpson, Fisher indices.
   * Calculates 1-way or multifactor ANOVAs based on meta-data.
   * Output tables are rendered with the [DT](https://rstudio.github.io/DT/) package.
   * Barplots rendered with the [highcharter](http://jkunst.com/highcharter/) package.
   * All data (α-diversity calculations and statistics) are downloadable.
   
* β-diversity statistics by taxonomic levels:

   * Calculates multiple dissimilarity, distance, and tree based parameters.
   * Calculates several ordination methods, including Principal Co-ordinate Analysis, Non-Metric Multidimensional Scaling, and others.
   * Ordination plots rendered with the [scatterD3](https://cran.r-project.org/web/packages/scatterD3/vignettes/introduction.html) package.
   * Tables are rendered with the [DT](https://rstudio.github.io/DT/) package.
   
* Differential abundance analysis using Negative Binomial Regression by taxonomic levels:

   * Pairwise comparisons of meta-data using [DESeq2](http://genomebiology.biomedcentral.com/articles/10.1186/s13059-014-0550-8) workflow.
   * DESeq2 result table rendered with [DT](https://rstudio.github.io/DT/) package.
   * Boxplots displayed with either Total Reads or Percent Abundance and rendered with the [highcharter](http://jkunst.com/highcharter/) package.
   * All results are downloadable.

### Instructions/Manual

Detailed instructions can be found at the [DAME Wiki](https://github.com/bdpiccolo/ACNC-DAME/wiki) and [Github repository](https://github.com/bdpiccolo/ACNC-DAME/tree/master/Instructions)
