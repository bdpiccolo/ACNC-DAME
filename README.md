# Dynamic Assessment of Microbial Ecology (DAME)

A shiny app usint the R environment to perform microbial analysis of phylogenetic sequencing data.  Dame was specifically designed to work directly with output files from QIIME 1 with as minimal file processing as possible.

The current release (v0.1) assesses α- and β-Diversity measurements, and differential expression analyses of sequencing count data. DAME requires the .BIOM file from QIIME and a .CSV file containing the .BIOM sample labels and metadata (experimental grouping data) associated with each sample. This app utilizes the Shiny framework to allow for dynamic and real-time interaction with virtually all aspects of the data workflow. Where possible, table and graphic outputs utilize D3 for a fully interactive experience.

---

## Getting Started ##

*DAME* requires two files to operate:

1. BIOM file - QIIME output file typically found in the folder (**OTU**) during otu picking method using either of the programs (**pick_open_reference_otus.py**, **pick_closed_reference_otus.py** or **pick_de_novo_otus.py**).

  Note: Use the OTU generated file that has taxonomy details (e.g. **otu_table_mc3_w_tax.biom**).  DAME will fail to recognize OTU table without taxonomy details.

2. BIOM Metadata File - .CSV file containing a column with exact sample labels used in QIIMe analysis and experimental groupings.  It is recommended to re-purpose the original map file used in QIIME analysis.

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

![Metadata Example](https://github.com/bdpiccolo/ACNC-DAME/tree/Additions/www/MetadataPic.png)
