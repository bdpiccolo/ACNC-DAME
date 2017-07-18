# Differential Abundance

Current implementation of differential abundance analysis of individual taxa is computed via negative binomial regression through the [DESeq2 package](https://github.com/Bioconductor-mirror/DESeq2).  Users can assess overall group differences with a likelihood ratio test comparing the full model versus a reduced model.  Pairwise comparisons of group levels are determined using Wald's Test.  Group differences of individual taxa are graphically shown as an interactive boxplot using the [highcharter package](https://github.com/jbkunst/highcharter).  DESeq2 results are presented in a tabular format using the [DT package](https://github.com/rstudio/DT).   

# Getting Started

There are 3 widgets that are initially displayed when selecting the Differential-Abundance tab.  The first 2 allow for selections of taxonomic level(s) and a specific test for the negative binomial regression.  All of these widgetes are managed by the *Finalize Differential Abundance* button.  Pressing the *Finalize Differential Abundance* button will finalize the analysis and render additional input widgets, DESeq2 results, and graphics.

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_dabund_getting_started.png?raw=true)

1. Select Taxonomic Level(s):

	* Can select 1 or more taxonomic levels (Phylum, Class, Order, Family, Genus, OTU)
	
	* Taxa selections do not need to be in the hierarchical order
	
	* Selecting OTU will use uploaded OTU data, while all other taxa selections will merge the OTU count data into the selected taxa using the tax_glom() function from the phyloseq package.  It takes longer to compute genus relative to family and so forth (genus > family > order > class > phylum)
		
2. Select Negative Binomial Regression Test:

	* Defaults to Wald Test.  Likelihood Ratio Test can be selected.
	
		* Selection of Wald Test will compute negative binomial regression with Wald test for pairwise comparisons.
		
		* Selection of Likelihood Ratio Test will compute a full model (includes group variable) and reduced (does not include group variable) models and then test whether the addition of the group variable is statistically signficant using a likelihood ratio test. 
	
	* If Wald Test is selected, an additional input widget listing available pairwise comparisons will render after pressing the *Finalize Differential Abundance* button.  This option is not rendered if Likelihood Ratio Test is selected.
		
3. Finalize Differential Abundance control button:

	* Press this button after making selections from the previous widgets.
	
	* Triggers calculations and analyses.
	
	* Renders additional widgets for group selections.
	
	* Renders DESeq2 results (downloading options and DT table), and boxplot(s) with additional control widgets. 
	
	* Can be pressed at anytime and will re-calculate DESeq2 calculations, and then render newly selected taxonomic levels if pressed again.		

# Group selections

Input widgets allowing for realtime updates of result table(s) and boxplot(s) are rendered below the initial set of widgets after pressing the *Finalize Differential Abundance* control button.  These widgets control which group is assessed in the negative binomial regression, which means that the DESeq2 calculations will need to be re-run for each group selection.  This will happen automatically and does not need a control button to manage the calculations.  

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_dabund_group_pairw.png?raw=true)

1. Select Group(s)

	* Can only select 1 group.  Available groups are dependent on Metadata file and final data selections from Import Data tab.
	
	* Groups will not be displayed in widget if data was finalized with only 1 group remaining.
	
	* Defaults selections to alphabetical order
	
	* Selections will automatically trigger new DESeq2 calculations, causing the output tables and graphics to refresh.
	
		* Amount of time required to re-assess DESeq2 calculations depends on selection of taxonomic level(s) and number of OTU within taxonomic level(s).
		
2. Select Pairwise Comparison

	* Only rendered if Wald Test is selected in *Select Negative Binomial Regression Test* widget above.
	
	* If there are only 2 factor levels in an experimental group, then the widget will only provide a single pairwise comparison.
	
	* If there are > 2 factor levels in an experimental group, then the widget will provide all potential pairwise comparisons.
	
# Results

For each taxonomic section there is a DT table providing the DESeq2 results and an interactive boxplot that is managed by additional input widgets. Rendering for each taxonomic level(s) is separated by a line break (horizontal rule), so each taxonomic “section” contains a graphical and tabular output. Each taxonomic section contains a header that identifies which taxonomic level is displayed. Rendering of the taxonomic sections will always follow the hierarchical ordering, regardless if the selections in the Select Taxonomic Levels input widget are not in order.
	
# Differential Abundance Table

DAME uses DESeq2 to calculate negative binomial regression models and post hoc tests.  A modified version of the output provided by the [results() function](https://bioconductor.org/packages/devel/bioc/manuals/DESeq2/man/DESeq2.pdf) is diplayed as a DT table.  The unaltered results() output for all pairwise comparisons within a taxonomic level is downloadable.  

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_dabund_deseq2_table.png?raw=true)

1. The DT table always displays the OTU tag, taxonomic information, base mean, log2 fold change (if Wald Test is selected), original P-value, and the FDR adjusted P-value. 

	* If Wald Test is selected, then the table represents the pairwise comparison designated in the *Select Pairwise Comparison* widget.
	
	* If Likelihood Ratio Test is selected, then the table represents a likelihood ratio test comparing the full model against the null model.  

	* Selections of *Select Group(s)* and *Select Pairwise Comparison* widgets will update the table in real time.
 
	* Table is fully interactive: 
	
		* Expand and contract number of rows using the Show entries drop down feature.
		
		* Save table to either different file formats (Excel, PDF, and CSV).  Will only save the currently displayed information.
		
		* Search capabilities.
		
		* All columns are sortable.
		
		* Can select previous or next pages.
		
2. The full DESeq2 results() output is available by clicking the Show/Hide Differential Abundance Downloading Options hyperlink above the DT table.

	* Clicking on the hyperlink will render 2 additional widgets.  This hyperlink is below the 2 hyperlinks for downloading options. 
	
		1. Choose File Name
		
			* Defaults file name to "Differential Abundance Data."
			
			* Can add text to box and type in new name of file.
			
		2. Download Relative Abundance Data
		
			* Pressing this button will download a .csv file with the base means, log2 fold change, log2 fold change standard error, Wald statistic, p-value, adjusted p-value, taxonomic information, OTU tag, test used for analysis (Wald or LRT), and group.


# Boxplots

An interactive boxplot is rendered within each taxonomic section using the highcharter package.  Either 2 or 3 input widgets are rendered with the boxplot depending on how many factor levels are within the selected experimental group from the *Select Group(s)* widget.

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_dabund_plot_interactivity.png?raw=true)

1. Select Bacteria

	* Selection of data to display in boxplot.
	
	* Displays taxa name and OTU identifier in phylum - genus sections.  Only shows OTU identifier in OTU section.
	
		* Taxa that are not annotated at a given taxonomic unit are labeled as unassigned and distinguished by their OTU identifier.
		
	* Defaults to taxa with the lowest adjusted p-value in the adjacent table.
		
	* The shiny::selectInput() function that renders the dropdown box appears to have a item limit and may not show all OTUs; however, OTUs that are not displayed in the dropdown menu are still accessible.
	
		* Click the selection box and hit backspace.  The dropdown selections will appear, but do not select any of these items. 
		
		* Immediately type in the name or number of the taxa of interest.
		
		* The dropdown selections should filter those that contain the character string typed into the selection box.
		
		* Select provided taxa item.
		
2. Select Data Type

	* Selects the data format for the boxplot
	
	* Defaults to total reads (counts).  Percent Abundance (% relative abundance) can be selected.
	
	* As DESeq2 calculates the negative binomial model on the raw counts, DAME defaults to Total Reads.
	
3. Show all Groups

	* Will only render if Wald Test is selected and there are > 2 factor levels in the selected experimental group.
	
	* As the Wald Test is a pairwise comparison, DAME defaults to "NO" and will display the groups selected in the *Select Pairwise Comparison* widget.  
	
	* Clicking "YES" will update the boxplot with all factor levels found within the selected experimental group.
	
4. Boxplot

	* More information regarding the highcharter package can be found [here](http://jkunst.com/highcharter/).  Plotting interactivity (e.g., hovering text box, group selections, etc.) in highcharter plots within a taxonomic section are independent of others if multiple taxonomic levels are rendered.  Interactive functionality in DAME is described herein.

	* Hovering the mouse cursor over a bar will provide additional text.  Currently, DAME will provide the name of the group level, the maximum value contained within the box and whisker, the upper quartile value, the median value, the lower quartile value, and the minimum value contained in the box and whisker.

	* Click and holding the left mouse button while dragging the mouse will zoom into the highlighted graphical area.  There are no known limits to zooming.  A *Reset zoom* button will appear in the top right hand corner of the plotting area if zoomed.  Pressing that button will reset the plot to the default size.

	* Outliers are shown as black circles above or below the box and whisker.  Hovering over the outlier provides the x and y coordinates of the circle.  The y coordinate represents the value of the outlier.
	
	* Currently, DAME can only implement the default highcharter color setting.

	* There are three horizontal lines at the very top right hand corner of the graphical area that provide downloadable options if clicked.  Current options include:

		* Printing Chart
		
		* Download image as either PNG, JPEG, SVG, and PDF.  NOTE: Selecting PDF option tends to crash DAME.
		
		* Download plotting data in either CSV or XLS format.  Only provides data from selected plot.  
		
		* Display data table will render a HTML table below plot with plotting data.	
		
	* This app uses [Highsoft software](https://shop.highsoft.com/faq/non-commercial) with non-commercial packages. Highsoft software product is not free for commercial use.
