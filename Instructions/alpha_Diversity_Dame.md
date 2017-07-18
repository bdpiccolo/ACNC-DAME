# α-Diversity

Current implementation of α-diversity calculations is handled through the estimate_richness() function from the [phyloseq package](https://joey711.github.io/phyloseq/index.html). Group differences are assessed with the aov() function from the stats package and shown graphically with interactive bar plots using the [highcharter package](https://github.com/jbkunst/highcharter).  ANOVA results are provided alongside of bar plot(s) in a tabular format using the [DT package] (https://github.com/rstudio/DT).

# Getting Started

There are 4 widgets that are initially displayed when selecting the Alpha-Diversity tab.  The first 3 allow for selections of taxonomic level(s), α-diversity parameter(s), and experimental group(s).  There are 2 additional widgets that are displayed if > 1 experimental groups are selected.  All of these widgetes are managed by the *Finalize α-diversity* button.  Pressing the *Finalize α-diversity* button will finalize the analysis and render downloadable options, graphics, and ANOVA results.

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_adiv_getting_started.png?raw=true)

1. Select Taxonomic Level(s):

	* Can select 1 or more taxonomic levels (Phylum, Class, Order, Family, Genus, OTU)
	
	* Taxa selections do not need to be in the hierarchical order
	
	* Selecting OTU will use the uploaded OTU data, while all other taxa selections will merge the OTU count data into the selected taxa using the tax_glom() function from the phyloseq package.  It takes longer to compute genus relative to family and so forth (genus > family > order > class > phylum)
	
2. Select α-Diversity Parameters:

	* Can select 1 or more α-Diversity indices available. 
	
	* Defaults selections to 'Observed' index
	
	* Currently, DAME implements the following α-Diversity measurements (Total Observed, Chao1, ACE, Shannon, Simpson, Inverse Simpson, and Fisher)	
	
3. Select groups for statistical analysis:

	* Can select 1 or more groups if available.  Available groups are dependent on Metadata file and final data selections from Import Data tab.
	
	* Groups will not be displayed in widget if data was finalized with only 1 group remaining.
	
	* Defaults selections to alphabetical order

	* Choosing > 1 groups will open additional widgets 
		
		1. Select Type of ANOVA
		
			* Currently, DAME implements either a 1-way or multifactor ANOVA for assessing group differences with > 1 selected groups.
			
			* Defaults to 1-way ANOVA
			
			* Selection of 1-way ANOVA will concatenate the factor levels of each group and then use a single grouping factor within the aov() function.
			
			* Selection of a multifactor ANOVA will assess main effects of selected groups with interaction using the aov() function.
			
		2. Dispersion
			
			* Selects dispersion method for output table.
			
			* Defaults to Standard Error Mean; Standard Deviation can be selected.
		
4. Finalize α-Diversity control button:

	* Press this button after making selections from the previous widgets.
	
	* Triggers calculations and analyses.
	
	* Renders bar plot and ANOVA table of selected taxonomic level(s).
	
	* Renders advances options for highcharter plots.
	
	* Renders hyperlinks for downloading plotting data and α-diversity calculations
	
	* Can be pressed at anytime and will re-calculate α-diversity calculations and ANOVA(s), and then render newly selected taxonomic levels if pressed again.	

# Downloading Options

Hyperlinks will appear below the initial set of widgets with options to download sample-wise α-diversity calculations and plotting data. 

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_adiv_download_opts.png?raw=true)

1. Show/hide Plotting Data Downloading Options

	* Clicking on the hyperlink, *Show/hide Plotting Data Downloading Options* will reveal a character input box and a control button.

	* Can choose name of file by inputting text into Choose File Name widget.  
	
	* Pressing the *Download Raw Data* button will download a .csv file with the α-diversity selection(s), group mean, group SEM/SD, ANOVA p-value, and taxonomic level(s).
	
2. Show/hide α-Diversity Data Downloading Options

	* Clicking on the hyperlink, *Show/hide α-Diversity Data Downloading Options* will reveal a character input box and a control button.

	* Can choose name of file by inputting text into Choose File Name widget.  
	
	* Pressing the *Download α-Diversity Data* button will download a .csv file with the sample ID, group(s), α-diversity calculations, and taxanomic levels.

# Advanced highcharter options

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_adiv_advplot_opts.png?raw=true)

	* Clicking on the hyperlink, *Show/hide advanced highcharter options*, will render 2 additional widgets.  This hyperlink is below the 2 hyperlinks for downloading options.  
	
		1. Type
		
			* Select the plotting direction of the bars in the graph.
			
			* Defaults to Vertical.  Horizontal can be selected.
			
		2. Theme
		
			* Various themes are available from the highcharter library.

			* Defaults to the *Default* theme.  
			

# Results

For each taxonomic level selected there is an interactive bar plot and ANOVA table rendered below the input widgets.  Rendering for each taxonomic level(s) is separated by a line break (horizontal rule), so each taxonomic "section" contains a graphical and tabular output.  Each taxonomic section contains a header that identifies which taxonomic level is displayed.  Rendering of the taxonomic sections will always follow the hierarchical ordering, regardless if the selections in the Select Taxonomic Levels input widget are not in order. 

# Bar Plots

An interactive bar plot is rendered within each taxonomic section using the highcharter package.  The bar plot consists of a bars which are the mean of the levels in the selected experimental group(s).  All α-diversity selections will render on a single graphical output, so be mindful of units.  If > 1 groups are selected, then the groups will concatenate the factor levels and display a bar for the combined levels.

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_adiv_plot_interactivity.png?raw=true)

More information regarding the highcharter package can be found [here](http://jkunst.com/highcharter/).  Plotting interactivity (e.g., hovering text box, group selections, etc.) in highcharter plots within a taxonomic section are independent of others if multiple taxonomic levels are rendered.  Interactive functionality in DAME is described herein.

* Hovering the mouse cursor over a bar will provide additional text.  Currently, DAME will provide the α-diversity parameter, the group level, and the mean α-diversity measurement.

* Click and holding the left mouse button while dragging the mouse will zoom into the highlighted graphical area.  There are no known limits to zooming.  A *Reset zoom* button will appear in the top right hand corner of the plotting area if zoomed.  Pressing that button will reset the plot to the default size.

* The color of the bars are based on the default color scheme in highcharter and are given as filled circles along with the corresponding group level in the legend at the top of the plotting graphic.  Clicking on the legend circle or group level will either remove or add the group to the plot.  The plot will resize in real time based on the these selections.

* There are three horizontal lines at the very top right hand corner of the graphical area that provide downloadable options if clicked.  Current options include:

	* Printing Chart
	
	* Download image as either PNG, JPEG, SVG, and PDF.  NOTE: Selecting PDF option tends to crash DAME.
	
	* Download plotting data in either CSV or XLS format.  Only provides data from selected plot.  Comprehensive plotting data is provided in Downloading Options hyperlink above.
	
	* Display data table will render a HTML table below plot with plotting data.	
		
	* This app uses [Highsoft software](https://shop.highsoft.com/faq/non-commercial) with non-commercial packages. Highsoft software product is not free for commercial use.
	
# ANOVA table

Currently, DAME utilizes ANOVA to assess group differences in a-diversity calculations using the aov() function.  between groups. 

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_adiv_ANOVA_table.png?raw=true)

The ANOVA output is generated using the [DT](https://rstudio.github.io/DT/) package and provides group calculations of Degrees of Freedom, Sequential Sums of Squares, Mean Squares, F-Statistic, Partial R-Squared, and P-value.  Residual and Total calculations are also provided in the table.  Interactive functions are provided herein.

* The table can be downloaded as either an Excel, PDF, or CSV file.  Buttons are provided at the top of the table for each file option.

* Each column can be sorted.  Clicking the column label will first order the table by the decreasing order of the column.  Clicking the same column again will re-order the table by the increasing order of the column.
