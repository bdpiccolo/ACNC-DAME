# α-Diversity

α-Diversity is a measurement of biodiversity and has also described as the within sample diversity.  It provides information related to taxa presence, absence, richness, evenness, and other ecology based metrics.  DAME provides commonly used [α-diversity estimates](https://www.rdocumentation.org/packages/phyloseq/versions/1.16.2/topics/estimate_richness) from the [phyloseq](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0061217) package. 

* Observed: Unadjusted number of taxa observed in sample
	
* Chao1: Richness estimate that utilizes correction factor on observed taxa.  Robust with data with a high number of low abundant taxa ([Hughes et al., _Appl Environ Microbiol_, 2001](http://aem.asm.org/content/67/10/4399) ). 

* Abundance-based Coverage Estimator (ACE): Richness estimate that also utilizes correction factor on observed taxa.  Low sample sizes will cause both Chao1 and ACE to underestimate _true_ richness ([Hughes et al., _Appl Environ Microbiol_, 2001](http://aem.asm.org/content/67/10/4399) ). 
	
* Shannon Diversity Index: Takes into account abundance and evenness of taxa.  Assumes samples are randomly sampled and all taxa are represented ([Magguran, Measuring Biological Diversity, Wiley-Blackwell, 2003](http://www.wiley.com/WileyCDA/WileyTitle/productCd-0632056339.html) ).  
	
* Simpson's Index: Dominance measurement.  Weighted toward the most abundant taxa and less sensitive toward richness ([Magguran, Measuring Biological Diversity, Wiley-Blackwell, 2003](http://www.wiley.com/WileyCDA/WileyTitle/productCd-0632056339.html) ).  
	
* Inverse Simpson's Index: Reciprocal of Simpson's Index. More commonly used relative to Simpson's Index, but noted to have variance problems ([Magguran, Measuring Biological Diversity, Wiley-Blackwell, 2003](http://www.wiley.com/WileyCDA/WileyTitle/productCd-0632056339.html) ).  
	
* Fisher's alpha Index: Evenness measurement that assumes the taxa has a log-series distribution ([Fisher et al., _J Animal Ecol_, 1943](https://www.jstor.org/stable/1411?seq=1#page_scan_tab_contents]) ).

# Overview
	
Current implementation of α-diversity calculations is handled through the estimate_richness() function from the [phyloseq package](https://joey711.github.io/phyloseq/index.html). Group differences are assessed with the aov() function from the stats package and shown graphically with interactive plots using the [rbokeh package](http://hafen.github.io/rbokeh/).  ANOVA results are provided alongside of bar plot(s) in a tabular format using the [DT package](https://github.com/rstudio/DT).

# Getting Started

There are 5 widgets that are initially displayed when selecting the Alpha-Diversity tab.  The first 4 allow for selections of taxonomic level(s), α-diversity parameter(s), experimental group(s), and the statistical test used to compare groups.   All of these widgetes are managed by the *Finalize α-diversity* button.  Pressing the *Finalize α-diversity* button will finalize the analysis and render downloadable options, graphics, and ANOVA results.

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_adiv_getting_started_92517.png?raw=true)

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

	* Sets reactive selections for Statitical Test widget.

4. Select Statistical Test:

	* Selections are reactive based on the group selection(s).
	
	* If there is only 1 group selected with only 2 levels, then the selections will be either t-tests (parametric) or Mann Whitney U (non-parametric).
	
	* If there is only 1 group selected with > 2 levels, then the selections will be either 1-way ANOVA (parametric) or Kruskall Wallis Test (non-parametric).
	
	* If there are > 1 group selected, then the selections will be either 1-way ANOVA (parametric), Multi-factor ANOVA (parametric), or Kruskall Wallis test (non-parametric).
	
	* Sets reactive selections for Graphic Options widget.
		
5. Finalize α-Diversity control button:

	* Press this button after making selections from the previous widgets.
	
	* Triggers calculations and analyses.
	
	* Renders bar plot and ANOVA table of selected taxonomic level(s).
	
	* Renders hyperlinks for downloading plotting data and α-diversity calculations
	
	* Renders advances options for rbokeh plots.
	
	* Can be pressed at anytime and will re-calculate α-diversity calculations and ANOVA(s), and then render newly selected taxonomic levels if pressed again.	

# Downloading Option

Hyperlinks will appear below the initial set of widgets with options to download sample-wise α-diversity calculations. 

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_adiv_download_opts.png?raw=true)

1. Show/hide α-Diversity Data Downloading Options

	* Clicking on the hyperlink, *Show/hide α-Diversity Data Downloading Options* will reveal a character input box and a control button.

	* Can choose name of file by inputting text into Choose File Name widget.  
	
	* Pressing the *Download α-Diversity Data* button will download a .csv file with the sample ID, group(s), α-diversity calculations, and taxanomic levels.

# Advanced Options for Graphics

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_adiv_download_opts_92517.png?raw=true)

* Will only appear if parametric statistical tests are chosen above (e.g., 1-way ANOVA, Multi-factor ANOVA, and t-test)
	
* Clicking on the hyperlink, *Show/hide plotting options*, will render a single widgets.  
	
* Provides graphics representing the distribution of the data and visual assessments of parametric assumptions (e.g., normal distribution and heterogeneity of variance).
	
* If 1-way ANOVA or Multifactor ANOVA were chosen statistical test, widget will display Boxplot, Barplot, qqplot, and Fitted vs Residuals plot.
	
* If t-test is the chosen statistical test, widget will display Boxplot, Barplot, and qqplot.
	
* All plots default to Boxplot.  Boxplots are overlayed with jittered values.
	
* Selection of barplot will display means with standard error bars
	
* Selection of qqplot will display a probability plots of quantiles of a standard normal distribution vs the quantiles of standardized residuals in the case of ANOVA or sample values (t-test).  Points on the plot should follow a straight line and deviations from the line may suggest that the data is not normally distributed.  Non-normal distribution may suggest the need for a non-parametric test.
	
* Selection of Fitted vs Residuals Plot will display the estimated ANOVA values vs the residuals from the ANOVA model.  The points on the point should appear at random between the 0 value on the y-axis.  Visual identification of patterns in the fitted vs residual plots may suggest unequal variance across the range of values (heteroscedasiticity).  Presence of heteroscedasiticity may suggest the need for a non-parametric test.
		

# Results

For each taxonomic level selected there is an interactive graphic and ANOVA table rendered below the input widgets.  Rendering for each taxonomic level(s) is separated by a line break (horizontal rule), so each taxonomic "section" contains a graphical and tabular output.  Each taxonomic section contains a header that identifies which taxonomic level is displayed.  Rendering of the taxonomic sections will always follow the hierarchical ordering, regardless if the selections in the Select Taxonomic Levels input widget are not in order. The number graphics will depend on the number of α-diversity parameters selected.

# Box Plots

An interactive box plot(s) is rendered within each taxonomic section using the rbokeh package.  The box plot(s) consist of box-and-whisker plot(s) and are described [here](https://www.rdocumentation.org/packages/grDevices/versions/3.4.1/topics/boxplot.stats).  Raw values are overlayed on the boxplots.  A single graphic will appear when 1 α-diversity parameter is selected.  When multiple α-diversity selections are chosen, the selections will be shown in single plots in a two column format.  If > 1 groups are selected, then the groups will concatenate the factor levels and display a bar for the combined levels.

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_adiv_boxplot_interactivity_92517.png?raw=true)

More information regarding the rbokeh package and boxplots can be found [here](https://hafen.github.io/rbokeh/rd.html#ly_boxplot).  Plotting interactivity (e.g., hovering text box, zooming, etc.) in rbokeh plots within a taxonomic section are independent of others if multiple taxonomic levels are rendered.  Furthermore, interactivity within a single plot is independent of multiple plots within a taxonomic section.  Resets and save functions will affect multiple plots when within a single taxonomic section. Interactive functionality in DAME is described herein.

* Hovering the mouse cursor over a point will provide additional text.  Currently, DAME displays the group level and the α-diversity measurement.

* By default, click and holding the left mouse button while dragging the mouse will move the plotting canvas.  This function is designated by the arrow cross icon found on the top right hand corner of the graphic window.  A *Reset* button is also found on the upper right panel and will reset changes to all plots in the graphic window.

* There are two ways to zoom.  The panel in the top right hand corner has an icon with a magnifying glass in a box and another with a magnifying glass next to an oval. Pressing The former icon will allow zooming by mouse dragging while the latter will allow zooming with the mouse wheel.  Both zooming options are plot independent, i.e., will affect a single plot within a multi plot panel.   A *Reset* button is also found on the upper right panel and will reset changes to all plots in the graphic window.

* The graphic window can be saved in PNG format by clicking the floppy disk icon on the panel in the top right hand panel.  Please note that this will save all plots within the graphic space.
	
# Bar Plots

An interactive bar plot(s) is rendered within each taxonomic section using the rbokeh package.  The bar plot(s) consist of bars with standard error bars.  A single graphic will appear when 1 α-diversity parameter is selected.  When multiple α-diversity selections are chosen, the selections will be shown in single plots in a two column format.  If > 1 groups are selected, then the groups will concatenate the factor levels and display all levels within a single plot.

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_adiv_barplot_interactivity_92517.png?raw=true)

More information regarding the rbokeh package and bar plots can be found [here](https://hafen.github.io/rbokeh/rd.html#ly_bar).  Plotting interactivity (e.g., hovering text box, zooming, etc.) in rbokeh plots within a taxonomic section are independent of others if multiple taxonomic levels are rendered.  Furthermore, interactivity within a single plot is independent of multiple plots within a taxonomic section.  Resets and save functions will affect multiple plots when within a single taxonomic section. Interactive functionality in DAME is described herein.

* Hovering the mouse cursor over a bar will provide additional text.  Currently, DAME displays the group level and the mean α-diversity measurement.

* By default, click and holding the left mouse button while dragging the mouse will move the plotting canvas.  This function is designated by the arrow cross icon found on the top right hand corner of the graphic window.  A *Reset* button is also found on the upper right panel and will reset changes to all plots in the graphic window.

* There are two ways to zoom.  The panel in the top right hand corner has an icon with a magnifying glass in a box and another with a magnifying glass next to an oval. Pressing The former icon will allow zooming by mouse dragging while the latter will allow zooming with the mouse wheel.  Both zooming options are plot independent, i.e., will affect a single plot within a multi plot panel.   A *Reset* button is also found on the upper right panel and will reset changes to all plots in the graphic window.

* The graphic window can be saved in PNG format by clicking the floppy disk icon on the panel in the top right hand panel.  Please note that this will save all plots within the graphic space.

# QQ plots

An interactive qq plot(s) is rendered within each taxonomic section using the rbokeh package.  The qq plot(s) is a x and y scatter plot of quantiles of a normal distribution against quantiles of data.  QQ plots will only render if a parametric statistical test is selected.  A single graphic will appear when 1 α-diversity parameter is selected.  When multiple α-diversity selections are chosen, the selections will be shown in single plots in a two column format.  If > 1 groups are selected, then the groups will concatenate the factor levels and display all levels within a single plot.

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_adiv_qqplot_interactivity_92517.png?raw=true)

More information regarding the rbokeh package and point plots can be found [here](https://hafen.github.io/rbokeh/rd.html#ly_patch).  Plotting interactivity (e.g., hovering text box, zooming, etc.) in rbokeh plots within a taxonomic section are independent of others if multiple taxonomic levels are rendered.  Furthermore, interactivity within a single plot is independent of multiple plots within a taxonomic section.  Resets and save functions will affect multiple plots when within a single taxonomic section. Interactive functionality in DAME is described herein.

* Hovering the mouse cursor over a bar will provide additional text.  Currently, DAME displays the group level and the mean α-diversity measurement.

* By default, click and holding the left mouse button while dragging the mouse will move the plotting canvas.  This function is designated by the arrow cross icon found on the top right hand corner of the graphic window.  A *Reset* button is also found on the upper right panel and will reset changes to all plots in the graphic window.

* There are two ways to zoom.  The panel in the top right hand corner has an icon with a magnifying glass in a box and another with a magnifying glass next to an oval. Pressing The former icon will allow zooming by mouse dragging while the latter will allow zooming with the mouse wheel.  Both zooming options are plot independent, i.e., will affect a single plot within a multi plot panel.   A *Reset* button is also found on the upper right panel and will reset changes to all plots in the graphic window.

* The graphic window can be saved in PNG format by clicking the floppy disk icon on the panel in the top right hand panel.  Please note that this will save all plots within the graphic space.

# Fitted vs Residual plots

An interactive Fitted vs Residuals plot(s) is rendered within each taxonomic section using the rbokeh package.  The Fitted vs Residuals plot(s) is a x and y scatter plot of estimated ANOVA values against the standardized residuals from the ANOVA.  Fitted vs Residual plot(s) will only be displayed if ANOVA based tests are selected.  A single graphic will appear when 1 α-diversity parameter is selected.  When multiple α-diversity selections are chosen, the selections will be shown in single plots in a two column format.  If > 1 groups are selected, then the groups will concatenate the factor levels and display all levels within a single plot.

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_adiv_fvsrplot_interactivity_92517.png?raw=true)

More information regarding the rbokeh package and point plots can be found [here](https://hafen.github.io/rbokeh/rd.html#ly_patch).  Plotting interactivity (e.g., hovering text box, zooming, etc.) in rbokeh plots within a taxonomic section are independent of others if multiple taxonomic levels are rendered.  Furthermore, interactivity within a single plot is independent of multiple plots within a taxonomic section.  Resets and save functions will affect multiple plots when within a single taxonomic section. Interactive functionality in DAME is described herein.

* Hovering the mouse cursor over a bar will provide additional text.  Currently, DAME displays the group level and the mean α-diversity measurement.

* By default, click and holding the left mouse button while dragging the mouse will move the plotting canvas.  This function is designated by the arrow cross icon found on the top right hand corner of the graphic window.  A *Reset* button is also found on the upper right panel and will reset changes to all plots in the graphic window.

* There are two ways to zoom.  The panel in the top right hand corner has an icon with a magnifying glass in a box and another with a magnifying glass next to an oval. Pressing The former icon will allow zooming by mouse dragging while the latter will allow zooming with the mouse wheel.  Both zooming options are plot independent, i.e., will affect a single plot within a multi plot panel.   A *Reset* button is also found on the upper right panel and will reset changes to all plots in the graphic window.

* The graphic window can be saved in PNG format by clicking the floppy disk icon on the panel in the top right hand panel.  Please note that this will save all plots within the graphic space.

# ANOVA table

Currently, DAME utilizes ANOVA to assess group differences in a-diversity calculations using the aov() function.  between groups. 

![](https://raw.githubusercontent.com/bdpiccolo/ACNC-DAME/master/Instructions/Images/DAME_adiv_ANOVA_table_92517.png?raw=true)

The ANOVA output is generated using the [DT](https://rstudio.github.io/DT/) package and provides group calculations of Degrees of Freedom, Sequential Sums of Squares, Mean Squares, F-Statistic, Partial R-Squared, and P-value.  Residual and Total calculations are also provided in the table.  Interactive functions are provided herein.

* The table can be downloaded as either an Excel, PDF, or CSV file.  Buttons are provided at the top of the table for each file option.

* Each column can be sorted.  Clicking the column label will first order the table by the decreasing order of the column.  Clicking the same column again will re-order the table by the increasing order of the column.
