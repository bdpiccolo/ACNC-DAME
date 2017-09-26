# β-Diversity

β-diversity is an estimate of biodiversity, also referred to as between sample diversity. Dissimilarity and distance indices are commonly used to estimate β-diversity. Dissimilarity based measurements (Bray-Curtis and Jaccard) are commonly used in microbial analyses. β-diversity is visualized with by data reduction techniques that are collectively referred to as ordinations.  Ordinations  work by summarizing the inherent variance found within a dataset and then projecting the results into a lower dimensional plot (e.g., scatterplot) where similar samples will cluster together and dissimilar samples will distance themselves from one another. Principal Co-ordinate Analysis (PCoA), and Non-multi Dimensional Scaling (NMDS) are commonly used in microbial analyses.

More in depth review can be found [here](http://www.sciencedirect.com/science/article/pii/S0092867414008642?via%3Dihub\).  Description of non-phylogenetic β-diversity measurements can be found [here](https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/vegdist).  Description of phylogenetic β-diversity measurements can be found [here](https://rdrr.io/bioc/phyloseq/man/distance.html).  Description of ordinations can be found [here](https://www.rdocumentation.org/packages/phyloseq/versions/1.16.2/topics/ordinate).  


# Overview

Current implementation of β-diversity calculations and ordinations are handled through the ordinate() function from the [phyloseq package](https://joey711.github.io/phyloseq/index.html). Ordinations numerically calculate how similar or dissimilar samples are from each other and these calculations can then be graphically demonstrated in a scatterplot.  At this time, only the first two components of ordinations are available for visualization and rendered with the [scatterD3](https://github.com/juba/scatterD3) package.  Group comparisons of β-diversity is assessed with permutational multivariate ANOVA (PERMANOVA) of a dissimilarity or distance matix using the adonis2() function from the [vegan](https://github.com/vegandevs/vegan) package.

# Getting Started

There are 9 widgets that are initially displayed when first selecting the Beta-Diversity tab, but only the first 4 are necessary to render the 2-dimensional ordinations scatterplot and PERMANVOA table.  The remaining widgets influence the the 2-dimensional ordination plots after rendering.

![](https://github.com/bdpiccolo/ACNC-DAME/blob/master/Instructions/Images/DAME_bdiv_getting_started_92517.png?raw=true)

1. Select Taxonomic Level(s):

	* Can select 1 or more taxonomic levels (Phylum, Class, Order, Family, Genus, OTU)
	
	* Taxa selections do not need to be in the hierarchical order
	
	* Selecting OTU will use the uploaded OTU data, while all other taxa selections will merge the OTU count data into the selected taxa using the tax_glom() function from the phyloseq package.  It takes longer to compute genus relative to family and so forth (genus > family > order > class > phylum)

2. Select Group(s) for PERMANOVA Analyses:

	* Can select 1 or more groups if available.  Available groups are dependent on Metadata file and final data selections from Import Data tab.
	
	* Groups will not be displayed in widget if data was finalized with only 1 group remaining.
	
	* Defaults selections to alphabetical order
	
3. Choose number of permutations for PERMANOVA:

	* Defaults to 500
	
	* Ranges between 100 and 1000 by 100 units.  The greater the number of permutations the longer computation time.
		
4. Finalize β-diversity control button:

	* Press this button after making selections from the previous widgets.
	
	* Triggers calculations and analyses.
	
	* Renders 2-dimensional ordination plot and PERMANOVA table of selected taxonomic level(s).
	
	* Renders advances options for scatterD3 plots.
	
	* Can be pressed at anytime and will re-calculate β-diversity ordination(s) and PERMANOVA(s), and then render newly selected taxonomic levels if pressed again.

# Plotting Widgets

Although several plotting widgets are displayed when the β-diversity tab is first loaded, they are not functional and will reset to defaults after the Finalize β-diversity control button is pressed.  Each widget will return to its default setting when the Finalize β-diversity control button is re-pressed.

![](https://github.com/bdpiccolo/ACNC-DAME/blob/master/Instructions/Images/DAME_bdiv_plot_widgets_92617.png?raw=true)

1. Select β-Diversity Parameter(s):

	* Defaults to Bray-Curtis Dissimilarity index if a TRE file is not loaded (Defaults to Unweighted Unifrac distances if TRE file is loaded).
	
	* Currently, DAME implements the following β-Diversity measurements (Bray-Curtis, Jaccard, Jensen-Shannon, Mountford, Gower, Morisita, Horn, Kulczynski, Raup, Binomial, Maximum, Binary, Euclidean, Manhattan, Canberra, Minkowski)
	
	* DAME implements the following β-Diversity measurements when a TRE file is uploaded (Unweighted Unifrac, Weighted Unifrac, DPCOA) in addition to those mentioned above.
	
	* There can be considerable computational time with TRE based measurements, especially DPCOA.  Very dependent on the number of OTUs in the finalized data.
	
	* Selections will alter all plots in real time.
	
2. Select Ordination Method:

	* Defaults to Principal Coordinate Analysis.
	
	* Currently, DAME implements the following ordination methods (Principal Coordinate Analysis, Non-metric MultiDimensional Scaling, Multideminsional Scaling, Constrained Correspondence Analysis, Redundancy Analysis, Detrended Correspondence Analysis).
	
	* DAME implements the following ordination method when a TRE file is uploaded (Double Principal Coordinate Analysis) in addition those listed above.
	
	* Selections will alter all plots in real time.
	
3. Color Mapping Variable:

	* Defaults to first group in alphabetical order.
	
	* Selecting None will remove all colors.
	
	* Groups will not be displayed in widget if data was finalized with only 1 group remaining.
	
	* Selections will alter all plots in real time.
		
4. Shape Mapping Variable:

	* Defaults to None (i.e., will not discriminate any groups by shape).
	
	* Defaults selections to alphabetical order.
	
	* Groups will not be displayed in widget if data was finalized with only 1 group remaining.
	
	* Selections will alter all plots in real time.
	
5. Display/Toggle Confidence Ellipses:

	* Defaults to display confidence ellipses.
	
	* Unselecting box will remove confidence ellipses.
	
# Advanced scatterD3 Options

A hyperlink will appear below the Display/Toggle Confidence Ellipses widget after the Finalize β-diversity control button is pressed.  Clicking this hyperlink will provide 6 additional widgets that will alter the appearance of the scatterD3 plot.

![](https://github.com/bdpiccolo/ACNC-DAME/blob/master/Instructions/Images/DAME_bdiv_adv_scat_opt_92517.png?raw=true)

1. Size Mapping Variable:

	* Will alter the size of points in the scatterD3 plot(s).
	
	* Defaults to 100.
	
	* Ranges between zero (hides points) and 500, by 20 units.
	
	* Updates plots at all taxonomic levels in real time.
	
2. Points Opacity:

	* Will change the opacity of points in the scatterD3 plot(s).
	
	* Defaults to 1.
	
	* Ranges between zero (hides points) and 1, by 0.05 units.
	
	* Updates plots at all taxonomic levels in real time.

3. Add Labels:

	* Prints ID labels on points in the scatterD3 plot(s).
	
	* Defaults to no labels printed.  Check box to print ID labels on plot(s).
	
	* Can select the labels and size using the widgets directly below this widget.
	
4. Labels:

	* Select which labels will print in the scatterD3 plot(s).
	
	* Defaults to labels attached to the BIOM file (shown as IDs in widget dropdown box).
	
	* Other selections are labels from group selections in import tab.  Must have > 1 groups remaining to be option in this widget.
	
5. Label Size:

	* Will change the size of points in the scatterD3 plot(s).
	
	* Defaults to 11.
	
	* Ranges between 5 and 25, by 1 units.
	
	* Updates plots at all taxonomic levels in real time.	
	
# Results

For each taxonomic level selected there is a 2-dimensional ordination plot and a PERMANOVA table rendered below the input widgets.  Rendering for each taxonomic level(s) is separated by a line break (horizontal rule), so each taxonomic "section" contains a graphical and tabular output.  Each taxonomic section contains a header that identifies which taxonomic level is displayed.  Rendering of the taxonomic sections will always follow the hierarchical ordering, regardless if the input widget is not in order. 

# Ordination Plots

A 2-dimensional interactive ordination plot is rendered within each taxonomic section using the scatterD3 package.  The ordination plot consists of a scatterplot where each point represents an individual sample.  Samples that cluster closer together are considered more similar, whereas samples that are separated by large distances in the plot are considered more dissimilar.  Some ordinations will have the same calculations for very similar samples and may not be distinguishable.  

![](https://github.com/bdpiccolo/ACNC-DAME/blob/master/Instructions/Images/DAME_bdiv_ordination_plot_92517.png?raw=true)

More information regarding the scatterD3 package can be found [here](https://github.com/juba/scatterD3) and [here](https://cran.r-project.org/web/packages/scatterD3/vignettes/introduction.html).  Currently, DAME only supports the first 2 dimensions of the ordinations.  Plotting interactivity (e.g., zooming, shifting, etc.) in scatterD3 plots within a taxonomic section are independent of others if multiple taxonomic levels are rendered.  Interactive functionality in DAME is described herein.

* Hovering the mouse cursor over a point will provide additional text.  Currently, DAME provides the x- and y-coordinates and the color and shape mapping groups if selected (i.e., selecting None for color or shape mapping will not provide this text when hovered).

* Moving the mouse while the holding the left mouse button on the plotting canvas will shift the plotting area.  The plotting axes will update as the plotting area shifts.

* Using the mouse scroll wheel will cause the plotting window to zoom in and zoom out when the mouse cursor is hovering over the plotting window.  The axes will update automatically.

* Hovering the mouse cursor over the legend groups labels will highlight samples belonging to the hovered group (i.e., samples that do not belong to the hovered group will be come opaque).

* A Reset Zoom button is found below each scatterD3 plot and will reset the plot to its default settings.  This only applies to the interactive changes, e.g., zooming, shifting, etc.

* Currently, DAME is only able to download scatterD3 plots as a SVG file.  A Download SVG button is provided below each scatterD3 plot.
	
# PERMANOVA table

DAME uses PERMANOVA to identify group differences in dissimilarity or distance matrices.  Details of the method can be found [here](http://cc.oulu.fi/~jarioksa/softhelp/vegan/html/adonis.html).

![](https://github.com/bdpiccolo/ACNC-DAME/blob/master/Instructions/Images/DAME_bdiv_PERMANOVA_92517.png?raw=true)

The PERMANOVA output is generated using the [DT](https://rstudio.github.io/DT/) package and provides group calculations of Degrees of Freedom, Sums of Squares,F-Statistic, and P-value.  Residual calculations are also provided in the table.  Interactive functions are provided herein.

* The table can be downloaded as either an Excel, PDF, or CSV file.  Buttons are provided at the top of the table for each file option.

* Each column can be sorted.  Clicking the column label will first order the table by the decreasing order of the column.  Clicking the same column again will re-order the table by the increasing order of the column.

