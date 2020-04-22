# Data-Visualization-Wrapper-Function-using-R-ggplot2

Using R ggplot2 library, I created a set of functions that ingest variables of interest, and output the best visualizations. It takes dataframe object and names of variables of interest as arguments of function. The key idea is, different data types and number of variables of interest require different optimal visualization.

If...
1. only one variable is given,
  a. and it is categorical, the function outputs bargraph, piechart, waffle chart, and dot plot.
  b. and it is numeric, the function outputs histogram and density plot.
  
2. two variables are given,
  a. and both are numeric, the function outputs scatterplot with loess smoothing, with margin histograms/boxplots, and jitterplot.
  b. and one is categorical while another is numeric, the function outputs boxplot and violin plot.
  c. and both are categorical, the function outputs jitterplot, column chart, and mosaic chart. 
  
3. more than two variables are given (up to four max),
  a. it will add color/size variation to the plots above or use new facets.
  b. perspective plot and contour plot for all numeric case.

4. data is time series, the function outputs line plot. (e.g. US GDP growth)

5. data is matrix, the function outputs heatmap. (e.g. correlation matrix)
