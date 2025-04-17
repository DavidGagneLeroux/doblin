## ----include = FALSE----------------------------------------------------------

knitr::opts_knit$set(
              self.contained = TRUE)
knitr::opts_chunk$set(
  #collapse = TRUE,
  dpi = 55,
  fig.retina = 1,
  comment = "#>"
  )
  

## ----eval=FALSE, collapse=TRUE------------------------------------------------
#  Rscript ./main.R -t 0.0005 -o ~/Documents/ -n test -i ~/Documents/test.csv -c 12

## ----eval=FALSE, collapse=TRUE------------------------------------------------
#  [1] "Step 0: Processing CSV file..."
#  Do you want to plot the dynamics of your dataset?(y/n): y

## ----eval=FALSE, collapse=TRUE------------------------------------------------
#  [1] "Step 1: Plotting the dynamics..."
#  [1] "1.1 Reshaping input file into long-format dataframe..."
#  [1] "1.2 Retrieving the first 1000 barcodes with the highest maximum frequencies..."
#  [1] "1.3 Assigning colors to lineages having reached the minimum frequency threshold
#  among the 1000 most dominant barcoded lines..."

## ----eval=FALSE, collapse=TRUE------------------------------------------------
#  Plot a log-scale model, a linear-scale model or both? (logarithmic/linear/both): both
#  [1] "Plotting in progress..."

## ----eval=FALSE, collapse=TRUE------------------------------------------------
#  Do you want to plot the diversity of your dataset?(y/n): y
#  [1] "2.1 Calculating the diversity..."
#  [1] "2.2 Plotting the diversity..."

## ----eval=FALSE, collapse=TRUE------------------------------------------------
#  [1] "Step 3: Clustering..."
#  Specify a minimum mean frequency below which lineages are not taken into account during
#  clustering (ex: 0.00005): 0.00005
#  [1] "3.1 Filtering the input data..."

## ----eval=FALSE, collapse=TRUE------------------------------------------------
#  [1] "3.2 Clustering the filtered data..."
#  Enter a linkage method (ex: average): average
#  Enter the metric to be used to measure similarity between two time-series (pearson/dtw):
#    pearson
#  Enter a method for computing covariances in the presence of missing values.
#  Please refer to stats::cor() R documentation. (ex: pairwise.complete.obs):
#    pair

## ----eval=FALSE, collapse=TRUE------------------------------------------------
#  [1] "3.2 Clustering the filtered data..."
#  Enter a linkage method (ex: average): average
#  Enter the metric to be used to measure similarity between two time-series (pearson/dtw):
#    dtw
#  Enter the norm for the local distance calculation
#  ('L1' for Manhattan or 'L2' for (squared) Euclidean): L2

## ----eval=FALSE, collapse=TRUE------------------------------------------------
#  [1] "3.2.1 Computing the relative clusters for ALL thresholds between 0.1 and maximum
#  height of hierarchical clustering... "
#  [1] "3.2.2 Filtering the hierarchical clustering results..."
#  Enter the minimum number of members per cluster for test : 8
#  By ignoring clusters with fewer than 8  members, you are potentially ignoring dominant
#  clusters. Please indicate a minimum average frequency that must be reached by at least
#  one of the lines of potentially ignored clusters for them to be taken into account:
#    0.001

## ----eval=FALSE, collapse=TRUE------------------------------------------------
#  [1] "3.2.3 Quantifying the hierarchical clustering..."
#  3.2.4 Enter the chosen threshold for the clustering of test : 0.3

## ----eval=FALSE, collapse=TRUE------------------------------------------------
#  [1] "3.2.5 Plotting the resulting clusters..."
#  [1] "DONE"

