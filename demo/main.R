#!/usr/bin/env Rscript

## A step-by-step demonstration to understand the pipeline
## ex usage: Rscript ./main.R -x 1000 -t 0.0005 -o ~/Documents/ -s M1,M2 -i ./M1_input.csv,./M2_input.csv -c 14

## Input file format: a csv file containing the barcode extraction results over 3 columns: ID, Time, Reads
## ID: consensus sequence that identifies a group of barcodes
## Time: integer representing the time at which the data was measured.
## Please make sure your time points start at least at t=1. If your input file
## contains time points starting at t=0, extract them in a separate file.
## This newly created file will be used as initial barcode library.
## If the user doesn't have an initial barcode library, the time points should at least start at t=1.
## Reads: number of barcodes that were computed at a certain time for a certain consensus sequence

## Install missing packages
list.of.packages <- c("grid", "ggthemes", "ggplot2", "magrittr", "dplyr", "ggnewscale",
                      "readr", "data.table", "reshape2", "grDevices", "doblin",
                      "optparse", "egg", "ggpubr", "stats", "imputeTS", "data.table", "dtwclust",
                      "purrr", "tidyr", "TSdist")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the library
library(doblin)
library(optparse)

## Parse arguments from command line
options <- list(
  make_option(c("-x", "--intersection"), action = "store", type="integer", default=1000, help="Number of top barcodes to fetch [default %default]"),
  make_option(c("-t", "--treshold"), action = "store", type="double", default=0.0005, help="Limit frequency above which barcodes have assigned colors [default %default]"),
  make_option(c("-o", "--outputPath"), action = "store", type="character", default=getwd(), help="Output directory [default %default]"),
  make_option(c("-s", "--sampleNames"), action = "store", type="character", help="Sample's name or ID. If multiple: use comma to separate"),
  make_option(c("-i", "--inputFiles"), action = "store", type="character", help="Input csv file . If multiple: use comma to separate"),
  make_option(c("-c", "--timeCut"), action = "store", type="integer", help="Time point threshold (keep barcodes with at least x non-zero time points)")
)
arguments <- parse_args(OptionParser(option_list = options))

## Test if there is at least one argument: if not, return an error
if (length(arguments)>= 7) {

  print("Processing the command line...")

  #n_intersect = 1000
  n_intersect = as.numeric(arguments$intersection)
  #max_freq_treshold = 0.0005
  max_freq_treshold= as.double(arguments$treshold)
  #output_directory = "~/Documents/"
  output_directory = arguments$outputPath
  #cohort_names = list("M1", "M2")
  cohort_names = as.list(strsplit(arguments$sampleNames, ",")[[1]])
  #input_files = list("./M1_input.csv", "./M2_input.csv")
  input_files = as.list(strsplit(arguments$inputFiles, ",")[[1]])
  #time_treshold = 14
  time_treshold = as.numeric(arguments$timeCut)

} else {

  stop("Missing arguments. Arguments must be supplied!", call.=FALSE)
}

## Step 0: Processing csv file(s)
print("Processing CSV files...")
input_data <- lapply(input_files, readr::read_csv)

## Step 1: Reshape input files into dataframes
print("Reshaping input files into dataframes...")
dataframes <- lapply(input_data, reshapeDF)

## Step 2: Fetch the top N barcodes of each dataframe
print("Fetching the top N barcodes of each dataframe...")
top_final <- list()
top_max <- list()
x <- lapply(dataframes, fetchTop, n_intersect)
for(element in x){
  top_final <-append(top_final, element[1])
  top_max <- append(top_max, element[2])
}

## Step 3: Color assignment for plotting
print("Assigning colors for dynamics plots...")

## create list of unique top.max barcodes across all data
all.top.max = unique(data.table::rbindlist(top_max))
all.top.max = all.top.max[,c(1,2)]
all.top.max = all.top.max[order(-all.top.max$max),]
dup.vec = duplicated(all.top.max$ID)
all.top.max = all.top.max[!dup.vec,]

## we limit the selection such that all barcodes with max frequency > 0.0005 are assigned a hex color
treshold.top.max <- all.top.max[all.top.max$max >= max_freq_treshold, ]

## we create a very long list of colors for low-frequency barcodes.
## repetition of colors isn't an issue, we just want each barcodes to be colored instead of assigning gray values
top_colors <- readr::read_csv("../inst/extdata/top_colors2.csv")
long.color.list = rep(top_colors$hex,50)
long.color.list.random = sample(long.color.list)

## As a result, all top max frequencies > treshold are assigned a hex color
top_colors = top_colors[1:length(treshold.top.max[[1]]),]
treshold.top.max = cbind(treshold.top.max,top_colors)

## Step 4: Plot dynamics
print("Plotting the dynamics...")
cat("Regarding the dynamics, do you want a linear model, a logarithmic model or both? (linear, log or both): ")
#plot_choice <- "log"
plot_choice <- readLines("stdin", n=1)
#TODO: Handle multiple labels?
for(i in 1:length(dataframes)) {
  plotDynamics(dataframes[[i]], max_freq_treshold, cohort_names[[i]], plot_choice)
  }

## Step 5: Calculate diversity
print("Calculating the diversities...")
diversities <- lapply(input_data, calculate_diversity)
for (i in 1:length(dataframes)){
  diversities[[i]]$Sample = cohort_names[[i]]
}

## Step 6: Plot diversities
print("Plotting the diversities...")
sample_diversities <- data.table::rbindlist(diversities)

plotDiversity(sample_diversities, cohort_names)

## Step 7: Spearman Correlation
cat("Do you have an initial barcode library (y/n) : ")
#exist_library <- "y"
exist_library <- readLines("stdin", n=1)
if(exist_library == "y"){
  cat("Enter your reference file (ex: {path_to_file}/ref_file.csv) : ")
  #ref_file <- "./ref_time0.csv"
  ref_file <- readLines("stdin", n=1)
  ## 7.1: Plot the barcode frequencies from the initial library.
  initial_barcode_library <- readr::read_csv(ref_file)
  barcode_library <- plotInitFreq(initial_barcode_library)
  barcode_library$Time = NULL
  colnames(barcode_library)[2]="Reads_time0"
  initial_barcode_library_topN = initial_barcode_library[1:n_intersect,]

  ## 7.2: Create a running scatterplot of each sample's barcode frequencies
  ## at all time points vs the barcode frequencies at time 0.
  sample_indices <- list()
  for(i in 1:length(input_data)) {
    sample_index <- plotCorrelation(input_data[[i]], barcode_library, cohort_names[[i]])
    sample_indices <- append(sample_indices, list(sample_index))
  }

  ## 7.3: Calculate the running frequency correlations with initial library for all samples
  ## and plot the result.

  sample_correlations <- list()
  for(i in 1:length(sample_indices)) {
    sample_corr <- calculateSpearmanCorr(sample_indices[[i]], cohort_names[[i]])
    sample_correlations <- append(sample_correlations, list(sample_corr))
  }
  sample_correlations <- do.call(rbind, sample_correlations)
  plotSpearmanCorr(sample_correlations)

  ## Step 8: Jaccard Index
  plotJaccardIndex(top_final, top_max, initial_barcode_library_topN, cohort_names)



}else if(exist_library == "n"){
  print("Skipping initial library...")
}else{
  stop("Error: Incorrect input")
}

##############################################
## Step 9: Clustering
print("Clustering")
## 9.1: Filtering data
print("Filtering input data...")

filtered_dataframes <- list()
for(i in 1:length(input_data)) {
  filtered_df <- filterData(input_data[[i]], time_treshold, cohort_names[[i]])
  filtered_dataframes <- append(filtered_dataframes, list(filtered_df))
}

## 9.2: Clustering with Pearson & DTW + threshold selection depending on distance
## between clusters & cluster number
cat("Enter a linkage method (methods from https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/hclust ): ")
#linkage <- "average"
linkage <- readLines("stdin", n=1)
cat("Enter a clustering method (pearson or dtw) : ")
#clustering <- "pearson"
clustering <- readLines("stdin", n=1)
cat("Enter a method to compute the covariance (ex: pairwise.complete.obs) : ")
#covariance <- "pairwise.complete.obs"
covariance <- readLines("stdin", n=1)

for(i in 1:length(filtered_dataframes)) {
  clusters_df = get_clonal_lineage_clusters(filtered_dataframes[[i]], cohort_names[[i]], linkage, clustering, covariance)
  plotHCQuantification(filtered_dataframes[[i]], cohort_names[[i]], clusters_df)
}

## breaks are essential for plots
breaks <- list()
for(i in 1:length(input_data)){
  breaks <-append(breaks, list(sort(c(unique(input_data[[i]]$Time)))))
}

#selected_threshold = as.numeric(readline(prompt = "Enter the chosen threshold : "))
cat("Enter the chosen threshold : ")
#selected_threshold <-0.5
selected_threshold <- as.numeric(readLines("stdin", n=1))

for(i in 1:length(filtered_dataframes)) {

  selected_clusters = get_clonal_lineage_clusters(filtered_dataframes[[i]], cohort_names[[i]], linkage, clustering, covariance, selected_threshold)
  plot_clusters_and_loess(filtered_dataframes[[i]], selected_clusters, cohort_names[[i]], breaks[[i]])
}

