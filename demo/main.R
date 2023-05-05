#!/usr/bin/env Rscript

## A step-by-step demonstration to understand the pipeline
## ex usage: Rscript ./main.R -x 1000 -t 0.0005 -o ~/Documents/ -s M1,M2 -i ./M1_input.csv,./M2_input.csv -c 14

## Input file format: a csv file containing the barcode extraction results over 3 columns: ID, Time, Reads
## ID: consensus sequence that identifies a group of barcodes.
## Time: integer representing the time at which the data was measured.
## If your input file contains time points starting at t=0, please extract them to a separate file.
## This newly created file requires the same format as the input file i.e. with 3 columns: ID, Time, Reads.
## It will be used as initial barcode library.
## If the user doesn't have an initial barcode library, the time points should at least start at t=1.
## Reads: number of barcodes that were counted at a certain time for a certain consensus sequence.

## Install missing packages
list.of.packages <- c("grid", "ggthemes", "ggplot2", "magrittr", "dplyr", "ggnewscale",
                      "readr", "data.table", "reshape2", "grDevices", "doblin",
                      "optparse", "egg", "ggpubr", "stats", "imputeTS", "data.table", "dtwclust",
                      "purrr", "tidyr", "TSdist")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## N.B.: Load the commented libraries if you run main.r manually
library(doblin)
library(optparse)
# library(ggplot2)
# library(dplyr)
# library(magrittr)
# library(ggpubr)

## Parse arguments from command line
options <- list(
  make_option(c("-x", "--intersection"), action = "store", type="integer", default=1000, help="Number of top barcodes to fetch [default %default]"),
  make_option(c("-t", "--threshold"), action = "store", type="double", default=0.0005, help="Limit frequency above which barcodes have assigned colors [default %default]"),
  make_option(c("-o", "--outputPath"), action = "store", type="character", default=getwd(), help="Output directory [default %default]"),
  make_option(c("-s", "--sampleNames"), action = "store", type="character", help="Sample's name or ID. If multiple: use comma to separate"),
  make_option(c("-i", "--inputFiles"), action = "store", type="character", help="Input csv file . If multiple: use comma to separate"),
  make_option(c("-c", "--timeCut"), action = "store", type="integer", help="Time point threshold (keep barcodes with at least '-c' non-zero time points)")
)
arguments <- parse_args(OptionParser(option_list = options))

## Test if there is at least seven arguments`: if not, return an error
if (length(arguments)>= 7) {

  print("Processing the command line...")

  #n_intersect = 1000
  n_intersect = as.numeric(arguments$intersection)
  #min_freq_threshold = 0.0005
  min_freq_threshold= as.double(arguments$threshold)
  #output_directory = "~/Documents/articles/z2r1_gen800_0.01/"
  #output_directory = "~/Documents/articles/m1/"
  output_directory = arguments$outputPath
  #cohort_names = list("Z2r1")
  #cohort_names = list("m1")
  cohort_names = as.list(strsplit(arguments$sampleNames, ",")[[1]])
  #input_files = list("./demo/Z2_r1_popZ_0.01_gen800.csv")
  #input_files = list("./demo/M1_input.csv")
  input_files = as.list(strsplit(arguments$inputFiles, ",")[[1]])
  #time_threshold = 10
  time_threshold = as.numeric(arguments$timeCut)

} else {

  stop("Missing arguments. Arguments must be supplied!", call.=FALSE)
}

## Step 0: Processing csv file(s)
print("Processing CSV files...")
input_data <- lapply(input_files, readr::read_csv)

# FOR CARTOON
# input_data[[1]][input_data[[1]]$ID == 'CTTCTGGCCACAATCC',]$Reads[12] = 3000000
# input_data[[1]][input_data[[1]]$ID == 'CTTCTGGCCACAATCC',]$Reads[13] = 2000000
# input_data[[1]][input_data[[1]]$ID == 'CTTCTGGCCACAATCC',]$Reads[14] = 1000000
# input_data[[1]][input_data[[1]]$ID == 'CTTCTGGCCACAATCC',]$Reads[15] = 500000
# input_data[[1]][input_data[[1]]$ID == 'CTTCTGGCCACAATCC',]$Reads[16] = 300000
# input_data[[1]][input_data[[1]]$ID == 'CTTCTGGCCACAATCC',]$Reads[17] = 100000

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

## we limit the selection such that all barcodes with max frequency > '-t' are assigned a hex color
threshold.top.max <- all.top.max[all.top.max$max >= min_freq_threshold, ]

## we create a very long list of colors for low-frequency barcodes.
top_colors <- readr::read_csv("./inst/extdata/top_colors2.csv")
long.color.list = rep(top_colors$hex,50)
long.color.list.random = sample(long.color.list)

## As a result, all top max frequencies > '-t' are assigned a hex color
top_colors = top_colors[1:length(threshold.top.max[[1]]),]
threshold.top.max = cbind(threshold.top.max,top_colors)

## Step 4: Plot dynamics
print("Plotting the dynamics...")
cat("Regarding the dynamics, do you want a linear model, a logarithmic model or both? (linear, log or both): ")
#plot_choice <- "both"
plot_choice <- readLines("stdin", n=1)
plot_choice <- match.arg(plot_choice, c("log", "linear", "both"))

for(i in 1:length(dataframes)) {
  plotDynamics(dataframes[[i]], min_freq_threshold, cohort_names[[i]], plot_choice)
  }

# ## Step 5: Calculate diversity
# print("Calculating the diversities...")
# diversities <- lapply(input_data, calculate_diversity)
# for (i in 1:length(dataframes)){
#   diversities[[i]]$Sample = cohort_names[[i]]
# }
#
# ## Step 6: Plot diversities
# print("Plotting the diversities...")
# sample_diversities <- data.table::rbindlist(diversities)
#
# plotDiversity(sample_diversities, cohort_names)
#
# ## Step 7: Spearman Correlation
# cat("Do you have an initial barcode library (y/n) : ")
# #exist_library <- "y"
# exist_library <- readLines("stdin", n=1)
#
# if(exist_library == "y"){
#   cat("Enter your reference file (ex: {path_to_file}/ref_file.csv) : ")
#   #ref_file <- "./demo/ref_time0.csv"
#   ref_file <- readLines("stdin", n=1)
#   ## 7.1: Plot the barcode frequencies from the initial library.
#   initial_barcode_library <- readr::read_csv(ref_file)
#   barcode_library <- plotInitFreq(initial_barcode_library)
#   barcode_library$Time = NULL
#   colnames(barcode_library)[2]="Reads_time0"
#   initial_barcode_library_topN = initial_barcode_library[1:n_intersect,]
#
#   ## 7.2: Create a scatter-plot of each sample's barcode frequencies
#   ## at all time points vs the barcode frequencies at time 0.
#   sample_indices <- list()
#   for(i in 1:length(input_data)) {
#     sample_index <- plotCorrelation(input_data[[i]], barcode_library, cohort_names[[i]])
#     sample_indices <- append(sample_indices, list(sample_index))
#   }
#
#   ## 7.3: Calculate the Spearman correlations between the initial library and all samples
#   ## and plot the result.
#
#   sample_correlations <- list()
#   for(i in 1:length(sample_indices)) {
#     sample_corr <- calculateSpearmanCorr(sample_indices[[i]], cohort_names[[i]])
#     sample_correlations <- append(sample_correlations, list(sample_corr))
#   }
#   sample_correlations <- do.call(rbind, sample_correlations)
#   plotSpearmanCorr(sample_correlations)
#
#   ## Step 8: Calculate the Jaccard Index between the initial library and all samples
#   plotJaccardIndex(top_final, top_max, initial_barcode_library_topN, cohort_names)
#
#
#
# }else if(exist_library == "n"){
#   print("Skipping initial library...")
# }else{
#   stop("Error: Incorrect input")
# }

## Step 9: Clustering
print("Clustering")
## 9.1: Filtering data
print("Filtering input data...")

cat("Enter a minimum mean frequency below which the data is not retained (ex: 0.00005): ")
#freq_filter_threshold=0.00005
freq_filter_threshold <- as.numeric(readLines("stdin", n=1))

filtered_dataframes <- list()
for(i in 1:length(input_data)) {
  filtered_df <- filterData(input_data[[i]], freq_filter_threshold, time_threshold, cohort_names[[i]])
  filtered_dataframes <- append(filtered_dataframes, list(filtered_df))
}

## 9.2: Clustering with Pearson & DTW + threshold selection depending on distance
## between clusters & cluster number
cat("Enter a linkage method (ex: average): ")
#linkage <- "average"
linkage <- readLines("stdin", n=1)

cat("Enter a clustering method (pearson or dtw) : ")
#clustering <- "pearson"
clustering <- readLines("stdin", n=1)
clustering <- match.arg(clustering, c("pearson","dtw"))
if (clustering=="pearson") {
  cat("Enter a method to compute the covariance (ex: pairwise.complete.obs) : ")
  #covariance <- "pair"
  covariance <- readLines("stdin", n=1)
  covariance <- match.arg(covariance, c("everything","all.obs","complete.obs","na.or.complete","pairwise.complete.obs"))
}else{
  covariance = NULL
}


print("Computing the relative clusters for ALL thresholds between 0.1 and max height of HC... ")
for(i in 1:length(filtered_dataframes)) {
  clusters_df = perform_hierarchical_clustering(filtered_dataframes[[i]], cohort_names[[i]], linkage, clustering, covariance)

  cat(paste("Enter the minimum number of members per cluster for", cohort_names[[i]], ": "))
  #min_members <-8
  min_members <- as.numeric(readLines("stdin", n=1))

  plotHCQuantification(filtered_dataframes[[i]], clusters_df, cohort_names[[i]], min_members)
}

breaks <- list()
for(i in 1:length(input_data)){
  breaks <-append(breaks, list(sort(c(unique(input_data[[i]]$Time)))))
}



for(i in 1:length(filtered_dataframes)) {

  cat(paste("Enter the chosen threshold for", cohort_names[[i]], ": "))
  #selected_threshold <-0.7
  selected_threshold <- as.numeric(readLines("stdin", n=1))

  selected_clusters = perform_hierarchical_clustering(filtered_dataframes[[i]], cohort_names[[i]], linkage, clustering, covariance, selected_threshold)
  plot_clusters_and_loess(filtered_dataframes[[i]], selected_clusters, cohort_names[[i]], breaks[[i]], min_members)
}

