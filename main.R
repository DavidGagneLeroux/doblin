#!/usr/bin/env Rscript

## Doblin main script
## usage example: Rscript ./main.R -t 0.0005 -o [OUTPUT_DIR] -n [INPUT_FILE_NAME] -i [INPUT_FILE] -c 14

## -t: Minimum frequency above which barcodes are assigned colors [default: 0.0005]. This argument is used in plotDynamics().
## -o: Output directory [default: current working directory].
## -n: Input file name.
## -i: Input file.
## -c: Time point threshold. We cluster the lineages that persist for at least '-c' time points).

## Input file format: a csv file containing the barcode extraction results over 3 columns: ID, Time, Reads
## ID: consensus sequence that identifies a group of barcodes.
## Time: integer representing the time at which the data was measured.
## Reads: number of barcodes counted at a given time for a given consensus sequence.

## Install missing packages
list.of.packages <- c("grid", "ggthemes", "ggplot2", "magrittr", "dplyr", "ggnewscale",
                      "readr", "data.table", "reshape2", "grDevices", "doblin",
                      "optparse", "egg", "ggpubr", "stats", "imputeTS", "data.table", "dtwclust",
                      "purrr", "tidyr", "TSdist")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## N.B.: If you run main.r manually, load the commented libraries
library(doblin)
library(optparse)
# library(ggplot2)
# library(dplyr)
# library(magrittr)
# library(ggpubr)

## Parse arguments from command line
options <- list(
  make_option(c("-t", "--threshold"), action = "store", type="double", default=0.0005, help="Minimum frequency above which barcodes are assigned colors [default %default]"),
  make_option(c("-o", "--outputPath"), action = "store", type="character", default=getwd(), help="Output directory [default %default]"),
  make_option(c("-n", "--inputName"), action = "store", type="character", help="Input name"),
  make_option(c("-i", "--inputFile"), action = "store", type="character", help="Input csv file"),
  make_option(c("-c", "--timeCut"), action = "store", type="integer", help="Time point threshold (keep barcodes with at least '-c' non-zero time points)")
)
arguments <- parse_args(OptionParser(option_list = options))

## Test the number of arguments: if not enough, return an error
if (length(arguments)== 6) {

  print("Processing the command line...")

  min_freq_threshold= as.double(arguments$threshold)
  output_directory = arguments$outputPath
  input_name = arguments$inputName
  input_file = arguments$inputFile
  time_threshold = as.numeric(arguments$timeCut)

} else {

  stop("Missing arguments. Arguments must be supplied!", call.=FALSE)
}

## Step 0:
print("Step 0: Processing CSV file...")
input_dataframe <- readr::read_csv(input_file, show_col_types = FALSE)

## Step 1:
cat("Do you want to plot the dynamics of your dataset?(y/n): ")
plot_choice <- readLines("stdin", n=1)
plot_choice <- match.arg(plot_choice, c("yes", "no"))

if (plot_choice == "yes"){

  print("Step 1: Plotting the dynamics...")
  print("1.1 Reshaping input file into long-format dataframe...")
  reshaped_dataframe <- reshapeData(input_dataframe)

  ## Step 1.2:
  N_LINEAGES = 1000
  print(paste("1.2 Retrieving the first",N_LINEAGES,"barcodes with the highest maximum frequencies..."))
  top_N_maxFreq <- fetchTop(reshaped_dataframe, N_LINEAGES)

  ## Step 1.3:
  print("1.3 Assigning colors to lineages having reached the minimum frequency threshold among the 1000 most dominant barcoded lines...")

  ## All barcodes with (maximum frequency >= minimum frequency threshold) are assigned a hex color
  colored_top_freq <- top_N_maxFreq[top_N_maxFreq$max >= min_freq_threshold, ]

  ## we create a very long list of colors for low-frequency barcodes.
  COLOR_LIST <- readr::read_csv("./inst/extdata/top_colors2.csv", show_col_types = FALSE)
  LONG_COLOR_LIST_RAND = sample(rep(COLOR_LIST$hex,50)) # for dynamics (linear-scale plot)...

  COLOR_LIST = COLOR_LIST[1:length(colored_top_freq[[1]]),]
  colored_top_freq = cbind(colored_top_freq,COLOR_LIST)

  ## Step 1.4:
  cat("Do you want to plot a log-scale model, a linear-scale model or both? (logarithmic/linear/both): ")
  #plot_model <- "log"
  plot_model <- readLines("stdin", n=1)
  plot_model <- match.arg(plot_model, c("linear", "logarithmic", "both"))

  print("Plotting in progress...")
  plotDynamics(reshaped_dataframe, colored_top_freq, min_freq_threshold, plot_model)

}

## Step 2:
cat("Do you want to plot the diversity of your dataset?(y/n): ")
diversity_choice <- readLines("stdin", n=1)
diversity_choice <- match.arg(diversity_choice, c("yes", "no"))

if (diversity_choice == "yes"){

  print("2.1 Calculating the diversity...")
  diversity <- calculate_diversity(input_dataframe)

  print("2.2 Plotting the diversity...")
  plotDiversity(diversity)
}

####################################################

## Step 3:
print("Step 3: Clustering...")

cat("Specify a minimum mean frequency below which lineages are not taken into account during clustering (ex: 0.00005): ")
freq_filter_threshold <- as.numeric(readLines("stdin", n=1))

print("3.1 Filtering the input data...")
filtered_df <- filterData(input_dataframe, freq_filter_threshold, time_threshold)

## 3.2: Clustering with Pearson & DTW + threshold selection depending on distance
## between clusters & cluster number
print("3.2 Clustering the filtered data...")
cat("Enter a linkage method (ex: average): ")
linkage <- readLines("stdin", n=1)

cat("Enter the metric to be used to measure similarity between two time-series (pearson/dtw) : ")
similarity_metric <- readLines("stdin", n=1)
similarity_metric <- match.arg(similarity_metric, c("pearson","dtw"))

if (similarity_metric == "pearson") {
  cat("Enter a method for computing covariances in the presence of missing values. Please refer to stats::cor() R documentation. (ex: pairwise.complete.obs) : ")
  missing_values <- readLines("stdin", n=1)
  missing_values <- match.arg(missing_values, c("everything","all.obs","complete.obs","na.or.complete","pairwise.complete.obs"))
}else{
  missing_values = NULL
}

print("3.2.1 Computing the relative clusters for ALL thresholds between 0.1 and maximum height of hierarchical clustering... ")

clusters_df = perform_hierarchical_clustering(filtered_df, linkage, similarity_metric, missing_values)

print("3.2.2 Filtering the hierarchical clustering results...")

cat(paste("Enter the minimum number of members per cluster for", input_name, ": "))
min_members <- as.numeric(readLines("stdin", n=1))

list_filtering_results = filterHC(filtered_df, clusters_df, min_members)

clusters_filtered <- list_filtering_results[[1]]
min_freq_ignored_clusters <- list_filtering_results[[2]]

print("3.2.3 Quantifying the hierarchical clustering...")
plotHCQuantification(clusters_filtered)

cat(paste("3.2.4 Enter the chosen threshold for the clustering of", input_name, ": "))
selected_threshold <- as.numeric(readLines("stdin", n=1))

selected_clusters = clusters_filtered[clusters_filtered$cutoff == selected_threshold, ]

print("3.2.5 Plotting the resulting clusters...")
plot_clusters_and_loess(selected_clusters)
