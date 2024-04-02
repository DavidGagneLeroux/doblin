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


options(repos = c(CRAN = "https://cran.rstudio.com/")) # CRAN mirror

## Install missing packages
list.of.packages <- c("grid", "ggthemes", "ggplot2", "magrittr", "dplyr", "ggnewscale",
                      "readr", "data.table", "reshape2", "grDevices", "devtools",
                      "optparse", "egg", "ggpubr", "stats", "imputeTS", "data.table", "dtwclust",
                      "purrr", "tidyr", "TSdist", "entropy", "gplots", "lazyeval", "pryr", "doblin")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)){
  install.packages(new.packages)
}

if (!"doblin" %in% installed.packages()[, "Package"]) {
  devtools::install_github("dagagf/doblin")
}

# Check if it matches your repository name
if (basename(getwd()) != "doblin") {
  stop("Make sure to be in the 'doblin' repository.")
}

library(doblin)
library(optparse)
library(pryr)

if (interactive()){
  library(ggplot2)
  library(dplyr)
  library(magrittr)
  library(ggpubr)

  output_directory <- readline(prompt = "Please enter an output directory [default: <current_dir>]: ")
  if (nchar(output_directory) == 0) {
    output_directory <- getwd()
  }
  print(paste("Provided output directory:", output_directory))

  input_name <- readline(prompt = "Please enter an input name [default: MyData]: ")
  if (nchar(input_name) == 0) {
    input_name <- "MyData"
  }
  print(paste("Provided input name:", input_name))

  input_file <- readline(prompt = "Please enter the input file (ex: ~/usr/repository/file_name.csv): ")
  if (nchar(input_file) == 0) {
    stop("No input file provided.")
  }
  print(paste("Provided input file:", input_file))

}else{

  ## Parse arguments from command line
  options <- list(
    make_option(c("-t", "--threshold"), action = "store", type="double", default=0.0005, help="Minimum frequency above which barcodes are assigned colors [default %default]"),
    make_option(c("-o", "--outputPath"), action = "store", type="character", default=getwd(), help="Output directory [default %default]"),
    make_option(c("-n", "--inputName"), action = "store", type="character", help="Input name"),
    make_option(c("-i", "--inputFile"), action = "store", type="character", help="Input csv file"),
    make_option(c("-c", "--timeCut"), action = "store", type="integer", help="Minimum duration, in terms of time points, for which lineages must persist to be eligible for clustering")
  )
  arguments <- parse_args(OptionParser(option_list = options))

  ## Test the number of arguments: if not enough, return an error. "+1" is to account for  the "--help" argument.
  if (length(arguments) != length(options) + 1) {
    stop("Missing arguments. Arguments must be supplied!", call.=FALSE)
  }

  print("Processing the command line...")

  min_freq_threshold= as.double(arguments$threshold)
  output_directory = arguments$outputPath
  input_name = arguments$inputName
  input_file = arguments$inputFile
  time_threshold = as.numeric(arguments$timeCut)

  # Saving cmd line
  sink(paste(output_directory,"cmd_line.txt"), append = TRUE)
  cat("Command line inputs:\n")
  cat(commandArgs(trailingOnly = TRUE))
  sink()

}

print("Step 0: Processing CSV file...")
input_dataframe <- readr::read_csv(input_file, show_col_types = FALSE)

## Non-interactive mode:
if (!interactive()){
  cat("Do you want to run this pipeline in an interactive way?(y/n): ")
  pipeline_choice <- readLines("stdin", n=1)
  print(pipeline_choice)
  pipeline_choice <- match.arg(tolower(pipeline_choice), c("yes", "no"))

  if (pipeline_choice == "no"){
    cat("Please provide the additional input file containing all the required arguments (Refer to user guide for format): ")
    additional_file <- readLines("stdin", n=1)
    additional_arguments <- readr::read_csv(additional_file, show_col_types = FALSE)
    # plot_choice: "yes", "no"
    plot_choice <- additional_arguments$plot_choice
    # plot_model: if yes "logarithmic", "linear", "both". else ""
    plot_model <- additional_arguments$plot_model
    # diversity_choice: "yes", "no"
    diversity_choice <- additional_arguments$diversity_choice
    # freq_filter_threshold: numeric
    freq_filter_threshold <- additional_arguments$freq_filter_threshold
    # agglomeration: "average", "ward.D","ward.D2", "centroid", "single", "complete", "median", "mcquitty"
    agglomeration <- additional_arguments$agglomeration
    # similarity_metric: "pearson", "dtw"
    similarity_metric <- additional_arguments$similarity_metric
    # missing_values: if pearson "everything","all.obs","complete.obs","na.or.complete","pairwise.complete.obs", if dtw ""
    missing_values <- additional_arguments$missing_values
    # dtw_norm: if dtw "L1", "L2", if pearson ""
    dtw_norm <- additional_arguments$dtw_norm
    # min_members: numeric
    min_members <- additional_arguments$min_members
    # min_freq_ignored_clusters: numeric
    min_freq_ignored_clusters <- additional_arguments$min_freq_ignored_clusters
    # selected_threshold: numeric
    selected_threshold <- additional_arguments$selected_threshold
  }
}


## Step 1:
if (interactive()) {
  plot_choice <- readline(prompt = "Do you want to plot the dynamics of your dataset?(y/n): ")
} else if (pipeline_choice == "yes") {
  cat("Do you want to plot the dynamics of your dataset?(y/n): ")
  plot_choice <- readLines("stdin", n=1)
}
plot_choice <- match.arg(tolower(plot_choice), c("yes", "no"))

if (plot_choice == "yes"){

  if (interactive()){
    min_freq_threshold <- as.double(readline(prompt = "Please enter a minimum frequency above which barcodes are assigned colors [default: 0.0005]: "))
    if (nchar(min_freq_threshold) == 0) {
      min_freq_threshold <- 0.0005
    }
    print(paste("Provided minimum frequency:", min_freq_threshold))
  }

  print("Step 1: Plotting the dynamics...")
  print("1.1 Reshaping input file into long-format dataframe...")
  reshaped_dataframe <- reshapeData(input_dataframe)

  ## Step 1.2:
  N_LINEAGES = 50
  print(paste("1.2 Retrieving the first",N_LINEAGES,"barcodes with the highest maximum frequencies..."))
  top_N_maxFreq <- fetchTop(reshaped_dataframe, N_LINEAGES)

  ## Step 1.3:
  print(paste("1.3 Assigning colors to lineages having reached the minimum frequency threshold among the",N_LINEAGES,"most dominant barcoded lines..."))

  ## All barcodes with (maximum frequency >= minimum frequency threshold) are assigned a hex color
  colored_top_freq <- top_N_maxFreq[top_N_maxFreq$max >= min_freq_threshold, ]

  ## we create a very long list of colors for low-frequency barcodes.
  COLOR_LIST <- readr::read_csv("./inst/extdata/top_colors2.csv", show_col_types = FALSE)
  LONG_COLOR_LIST_RAND = sample(rep(COLOR_LIST$hex,50)) # for dynamics (linear-scale plot)...

  COLOR_LIST = COLOR_LIST[1:length(colored_top_freq[[1]]),]
  colored_top_freq = cbind(colored_top_freq,COLOR_LIST)

  ## Step 1.4:
  if (interactive()) {
    plot_model <- readline(prompt = "Do you want to plot a log-scale model, a linear-scale model or both? (logarithmic/linear/both): ")
  } else if (pipeline_choice == "yes") {
    cat("Do you want to plot a log-scale model, a linear-scale model or both? (logarithmic/linear/both): ")
    plot_model <- readLines("stdin", n=1)
  }
  plot_model <- match.arg(plot_model, c("linear", "logarithmic", "both"))


  print("Plotting in progress...")
  plotDynamics(reshaped_dataframe, colored_top_freq, min_freq_threshold, plot_model)

}

## Step 2:
if (interactive()) {
  diversity_choice <- readline(prompt = "Do you want to plot the diversity of your dataset?(y/n): ")
} else if (pipeline_choice == "yes") {
  cat("Do you want to plot the diversity of your dataset?(y/n): ")
  diversity_choice <- readLines("stdin", n=1)
}
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

if (interactive()) {
  freq_filter_threshold <- as.numeric(readline(prompt = "Specify a minimum mean frequency below which lineages are not taken into account during clustering (ex: 0.00005): "))
  time_threshold <- as.numeric(readline(prompt="Specify the minimum duration, in terms of time points, for which lineages must persist to be eligible for clustering: "))
} else if (pipeline_choice == "yes") {
  cat("Specify a minimum mean frequency below which lineages are not taken into account during clustering (ex: 0.00005): ")
  freq_filter_threshold <- as.numeric(readLines("stdin", n=1))
}

print("3.1 Filtering the input data...")

start_time <- Sys.time()
filtered_df <- filterData(input_dataframe, freq_filter_threshold, time_threshold)
end_time <- Sys.time()
memory_usage <- mem_change(filterData(input_dataframe, freq_filter_threshold, time_threshold))
runtime <- end_time - start_time
step3_1 <- list(runtime = runtime, memory_used = memory_usage)

## 3.2: Clustering with Pearson & DTW + threshold selection depending on distance
## between clusters & cluster number
print("3.2 Clustering the filtered data...")

if (interactive()) {
  agglomeration <- readline(prompt="Enter an agglomeration method. Please refer to stats::hclust() R documentation (ex: average) : ")
  similarity_metric <- readline(prompt="Enter the metric to be used to measure similarity between two time-series (pearson/dtw) : ")
} else if (pipeline_choice == "yes") {
  cat("Enter an agglomeration method (refer to stats::hclust() R documentation): ")
  agglomeration <- readLines("stdin", n=1)
  cat("Enter the metric to be used to measure similarity between two time-series (pearson/dtw) : ")
  similarity_metric <- readLines("stdin", n=1)
}
agglomeration <- match.arg(agglomeration, c("average", "ward.D","ward.D2", "centroid", "single", "complete", "median", "mcquitty"))
similarity_metric <- match.arg(similarity_metric, c("pearson","dtw"))

if (similarity_metric == "pearson") {

  if (interactive()) {
    missing_values <- readline(prompt = "Enter a method for computing covariances in the presence of missing values. Please refer to stats::cor() R documentation (ex: pairwise.complete.obs) : ")
  } else if (pipeline_choice == "yes") {
    cat("Enter a method for computing covariances in the presence of missing values. Please refer to stats::cor() R documentation (ex: pairwise.complete.obs) : ")
    missing_values <- readLines("stdin", n=1)
  }

  missing_values <- match.arg(missing_values, c("everything","all.obs","complete.obs","na.or.complete","pairwise.complete.obs"))

}else{
  missing_values = NULL
}

print("3.2.1 Computing the relative clusters for ALL thresholds between 0.1 and maximum height of hierarchical clustering... ")

start_time <- Sys.time()
clusters_df = perform_hierarchical_clustering(filtered_df, agglomeration, similarity_metric, missing_values)
end_time <- Sys.time()
memory_usage <- mem_change(perform_hierarchical_clustering(filtered_df, agglomeration, similarity_metric, missing_values))
runtime <- end_time - start_time
step3_2_1 <- list(runtime = runtime, memory_used = memory_usage)


print("3.2.2 Filtering the hierarchical clustering results...")

if (interactive()) {
  min_members <- as.numeric(readline(prompt = paste("Enter the minimum number of members per cluster for", input_name, ": ")))
} else if (pipeline_choice == "yes") {
  cat(paste("Enter the minimum number of members per cluster for", input_name, ": "))
  min_members <- as.numeric(readLines("stdin", n=1))
}


start_time <- Sys.time()
clusters_filtered = filterHC(filtered_df, clusters_df, min_members)
end_time <- Sys.time()
memory_usage <- mem_change(filterHC(filtered_df, clusters_df, min_members))
runtime <- end_time - start_time
step3_2_2 <- list(runtime = runtime, memory_used = memory_usage)


print("3.2.3 Quantifying the hierarchical clustering...")

start_time <- Sys.time()
plotHCQuantification(clusters_filtered)
end_time <- Sys.time()
memory_usage <- mem_change(plotHCQuantification(clusters_filtered))
runtime <- end_time - start_time
step3_2_3 <- list(runtime = runtime, memory_used = memory_usage)


if (interactive()) {
  selected_threshold <- as.numeric(readline(prompt = paste("3.2.4 Enter the chosen threshold for the clustering of", input_name, ": ")))
} else if (pipeline_choice == "yes") {
  cat(paste("3.2.4 Enter the chosen threshold for the clustering of", input_name, ": "))
  selected_threshold <- as.numeric(readLines("stdin", n=1))
}

selected_clusters = clusters_filtered[clusters_filtered$cutoff == selected_threshold, ]

print("3.2.5 Plotting the resulting clusters...")

start_time <- Sys.time()
plot_clusters_and_loess(selected_clusters)
end_time <- Sys.time()
memory_usage <- mem_change(plot_clusters_and_loess(selected_clusters))
runtime <- end_time - start_time
step3_2_5 <- list(runtime = runtime, memory_used = memory_usage)

# Open a file connection to write runtime and memory usage information
output_file <- file.path(output_directory, "runtime_memory_usage.txt")
sink(output_file, append = TRUE)

# Print the runtime and memory usage information
cat("Step 3.1 - Filtering the input data:\n")
print(step3_1)
cat("\nStep 3.2.1 - Computing the relative clusters for ALL thresholds:\n")
print(step3_2_1)
cat("\nStep 3.2.2 - Filtering the hierarchical clustering results:\n")
print(step3_2_2)
cat("\nStep 3.2.3 - Quantifying the hierarchical clustering:\n")
print(step3_2_3)
cat("\nStep 3.2.5 - Plotting the resulting clusters:\n")
print(step3_2_5)

# Close the file connection
sink()


