#!/usr/bin/env Rscript
library(timeseriesAnalysis)
library(optparse)

#TODO: Make sure that all external functions are explicitly linked to their packages!

#Parse arguments from command line
options <- list(
  make_option(c("-x", "--intersection"), action = "store"),
  make_option(c("-t", "--treshold"), action = "store"),
  make_option(c("-p", "--outputPath"), action = "store"),
  make_option(c("-n", "--sampleNames"), action = "store"),
  make_option(c("-i", "--inputFiles"), action = "store")
)
arguments <- parse_args(OptionParser(option_list = options))

# test if there is at least one argument: if not, return an error
if (length(arguments)==1) {
  stop("Arguments must be supplied", call.=FALSE)
} else if (length(arguments)> 1) {

  print("Processing...")
  n_intersect = as.numeric(arguments$intersection)
  print(n_intersect)
  max_freq_treshold= as.double(arguments$treshold)
  print(max_freq_treshold)
  output_directory = arguments$outputPath
  print(output_directory)
  cohort_names = as.list(strsplit(arguments$sampleNames, ",")[[1]])
  print(cohort_names)
  input_files = as.list(strsplit(arguments$inputFiles, ",")[[1]])
  print(input_files)

  input_data <- lapply(input_files, readr::read_csv)
}

#1. Reshape input into dataframe
dataframes <- lapply(input_data, reshapeDF)

#2. Fetch the top N barcodes
top_final <- list()
top_max <- list()
x <- lapply(dataframes, fetchTop, n_intersect)
for(element in x){
  top_final <-append(top_final, element[1])
  top_max <- append(top_max, element[2])
}

#3. Bind colors to table

# create list of unique top.max barcodes across all data
all.top.max = unique(data.table::rbindlist(top_max))
all.top.max = all.top.max[,c(1,2)]
all.top.max = all.top.max[order(-all.top.max$max),]
dup.vec = duplicated(all.top.max$ID)
all.top.max = all.top.max[!dup.vec,]

# we limit the selection such that all barcodes with max frequency > 0.0005 are assigned a hex color
treshold.top.max <- all.top.max[all.top.max$max >= max_freq_treshold, ]

# TODO: access raw data : system.file("extdata", "model-coef.tsv", package = "myfirstpackage")
#TODO: grDevices::hcl.colors(n) could be used instead of csv
top_colors <- readr::read_csv("../inst/extdata/top_colors2.csv")

## we create a very long list of colors for low-frequency barcodes.
## repetition of colors isn't an issue, we just want each barcodes to be colored instead of assigning gray values
long.color.list = rep(top_colors$hex,50)
long.color.list.random = sample(long.color.list)

# As a result, all top max frequencies > treshold are assigned a hex color
top_colors = top_colors[1:length(treshold.top.max[[1]]),]
treshold.top.max = cbind(treshold.top.max,top_colors)

#4. Plot dynamics
#TODO: Handle multiple labels?
#TODO: Change axis titles in plots and y-scale!
for(i in 1:length(dataframes)) {
  plotDynamics(dataframes[[i]], max_freq_treshold, cohort_names[[i]])
  }

#5. Calculate diversity
diversities <- lapply(input_data, calculate_diversity)
for (i in 1:length(dataframes)){
  diversities[[i]]$Sample = cohort_names[[i]]
}

#6. Plot diversities
sample_diversities <- data.table::rbindlist(diversities)

plotDiversity(sample_diversities, cohort_names)








