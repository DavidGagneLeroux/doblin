#!/usr/bin/env Rscript

## A step-by-step demonstration to understand the pipeline
## ex usage: Rscript main.R -x 1000 -t 0.0005 -o ~/Documents/ -s M1,M2 -i ./M1_input.csv,./M2_input.csv

## Install missing packages
list.of.packages <- c("grid", "ggthemes", "ggplot2", "magrittr", "dplyr", "ggnewscale",
                      "readr", "data.table", "reshape2", "grDevices", "timeseriesAnalysis",
                      "optparse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the library
library(timeseriesAnalysis)
library(optparse)

## Parse arguments from command line
options <- list(
  make_option(c("-x", "--intersection"), action = "store", type="integer", default=1000, help="Number of top barcodes to fetch [default %default]"),
  make_option(c("-t", "--treshold"), action = "store", type="double", default=0.0005, help="Limit frequency above which barcodes have assigned colors [default %default]"),
  make_option(c("-o", "--outputPath"), action = "store", type="character", default=getwd(), help="Output directory [default %default]"),
  make_option(c("-s", "--sampleNames"), action = "store", type="character", help="Sample's name or ID. If multiple: use comma to separate"),
  make_option(c("-i", "--inputFiles"), action = "store", type="character", help="Input csv file. If multiple: use comma to separate")
)
arguments <- parse_args(OptionParser(option_list = options))

## Test if there is at least one argument: if not, return an error
if (length(arguments)==1) {
  stop("Arguments must be supplied", call.=FALSE)
} else if (length(arguments)> 1) {

  print("Processing...")

  n_intersect = as.numeric(arguments$intersection)
  max_freq_treshold= as.double(arguments$treshold)
  output_directory = arguments$outputPath
  cohort_names = as.list(strsplit(arguments$sampleNames, ",")[[1]])
  input_files = as.list(strsplit(arguments$inputFiles, ",")[[1]])
}

## Step 0: Processing csv file(s)
input_data <- lapply(input_files, readr::read_csv)

## Step 1: Reshape input files into dataframes
dataframes <- lapply(input_data, reshapeDF)

## Step 2: Fetch the top N barcodes of each dataframe
top_final <- list()
top_max <- list()
x <- lapply(dataframes, fetchTop, n_intersect)
for(element in x){
  top_final <-append(top_final, element[1])
  top_max <- append(top_max, element[2])
}

##Step 3: Color assignment for plotting

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
#TODO: Handle multiple labels?
for(i in 1:length(dataframes)) {
  plotDynamics(dataframes[[i]], max_freq_treshold, cohort_names[[i]])
  }

## Step 5: Calculate diversity
diversities <- lapply(input_data, calculate_diversity)
for (i in 1:length(dataframes)){
  diversities[[i]]$Sample = cohort_names[[i]]
}

## Step 6: Plot diversities
sample_diversities <- data.table::rbindlist(diversities)

plotDiversity(sample_diversities, cohort_names)

