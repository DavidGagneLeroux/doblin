#library(timeseriesAnalysis)

#TODO: Make sure that all external functions are explicitly linked to their packages!

# Retrieving arguments from command line
args <- commandArgs(trailingOnly = TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("Arguments must be supplied", call.=FALSE)
} else if (length(args)> 0) {

  print("Processing...")

  cat(args, sep = "\n")
  #n_intersect = args[1]
  n_intersect = 1000
  #max_freq_treshold= args[2]
  max_freq_treshold = 0.0005
  #label = args[3] (given by user?)
  #output_directory = args[4]
  output_directory = "~/Documents/"
  #cohort_names = args[5]
  cohort_names = list("M1", "M2")
  #input_files <- list()
  input_files <- list("M1_input.csv", "M2_input.csv")
  # for(i in 2:length(args)) {
  #   input_files <- append(input_files, args[i])
  # }

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
top_colors <- read_csv("timeseriesAnalysis/inst/extdata/top_colors2.csv")

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








