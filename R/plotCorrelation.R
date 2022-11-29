#' A plot function
#'
#' This function allows you to create a running scatterplot of each sample's barcode
#'  frequencies at all time points vs the barcode frequencies at time 0. In other
#'  words, the plot shows the correlation between initial frequencies and frequencies at time t.
#'
#' @param input_data_sample input file given by user
#' @param initial_library barcode library
#' @param sample_name the sample's name
#' @return A dataframe linking the sample to the library
#' @import ggplot2
#' @import egg
#' @import ggpubr
#' @import magrittr
#' @export plotCorrelation

## TODO: Try to import less libraries

plotCorrelation <- function(input_data_sample, initial_library, sample_name) {

  Sample.index = merge(input_data_sample,initial_library,by = "ID",all.x = TRUE)
  ## 1)For each timepoint, add up # of reads.
  ## 2) Calculate the frequency of reads in every clusters (for all timepoints)
  ## Result => each sample's barcode frequencies at all time points
  Sample.index$freq = Sample.index$Reads/ave(Sample.index$Reads,Sample.index$Time,FUN = sum)

  times=sort(unique(Sample.index$Time))
  freq_plots = list()

  # adjust color based on cohort

  for(i in seq_along(times)){
    freq_plots[[i]] = ggplot(Sample.index[Sample.index$Time==times[i],]) + geom_point(aes(frequency,freq),color=grDevices::hcl.colors(1)) +
      theme_Publication(base_size = 24) + scale_x_log10(limits=c(1e-07,1)) +
      scale_y_log10(limits=c(1e-07,1)) + geom_abline(slope = 1, intercept = 0, linetype="dashed") +
      xlab("Initial frequency") + ylab(paste("Frequency at time",times[i]))
  }
  ggarrange(plotlist = freq_plots) %>% ggexport(filename = paste(output_directory, sample_name,"_grid.png", sep=""),width = 1800,height = 1200,ncol = 4,nrow = 4)
  return(Sample.index)
}
