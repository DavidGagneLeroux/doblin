#' A plotting function
#'
#' This function allows you to plot the barcode frequencies from the initial library.
#'
#' @param initial_library a dataframe containing information on the initial barcode library
#' @return A dataframe containing information on the initial barcode library
#' @import ggplot2
#' @export plotInitFreq

plotInitFreq <- function(initial_library) {

  ## Error message if initial_library doesn't have the appropriate format
  testing_colnames <- identical(colnames(initial_library), c("ID", "Time", "Reads"))
  if (testing_colnames == FALSE){
    stop("# The reference file format has not been respected. Make sure it contains 3 columns named: ID, Time and Reads.")
  }

  ## Error message if the values of 'Reads' are not numeric
  if (is.numeric(initial_library$Reads) == FALSE){
    stop("# The values in 'Reads' column of reference file must be numeric.")
  }

  ## Error message if the values of 'Reads' are not numeric
  if (is.numeric(initial_library$Time) == FALSE){
    stop("# The values in 'Time' column of reference file must be numeric.")
  }

  #initial_library = initial_barcode_library
  s=sum(initial_library$Reads)
  initial_library$frequency=initial_library$Reads/s
  initial_library = initial_library[order(initial_library$frequency,decreasing = TRUE),]

  initial.frequencies = data.frame(table(initial_library$frequency))
  initial.frequencies$Var1 = as.numeric(as.character(initial.frequencies$Var1))

  #breaks_x = sort(c(unique(initial.frequencies$Var1)))

  gavage.plot = ggplot(initial.frequencies) + geom_line(aes(x=Var1,y=Freq)) +
    scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) + theme_Publication() +
    xlab("Barcode frequency") + ylab("# of unique barcodes") + annotation_logticks()

  ggsave(gavage.plot,filename = paste(output_directory,"library_frequency_spectrum.jpeg",sep=""),type = "cairo",width = 4,height = 4)

  return(initial_library)
}
