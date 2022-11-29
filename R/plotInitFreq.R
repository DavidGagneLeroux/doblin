#' A plotting function
#'
#' This function allows you to plot the barcode frequencies from the initial library.
#'
#' @param initial_library a dataframe containing information on the initial barcode library
#' @return A dataframe containing information on the initial barcode library
#' @import ggplot2
#' @export plotInitFreq

##TODO: Generalize the limits and breaks

plotInitFreq <- function(initial_library) {

  s=sum(initial_library$Reads)
  initial_library$frequency=initial_library$Reads/s
  initial_library = initial_library[order(initial_library$frequency,decreasing = TRUE),]

  initial.frequencies = data.frame(table(initial_library$frequency))
  initial.frequencies$Var1 = as.numeric(as.character(initial.frequencies$Var1))

  gavage.plot = ggplot(initial.frequencies) + geom_line(aes(x=Var1,y=Freq)) +
    scale_x_log10(limits=c(1e-7,1e-2), breaks = c(1e-7, 1e-6, 1e-5, 1e-4,1e-3, 1e-2),labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    scale_y_log10(limits=c(1e0,1e5), breaks = c(1e0, 1e1, 1e2, 1e3, 1e4, 1e5), labels = scales::trans_format("log10", scales::math_format(10^.x))) + theme_Publication() +
    xlab("Barcode frequency") + ylab("Number of unique barcodes") + annotation_logticks()

  ggsave(gavage.plot,filename = paste(output_directory,"library_frequency_spectrum.eps",sep=""),width = 4,height = 4,device = cairo_ps)

  return(initial_library)
}
