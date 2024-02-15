#' A plot function
#'
#' This function allows you to plot barcode dynamics (logarithmic-scale and/or linear-scale).
#'
#' @param reshaped_df a dataframe produced by reshapeData()
#' @param colored_topFreq_df a dataframe containing the top barcodes with their respective colors
#' @param min_freq_threshold a double representing the minimum frequency above which barcodes are assigned colors
#' @param plot_model a character string indicating what kind of plot the user wants to model
#' @import ggplot2
#' @export plotDynamics

plotDynamics <- function(reshaped_df, colored_topFreq_df, min_freq_threshold, plot_model) {

  colored_topFreq_df$max=NULL
  colored_df = merge(reshaped_df, colored_topFreq_df, by = "ID",all.x = TRUE)

  # All barcodes with maximum frequency < min_freq_threshold are assigned a gray hex value
  if (nrow(colored_df[is.na(colored_df$hex),]) > 0){
    colored_df[is.na(colored_df$hex),]$hex="#cccccc"
  }

  # convert ID to factor for grouping
  colored_df$ID = as.factor(colored_df$ID)

  # IMPORTANT: Dataframe is ordered by increasing maximum frequency; this will determine the stacking of areas in the linear plot
  colored_df = colored_df[order(colored_df$max),]

  # Subset dataframe with barcodes above a certain max frequency
  grouped_by = magrittr::`%>%`(colored_df, dplyr::group_by(hex))
  filtered = magrittr::`%>%`(grouped_by, dplyr::filter(max>min_freq_threshold))
  grouped_df = magrittr::`%>%`(filtered, dplyr::ungroup())

  rm(grouped_by)

  # Set color scale using universal hex colors
  mycolors = grouped_df$hex
  names(mycolors) = grouped_df$ID

  # Map factor levels to max frequencies
  grouped_df$ID = factor(grouped_df$ID, levels = unique(grouped_df$ID[order(grouped_df$max)]))
  colored_df$ID = factor(colored_df$ID, levels = unique(colored_df$ID[order(colored_df$max)]))

  # Here we plot a first geom_area with all the barcodes using the long color list
  # then we add a new fill scale, and plot a second geom_area on top of the first
  # using only the subsetted, high-freq barcodes

  x_breaks = sort(c(unique(reshaped_df$Time)))

  if (plot_model == "linear" || plot_model == "both"){
    print("WARNING: Linear-scale plots are very heavy. This might take several minutes...")

    #grDevices::cairo_ps(paste(output_directory, input_name,"_area.ps", sep=""),width = 8.25,height = 6)

    g = ggplot(colored_df) + geom_area(aes(x=Time,y=Frequency,group=ID,fill=ID),data=colored_df) + scale_fill_manual(values = LONG_COLOR_LIST_RAND, guide="none") +
      ggnewscale::new_scale_fill() + geom_area(aes(x=Time,y=Frequency,group=ID,fill=ID),data=grouped_df) +
      scale_x_continuous(limits=c(min(x_breaks), max(x_breaks))) + theme_Publication() +
      scale_fill_manual(values = mycolors,name="Cluster ID", guide="none") +
      labs(x="Time (generations)" , y="Barcode frequency") + coord_cartesian(expand = FALSE)

    output_file <- paste(output_directory, input_name, "_area.jpg", sep = "")
    ggsave(output_file, plot = g, width = 8.25, height = 6, dpi = 300) # Adjust dpi as per your requirements

    #graphics::plot(g)
    #grDevices::dev.off()

  }

  if(plot_model == "logarithmic" || plot_model == "both"){


    grDevices::cairo_ps(paste(output_directory, input_name,"_line.eps", sep=""),width = 8.25,height = 6)

    all.line = ggplot() + geom_line(aes(x=Time,y=Frequency,group=ID),data=colored_df,colour="#CCCCCC",alpha=0.3) +
      geom_line(aes(x=Time,y=Frequency,group=ID,colour=ID),data=grouped_df,linewidth=1) +
      theme_Publication() + scale_y_log10(limits=c(min(colored_df$Frequency)+1e-7,1e0)) + scale_color_manual(values = mycolors,name="Cluster ID") + labs(x ="Time" ,y="Barcode frequency") +
      scale_x_continuous(limits=c(min(x_breaks), max(x_breaks))) +
      guides(color = "none", shape = guide_legend(order = 1))  + coord_cartesian(expand = FALSE)

    #output_file <- paste(output_directory, input_name, "_line.jpg", sep = "")
    #ggsave(output_file, plot = all.line, width = 8.25, height = 6, dpi = 300) # Adjust dpi as per your requirements

    graphics::plot(all.line)
    grDevices::dev.off()

  }

}
