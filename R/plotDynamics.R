#' A plot function
#'
#' This function allows you to plot barcode dynamics (linear plot and
#'  log-transformed plot).
#'
#' @param raw_df a dataframe produced by reshapeDF()
#' @param freq_treshold a double representing a limit frequency above which barcodes have assigned colors
#' @param cohort_name a character string indicating the sample's name
#' @param plot_model a character string indicating what kind of plot the user wants to model
#' @import ggplot2
#' @export plotDynamics

plotDynamics <- function(raw_df, freq_treshold, cohort_name, plot_model) {

  #raw_df=dataframes[[1]]
  #freq_treshold=min_freq_treshold
  #cohort_name=cohort_names[[1]]
  #plot_model="both"

  x_breaks = sort(c(unique(raw_df$variable)))
  #log10_y_breaks = sort(c(unique(raw_df$value)))

  treshold_top_max = treshold.top.max
  treshold_top_max$max=NULL

  df = merge(raw_df, treshold_top_max, by = "ID",all.x = TRUE)

  # for all barcodes without a universal color (barcodes with max frequencies < treshold),
  # assign a gray hex value
  if (nrow(df[is.na(df$hex),]) > 0){
    df[is.na(df$hex),]$hex="#cccccc"
  }


  # convert ID to factor for grouping
  df$ID = as.factor(df$ID)

  # important: order dataframe by max frequency; this will determine the stacking of areas in the plot
  tf = df[order(df$max),]

  # subset dataframe with barcodes above a certain max frequency
  grouped_by = magrittr::`%>%`(tf, dplyr::group_by(hex))
  filtered = magrittr::`%>%`(grouped_by, dplyr::filter(max>freq_treshold))
  grouped_tf = magrittr::`%>%`(filtered, dplyr::ungroup())

  # set color scale using universal hex colors
  mycolors = grouped_tf$hex
  names(mycolors) = grouped_tf$ID

  # map factor levels to max frequencies
  grouped_tf$ID = factor(grouped_tf$ID, levels = unique(grouped_tf$ID[order(grouped_tf$max)]))
  tf$ID = factor(tf$ID, levels = unique(tf$ID[order(tf$max)]))

  # here we plot a first geom_area with all the barcodes using the long color list
  # then we add a new fill scale, and plot a second geom_area on top of the first
  # using only the subsetted, high-freq barcodes

  if (plot_model == "log"){

    g = ggplot(tf) + geom_area(aes(x=variable,y=value,group=ID,fill=ID),data=tf) + scale_fill_manual(values = long.color.list.random, guide=FALSE) +
      ggnewscale::new_scale_fill() + geom_area(aes(x=variable,y=value,group=ID,fill=ID),data=grouped_tf) +
      scale_x_continuous(limits=c(min(x_breaks), max(x_breaks))) + theme_Publication() +
      scale_fill_manual(values = mycolors,name="Cluster ID", guide="none") +
      labs(x="Time" , y="Barcode density") + coord_cartesian(expand = FALSE)

    ggsave(g,filename = paste(output_directory, cohort_name,"_area.jpeg", sep=""), type = "cairo",width = 8.25,height = 6)

  } else if(plot_model == "linear"){


    all.line = ggplot() + geom_line(aes(x=variable,y=value,group=ID),data=tf,colour="#CCCCCC",alpha=0.3) +
      geom_line(aes(x=variable,y=value,group=ID,colour=ID),data=grouped_tf,size=1) +
      theme_Publication() + scale_y_log10(limits=c(min(tf$value)+1e-7,1e0)) + scale_color_manual(values = mycolors,name="Cluster ID") + labs(x ="Time" ,y="Barcode frequency") +
      scale_x_continuous(limits=c(min(x_breaks), max(x_breaks))) +
      guides(color = FALSE, shape = guide_legend(order = 1))  + coord_cartesian(expand = FALSE)

    ggsave(all.line,filename = paste(output_directory, cohort_name,"_line.jpeg", sep=""), type = "cairo",width = 8.25,height = 6)

  } else if(plot_model == "both"){

    g = ggplot(tf) + geom_area(aes(x=variable,y=value,group=ID,fill=ID),data=tf) + scale_fill_manual(values = long.color.list.random, guide=FALSE) +
      ggnewscale::new_scale_fill() + geom_area(aes(x=variable,y=value,group=ID,fill=ID),data=grouped_tf) +
      scale_x_continuous(limits=c(min(x_breaks), max(x_breaks))) + theme_Publication() +
      scale_fill_manual(values = mycolors,name="Cluster ID", guide="none") +
      labs(x="Time" , y="Barcode density") + coord_cartesian(expand = FALSE)

    ggsave(g,filename = paste(output_directory, cohort_name,"_area.jpeg", sep=""), type = "cairo",width = 8.25,height = 6)

    all.line = ggplot() + geom_line(aes(x=variable,y=value,group=ID),data=tf,colour="#CCCCCC",alpha=0.3) +
      geom_line(aes(x=variable,y=value,group=ID,colour=ID),data=grouped_tf,size=1) +
      theme_Publication() + scale_y_log10(limits=c(min(tf$value)+1e-7,1e0)) + scale_color_manual(values = mycolors,name="Cluster ID") + labs(x ="Time" ,y="Barcode frequency") +
      scale_x_continuous(limits=c(min(x_breaks), max(x_breaks))) +
      guides(color = FALSE, shape = guide_legend(order = 1))  + coord_cartesian(expand = FALSE)

    ggsave(all.line,filename = paste(output_directory, cohort_name,"_line.jpeg", sep=""), type = "cairo",width = 8.25,height = 6)

  }


}
