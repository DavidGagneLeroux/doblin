#' A function to plot the diversity dynamics
#'
#' This function allows you to plot barcode diversities for each sample. We save
#' a multi-panel figure with all diversities and the legend.
#'
#' @param dataframe a dataframe containing the barcode diversities for each sample
#' @param cohort_names a list containing the names of each sample
#' @return A plot of the diversity over time.
#' @import ggplot2
#' @export plotDiversity

#TODO: Generalize y-scale and adjust x-scale

plotDiversity <- function(dataframe, cohort_names) {

  #dataframe=sample_diversities

  color_palette <- grDevices::hcl.colors(length(unique(dataframe$Sample)))

  df <- reshape2::melt(dataframe, id.vars = c("Generations", "Sample"),
              variable.name = "q_type",
              value.name = "q_value")

  x_breaks = sort(c(unique(df$Generations)))
  y_breaks = sort(c(unique(log10(df$q_value))))

  df_diversities = ggplot(df) + geom_line(aes(Generations,log10(q_value),color=Sample),size=1.5) + geom_hline(yintercept =3,linetype="dashed") +
    theme_Publication() + xlab("Time") + scale_x_continuous(limits =c(min(x_breaks), max(x_breaks))) +
    coord_cartesian(expand = FALSE) + scale_y_continuous(limits=c(0, max(y_breaks) + 1)) + scale_color_manual(values = color_palette) + guides(color = guide_legend(override.aes = list(size=8,shape=15)))
  p = df_diversities + guides(color = FALSE) + theme_Publication_noYaxis() + coord_cartesian(expand = TRUE) + facet_wrap(~ q_type)

  file_name = paste(cohort_names, collapse = '')

  ggsave(p,filename = paste(output_directory, file_name,"_diversity.jpeg",sep=""),type = "cairo",width = 8.25,height = 6)

}
