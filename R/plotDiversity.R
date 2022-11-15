#' A plot function
#' This function allows you to plot barcode diversities for each sample.
#' Each panel of each cohort is saved individually. Then we save a multi-panel
#' figure with all diversities and the legend.
#' @export

#TODO: Generalize y-scale and adjust x-scale
#TODO: Plot each cohort individually? Add legend?
plotDiversity <- function(dataframe, cohort_names) {

  breaks = sort(c(unique(dataframe$Generations)))
  label = as.character(breaks)
  limits = c(min(breaks), max(breaks))

  color_palette <- grDevices::hcl.colors(length(unique(dataframe$Sample)))

  df <- reshape2::melt(dataframe, id.vars = c("Generations", "Sample"),
              variable.name = "q_type",
              value.name = "q_value")

  df_diversities = ggplot(df) + geom_line(aes(Generations,log10(q_value),color=Sample),size=1.5) + geom_hline(yintercept =3,linetype="dashed") +
    theme_Publication() + xlab("Time (post-gavage)") + scale_x_continuous(breaks = breaks,labels = label) +
    coord_cartesian(expand = FALSE) + scale_y_continuous(breaks=c(0,1,2,3,4,5,6),limits=c(0,6)) + scale_color_manual(values = color_palette) + guides(color = guide_legend(override.aes = list(size=8,shape=15)))
  p = df_diversities + guides(color = FALSE) + theme_Publication_noYaxis() + scale_x_continuous(breaks = breaks,labels = label,limits=limits) +
    coord_cartesian(expand = TRUE) + facet_wrap(~ q_type)

  file_name = paste(cohort_names, collapse = '')

  ggsave(p,filename = paste(output_directory, file_name,"_diversity.eps",sep=""),fonts=c("serif"),width = 8.25,height = 6,device = cairo_ps())

}
