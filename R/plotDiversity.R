#' A function to plot the diversity dynamics
#'
#' This function allows you to plot barcode diversities for a sample. We save
#' a multi-panel figure with all diversities and the legend.
#'
#' @param dataframe a dataframe containing the barcode diversities for each sample
#' @return A plot of the diversity over time.
#' @import ggplot2
#' @export plotDiversity


plotDiversity <- function(dataframe) {

  df <- reshape2::melt(dataframe, id.vars = c("Generations"),
              variable.name = "q_type",
              value.name = "q_value")

  x_breaks = sort(c(unique(df$Generations)))
  y_breaks = sort(c(unique(log10(df$q_value))))

  df_diversities = ggplot(df) + geom_line(aes(Generations,log10(q_value),color=q_type),size=1.5) + geom_hline(yintercept =3,linetype="dashed") +
    theme_Publication() + xlab("Time") + ylab("Diversity") + scale_x_continuous(limits =c(min(x_breaks), max(x_breaks))) +
    coord_cartesian(expand = FALSE) + scale_y_continuous(limits=c(0, max(y_breaks) + 1)) + guides(color = guide_legend(override.aes = list(size=8,shape=15)))

  p = df_diversities + guides(color = "none") + theme_Publication_noYaxis() + coord_cartesian(expand = TRUE) + facet_wrap(~ q_type)

  ggsave(p,filename = paste(output_directory, input_name,"_diversity.eps",sep=""), width = 8.25,height = 6)
}
