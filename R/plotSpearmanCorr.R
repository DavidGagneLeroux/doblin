#' Here we plot each sample's Spearman correlation over time
#'
#' @param sample_corr a dataframe containing each sample's Spearman correlation over time
#' @return A plot of the Spearman rank correlation of all samples over time
#' @import ggplot2
#' @export plotSpearmanCorr

plotSpearmanCorr <- function(sample_corr){

  limits <- sort(c(unique(sample_corr$times)))
  color_palette <- grDevices::hcl.colors(length(unique(sample_corr$sample)))
  sample_correlations_plot = ggplot(sample_corr) + geom_line(aes(x=times,y=spearman,group=sample,color=sample),size=1.5) + theme_Publication() + scale_color_manual(values = color_palette, name="Sample") + xlab("Time") + ylab("Spearman rank correlation")+ xlim(c(min(limits), max(limits))) + ylim(c(0,1))
  ggsave(sample_correlations_plot,filename = paste(output_directory, "spearman_series.png", sep=""),width=10,height=8)
}
