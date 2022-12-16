#' Plot the log10-transformed barcode frequencies and the moving averages (LOESS)
#'
#' This file contains multiple functions. The main function is: plot_clusters_and_loess()
#' and it uses plotClusterLog10(). plot_clusters_and_loess() starts in
#' the same way as plotHCQuantification(), but without a cutoff column. In plot_clusters_and_loess(),
#' we plot the log10-transformed barcode frequencies contained in all selected clusters,
#' we compute a moving average per cluster and group them in a plot. We also write the files associated with
#' those two plots.
#'
#' (write details for plotClusterLog10())
#'
#' @name plotClustersAndLoess
#' @param series.filtered a dataframe filtered by filterData()
#' @param selected.clusters  A dataframe containing the clusters from a hierarchical clustering
#'  for a specific threshold
#' @param sample_name a character string indicating the sample's name
#' @param effective.breaks a list containing the breaks for the plots
#' @import dplyr
#' @import ggplot2
#' @export plot_clusters_and_loess

plot_clusters_and_loess <- function(series.filtered, selected.clusters, sample_name, effective.breaks){

  #series.filtered = filtered_dataframes[[1]]
  #sample_name = "M1"
  #selected.clusters=selected_clusters

  #effective.breaks = breaks[[1]]
  effective.labels = as.character(effective.breaks)
  effective.limits = c(min(effective.breaks), max(effective.breaks))

  colnames(selected.clusters)=c("cluster")
  nRank = nrow(selected.clusters)
  selected.clusters$rank = seq(1:nRank)

  series.filtered$points = NULL
  series.filtered$rank=seq(1:nRank)

  grouped.clusters = merge(series.filtered,selected.clusters,by.x = "rank",by.y = "rank")

  ## series.reshaped is a dataframe where "variable" represents the time-point (1-18) and value
  ## represents the barcode frequencies
  series.reshaped = reshape2::melt(grouped.clusters,id.vars = c('ID','cluster','rank','mean'))
  series.reshaped$variable = as.numeric(as.character(series.reshaped$variable))

  ## Group by cluster and keep only the clusters with at least 8 members
  series.reshaped=series.reshaped %>%  group_by(cluster) %>% dplyr::filter(length(unique(ID)) >= 8)

  ## Keep only the persistent barcodes
  series_order=subset(series.reshaped,series.reshaped$variable==max(unique(series.reshaped$variable)))

  series_order=series_order %>%
    group_by(cluster) %>%
    summarise(average = mean(value))

  series_order=series_order[order(series_order$average,decreasing = TRUE), ]
  series_order$cluster_ranked=rownames(series_order)
  series.reshaped=merge(series.reshaped,series_order,by="cluster")
  series.reshaped$cluster=NULL
  names(series.reshaped)[7]="cluster"
  clusters.ranked=series_order$cluster_ranked

  max.range = max(series.reshaped$variable)-min(series.reshaped$variable)
  loess.range = (max.range*10)+1

  ## Get moving average of barcode frequencies for each cluster USING LOESS
  xx <- seq(from=min(series.reshaped$variable), to=max(series.reshaped$variable),length.out = loess.range)

  cluster.colors=c("#3cb44b","#4363d8","#e6194B","#e8ca00","#911eb4","#f58231","#22766dff","#42d4f4","#f032e6","#9A6324",
                   "#2F4C39", "#1D3F6E","#94170f","#665679","#F17829","#97A69C","#606EA9","#A9606E","#A99060","#F8F1AE",
                   "#bcf60c", "#fabebe", "#008080", "#e6beff", "#9a6324", "#fffac8")

  cluster.df=series.reshaped %>%  group_by(cluster) %>%
    summarise(value=predict(loess(log10(value+0.0000001)~variable,span=0.2),xx,se = FALSE))  %>%
    group_by(cluster) %>% mutate(time=xx)

  ## Plot log10-transformed barcode frequencies
  plotList = list()

  for(i in seq_along(unique(series.reshaped$cluster))){
    l = plotClusterLog10(series.reshaped[series.reshaped$cluster==clusters.ranked[i],], clusters.ranked[i], sample_name, cluster.colors[i],cluster.df[cluster.df$cluster==clusters.ranked[i],], effective.breaks)
    plotList[[i]]=l

  }

  top10.log =cowplot::plot_grid(plotlist=plotList, align = "hv",axis="tblr", nrow=ceiling(length(unique(series.reshaped$cluster))/5))
  ggsave(top10.log,filename = paste(output_directory, sample_name, "_clusters_log10.eps", sep=""),width = 18,height=7.5*length(unique(series.reshaped$cluster))/5,device = cairo_ps)

  ## Write series.reshape
  readr::write_csv(series.reshaped,file = paste(output_directory, sample_name, "_clustered_series_log10.csv",sep=""),col_names = TRUE)

  ## loesss
  cluster.df$cluster=paste("C",cluster.df$cluster,sep="")
  cluster.df$time=cluster.df$time*10
  clusters.loess <- tidyr::spread(cluster.df, cluster, value)

  # plot loess
  loess.plot = ggplot(cluster.df) + geom_line(aes(time/10,value,group=cluster,color=cluster),size=1) + scale_x_continuous(breaks = effective.breaks,labels = effective.labels, limits = effective.limits) +
    theme_Publication() + scale_color_manual(values = cluster.colors,name="cluster") + ylab("fit") + xlab("Time (post-gavage)") + ylim(-7,-1)
  ggsave(loess.plot,filename = paste(output_directory, sample_name, "_loess_clusters_log10.eps", sep=""),width = 8.25,height = 6,device = cairo_ps)

  # write loess file
  readr::write_csv(clusters.loess,file = paste(output_directory, sample_name, "_clustered_loess_log10.csv",sep=""),col_names = TRUE)

}

#################
#' @export
#' @rdname plotClustersAndLoess

# plot clusters
plotClusterLog10 <- function(df,cluster,sample,color,tf, effective.breaks){

  effective.labels = as.character(effective.breaks)
  effective.limits = c(min(effective.breaks), max(effective.breaks))

  p = ggplot(df,aes(x=variable,y=log10(value+0.0000001))) +
    geom_line(aes(group=ID),color=color,alpha=0.4) + theme_Publication(base_size = 18) +
    ylim(-7,0) +
    labs(x = "Time (post-gavage)",y="log10(Barcode frequency)") + guides(color = FALSE) +
    scale_x_continuous(breaks = effective.breaks,labels = effective.labels, limits = effective.limits) +
    coord_cartesian(expand = FALSE) + geom_line(data=tf,aes(time,value),color="black") +
    annotate("text", y=-0.75, x = 3,label=paste("n",length(unique(df$ID)),sep=" = "),hjust=0,size=5) +
    ggtitle(paste("Cluster",cluster,sep=" "))

  # must plot to extract fit
  ggsave(filename = paste(output_directory, sample, "_cluster", cluster, "_log10.eps", sep=""),width = 5.5,height = 4,device = cairo_ps)

  return(p)
}
