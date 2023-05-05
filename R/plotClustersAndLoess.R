#' Plot the log10-transformed barcode frequencies and the moving averages (LOESS)
#'
#' This file contains multiple functions. The main function is: plot_clusters_and_loess()
#' and it uses plotClusterLog10() and applyLOESS(). plot_clusters_and_loess() starts in
#' the same way as plotHCQuantification(), but without a cutoff column. In plot_clusters_and_loess(),
#' we plot the log10-transformed barcode frequencies contained in all selected clusters,
#' we compute a moving average per cluster and group them in a plot. We also write the files associated with
#' those two plots.
#'
#' @name plotClustersAndLoess
#' @param series.filtered a dataframe filtered by filterData()
#' @param selected.clusters  A dataframe containing the clusters from a hierarchical clustering
#'  for a specific threshold
#' @param sample_name a character string indicating the sample's name
#' @param effective.breaks a list containing the breaks for the plots
#' @param n_members an integer indicating the minimum number of members per cluster
#' @import dplyr
#' @import ggplot2
#' @export plot_clusters_and_loess

cluster.colors=c("#3cb44b","#4363d8","#e6194B","#e8ca00","#911eb4","#f58231","#22766dff","#42d4f4","#f032e6","#9A6324",
                 "#2F4C39", "#1D3F6E","#94170f","#665679","#F17829","#97A69C","#606EA9","#A9606E","#A99060","#F8F1AE",
                 "#bcf60c", "#fabebe", "#008080", "#e6beff", "#9a6324", "#fffac8")

plot_clusters_and_loess <- function(series.filtered, selected.clusters, sample_name, effective.breaks, n_members){

  #series.filtered = filtered_dataframes[[1]]
  #sample_name = "m1"
  #selected.clusters=selected_clusters
  #effective.breaks = breaks[[1]]
  #n_members = 8

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

  ## Group by cluster and keep only the clusters with at least n_members members
  series.reshaped.1=series.reshaped %>%  dplyr::group_by(cluster) %>% dplyr::filter(length(unique(ID)) >= n_members)

  ## To avoid ignoring the dominant barcodes, which might be in smaller clusters, we add a second criteria:
  if(nrow(series.reshaped.1) != nrow(series.reshaped)){

    cat("You might be ignoring dominant barcodes, please enter a minimum mean frequency that must be reached by at least one of the members for the cluster to be considered: ")
    #minimum_freq <-1e-03
    minimum_freq <- as.numeric(readLines("stdin", n=1))

    series.reshaped.2 = series.reshaped %>%  dplyr::group_by(cluster) %>% dplyr::filter(length(unique(ID)) < n_members) %>% mutate(mean_freq = mean(value)) %>% dplyr::filter(mean_freq >= minimum_freq)
    series.reshaped.2$mean_freq = NULL

    series.reshaped = rbind(series.reshaped.1, series.reshaped.2)

  }

  clusters_dataframe = apply_LOESS(series.reshaped, effective.breaks, sample_name)

  ## Write series.reshape
  readr::write_csv(series.reshaped,file = paste(output_directory, sample_name, "_clustered_series_log10.csv",sep=""),col_names = TRUE)


  ## loesss
  #TODO: considérer plusieurs cluster.df
  clusters_dataframe$cluster=paste("C",clusters_dataframe$cluster,sep="")
  clusters_dataframe$time=clusters_dataframe$time*10
  clusters.loess <- tidyr::spread(clusters_dataframe, cluster, value)

  # plot loess
  loess.plot = ggplot(clusters_dataframe) + geom_line(aes(x=time/10,y=10^(value),group=cluster,color=cluster),size=1) + scale_x_continuous(limits = effective.limits) +
    theme_Publication() + scale_color_manual(values = cluster.colors,name="cluster") + ylab("Clone frequency") + xlab("Time") + scale_y_log10(limits=c(min(10^clusters_dataframe$value),1e0))
  ggsave(loess.plot,filename = paste(output_directory, sample_name, "_loess_clusters_log10.eps", sep=""),width = 8.25,height = 6)

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

  # grDevices::cairo_ps(paste(output_directory, sample, "_cluster", cluster, "_log10.ps", sep=""),width = 5.5,height = 4)

  p = ggplot(df,aes(x=variable,y=value)) +
    geom_line(aes(group=ID),color=color) + theme_Publication(base_size = 18) +
    scale_y_log10(limits=c(min(df$value)+1e-7,1e0)) +
    labs(x = "Time",y="lineage frequency") + guides(color = FALSE) +
    scale_x_continuous(limits = effective.limits) +
    coord_cartesian(expand = FALSE) + geom_line(data=tf,aes(time,10^value),color="black") +
    annotate("text", y=-0.75, x = 3,label=paste("n",length(unique(df$ID)),sep=" = "),hjust=0,size=5) +
    ggtitle(paste("Cluster",cluster,sep=" "))

  # graphics::plot(p)
  # grDevices::dev.off()

  ggsave(p,filename =  paste(output_directory, sample, "_cluster", cluster, "_log10.eps", sep=""),width = 5.5,height = 4)

  return(p)
}

#################
#' @export
#' @rdname plotClustersAndLoess

apply_LOESS <- function(series_reshaped, effective.breaks, sample_name){

  #series_reshaped <- series.reshaped

  ## Keep only the persistent barcodes
  ## series_order only contains the data regarding the last generation/time-point
  series_order=subset(series_reshaped,series_reshaped$variable==max(unique(series_reshaped$variable)))

  series_order=series_order %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(average = mean(value))

  series_order=series_order[order(series_order$average,decreasing = TRUE), ]
  series_order$cluster_ranked=as.numeric(rownames(series_order))
  series_reshaped=merge(series_reshaped,series_order,by="cluster")
  series_reshaped$cluster=NULL
  names(series_reshaped)[7]="cluster"
  clusters.ranked=series_order$cluster_ranked

  max.range = max(series_reshaped$variable)-min(series_reshaped$variable)
  loess.range = (max.range*10)+1

  ## Get moving average of barcode frequencies for each cluster USING LOESS
  xx <- seq(from=min(series_reshaped$variable), to=max(series_reshaped$variable),length.out = loess.range)

  cluster.df=series_reshaped %>%  dplyr::group_by(cluster) %>%
    dplyr::summarise(value=predict(adjust_span(variable, value, span = 0.2),xx,se = FALSE))  %>%
    dplyr::group_by(cluster) %>% dplyr::mutate(time=xx)

  ## Plot log10-transformed barcode frequencies
  plotList = list()

  for(i in seq_along(unique(series_reshaped$cluster))){
    l = plotClusterLog10(series_reshaped[series_reshaped$cluster==clusters.ranked[i],], clusters.ranked[i], sample_name, cluster.colors[i],cluster.df[cluster.df$cluster==clusters.ranked[i],], effective.breaks)
    plotList[[i]]=l

  }


  #top10.log =cowplot::plot_grid(plotlist=plotList, align = "hv",axis="tblr")
  #ggsave(top10.log,filename = paste(output_directory, sample_name, "_clusters_log10_", as.character(clusters.ranked[i]) ,".eps", sep=""))

  return(cluster.df)
}



