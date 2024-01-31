#' Plot hierarchical clustering quantification
#'
#' This file contains multiple functions. The main function is plotHCQuantification()
#' and it uses melt_dist(), applyLOESS() and adjust_span(). plotHCQuantification() is called right after perform_hierarchical_clustering()
#' and uses the resulting dataframe. First, we compute the moving average of barcode
#' frequencies for each cluster USING LOESS. The moving average is computed using only
#' the persistent barcodes and each cluster must contain at least N members. Then,
#' we calculate the smallest distance between clones (for all thresholds). We plot
#' a graph representing the smallest distance between two clusters and the number of clusters
#' according to the threshold.
#'
#' @name plotHCQuantification
#' @param clusters_filtered a dataframe filtered by filterHC()
#' @import dplyr
#' @import ggplot2
#' @export plotHCQuantification

plotHCQuantification <- function(clusters_filtered){

  clusters_dataframe = applyLOESS(clusters_filtered)

  clusters_dataframe$cluster=as.factor(as.integer(clusters_dataframe$cluster))
  clusters_dataframe$cutoff=as.numeric(as.character(clusters_dataframe$cutoff))
  cutoff=unique(clusters_dataframe$cutoff)

  ## Calculate the smallest distance between clones depending on threshold.
  ## TSDistances: computes distances between pairs of time series. (TSdist::)
  if (!proxy::pr_DB$entry_exists("TSDistances")){
    proxy::pr_DB$set_entry(FUN = TSdist::TSDistances, names=c("TSDistances"), loop = TRUE, type = "metric", distance = TRUE)
  }
  tf = clusters_dataframe %>% split(.$cutoff)  %>%
    purrr::map(~{
      tidyr::spread(.x, key = cluster, value = model)
    }) %>%  purrr::map(~{
      proxy::dist(t(.x[c(-1,-2)]), method="TSDistances", distance="euclidean")
    })

  distance_pairwise=lapply(tf, function(x) melt_dist(as.matrix(x)))
  distance_pairwise=do.call(rbind, (purrr::imap(distance_pairwise, ~mutate(.x, cutoff = .y))))

  smallest_distance<- distance_pairwise %>%
    dplyr::mutate(cluster= as.numeric(iso1)) %>%
    dplyr::group_by(cutoff) %>%
    dplyr::mutate(cluster=max(cluster),id=paste(iso1,iso2 ,sep="_")) %>%
    dplyr::summarise(dist_small=min(dist),cluster=max(cluster))

  readr::write_csv(smallest_distance,file = paste(output_directory, input_name, "_threshold_selection.csv",sep=""),col_names = TRUE)

  rm(distance_pairwise)

  scale=max(smallest_distance$dist_small)/max(smallest_distance$cluster)

  ## Plot the quantification

  choose_threshold = ggplot(smallest_distance,aes(as.numeric(as.character(cutoff)),dist_small))+ geom_line(color="black", size=2.5) +
    theme_Publication() + geom_line(aes(as.numeric(as.character(cutoff)), as.integer(as.character(cluster))*scale), size = 2, color = "#56B4E9") +
    scale_y_continuous(sec.axis = sec_axis(~./scale,name="Number of clusters")) + scale_x_reverse() +xlab("Threshold") + ylab("Distance between clusters")

  ggsave(choose_threshold,filename =  paste(output_directory,input_name, "_threshold_selection", ".eps",sep=""),width = 9,height = 8)
}

#################
#' @export
#' @rdname plotHCQuantification

melt_dist <- function(dist, order = NULL, dist_name = 'dist') {
  if(!is.null(order)){
    dist <- dist[order, order]
  } else {
    order <- row.names(dist)
  }
  diag(dist) <- NA
  dist[upper.tri(dist)] <- NA
  dist_df <- as.data.frame(dist)
  dist_df$iso1 <- row.names(dist)
  dist_df <- dist_df %>%
    tidyr::gather_(key = "iso2", value = lazyeval::interp("dist_name", dist_name = as.name(dist_name)), order, na.rm = T)
  return(dist_df)
}

#################
#' @export
#' @rdname plotHCQuantification

applyLOESS <- function(clusters_filtered){

  ## Keep only the persistent barcodes
  series_order=subset(clusters_filtered,clusters_filtered$Time==max(unique(clusters_filtered$Time)))

  series_order=series_order %>%
    dplyr::group_by(cluster,cutoff) %>%
    dplyr::summarise(average = mean(Frequency)) %>% dplyr::ungroup() %>%
    dplyr::group_by(cutoff) %>%
    dplyr::arrange(desc(average), .by_group = TRUE)  %>% dplyr::mutate(cluster2=as.factor(row_number()))

  series_order$cluster2=as.integer(as.character(series_order$cluster2))
  clusters_filtered=merge(clusters_filtered,series_order,by=c("cluster","cutoff"))
  clusters_filtered$cluster=NULL
  names(clusters_filtered)[8]="cluster"

  max.range = max(clusters_filtered$Time)-min(clusters_filtered$Time)
  loess.range = (max.range*10)+1

  ## Get moving average of barcode frequencies for each cluster USING LOESS
  xx <- seq(from=min(clusters_filtered$Time), to=max(clusters_filtered$Time),length.out = loess.range)

  cluster_df=clusters_filtered %>%  dplyr::group_by(cluster,cutoff) %>%
    dplyr::summarise(model=predict(adjust_span(Time, Frequency, span = 0.2),xx,se = FALSE))  %>%
    dplyr::group_by(cluster,cutoff) %>% dplyr::mutate(time=xx)

  return(cluster_df)
}


