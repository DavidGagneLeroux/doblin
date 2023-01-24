#' Plot hierarchical clustering quantification
#'
#' This file contains multiple functions. The main function is plotHCQuantification()
#' and it uses melt_dist(). plotHCQuantification() is called right after get_clonal_lineage_clusters()
#' and uses the resulting dataframe. First, we compute the moving average of barcode
#' frequencies for each cluster USING LOESS. The moving average is computed using only
#' the persistent barcodes and each cluster must contain at least 8 members. Then,
#' we calculate the smallest distance between clones (for all thresholds). We plot
#' a graph representing the smallest distance between two clusters and the number of clusters
#' according to the threshold.
#'
#' @name plotHCQuantification
#' @param filtered_data a dataframe filtered by filterData()
#' @param sample_name a character string indicating the sample's name
#' @param clusters a dataframe containing the clusters from a hierarchical clustering
#'  for one or multiple thresholds
#' @import dplyr
#' @import ggplot2
#' @export plotHCQuantification

#library("dplyr")
#library("ggplot2")

plotHCQuantification <- function(filtered_data, sample_name, clusters){

  #clusters = clusters_df
  #filtered_data = filtered_dataframes[[1]]
  #sample_name = cohort_names[[1]]

  ## rank is just a way to numerate each barcode
  nRank = nrow(clusters)
  clusters$rank = seq(1:nRank)
  clusters_long=reshape2::melt(clusters,id.vars = c("rank"))
  colnames(clusters_long)=c("rank","cutoff","cluster")

  series.filtered <- filtered_data
  series.filtered$points = NULL
  series.filtered$rank=seq(1:nRank)
  series.filtered_long=reshape2::melt( series.filtered,id.vars = c('ID','rank','mean'))

  ## series.reshaped is a dataframe where "variable" represents the time-point (1-18) and value
  ## represents the barcode frequencies
  series.reshaped = merge(series.filtered_long,clusters_long,by.x = "rank",by.y = "rank", all = TRUE)
  series.reshaped$variable = as.numeric(as.character(series.reshaped$variable))

  ## Group by cluster & cutoff and keep only the clusters with at least 8 members
  series.reshaped=series.reshaped %>%  dplyr::group_by(cluster,cutoff) %>% dplyr::filter(length(unique(ID)) >= 8)

  ## Keep only the persistent barcodes
  series_order=subset(series.reshaped,series.reshaped$variable==max(unique(series.reshaped$variable)))

  series_order=series_order %>%
    dplyr::group_by(cluster,cutoff) %>%
    dplyr::summarise(average = mean(value)) %>% dplyr::ungroup() %>%
    dplyr::group_by(cutoff) %>%
    dplyr::arrange(desc(average), .by_group = TRUE)  %>% dplyr::mutate(cluster2=as.factor(row_number()))

  series.reshaped=merge(series.reshaped,series_order,by=c("cluster","cutoff"))
  series.reshaped$cluster=NULL
  names(series.reshaped)[8]="cluster"

  max.range = max(series.reshaped$variable)-min(series.reshaped$variable)
  loess.range = (max.range*10)+1

  ## Get moving average of barcode frequencies for each cluster USING LOESS
  xx <- seq(from=min(series.reshaped$variable), to=max(series.reshaped$variable),length.out = loess.range)

  df=series.reshaped %>%  dplyr::group_by(cluster,cutoff) %>%
    dplyr::summarise(model=predict(loess(log10(value+0.0000001)~variable,span=0.2),xx,se = FALSE))  %>%
    dplyr::group_by(cluster,cutoff) %>% dplyr::mutate(time=xx)

  rm(series.reshaped)

  df$cluster=as.factor(as.integer(df$cluster))
  df$cutoff=as.numeric(as.character(df$cutoff))
  cutoff=unique(df$cutoff)

  ## Calculate the smallest distance between clones depending on threshold.
  ## TSDistances: computes distances between pairs of time series. (TSdist::)
  if (!proxy::pr_DB$entry_exists("TSDistances")){
    proxy::pr_DB$set_entry(FUN = TSdist::TSDistances, names=c("TSDistances"), loop = TRUE, type = "metric", distance = TRUE)
  }
  tf = df %>% split(.$cutoff)  %>%
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

  rm(distance_pairwise)

  scale=max(smallest_distance$dist_small)/max(smallest_distance$cluster)

  ## Plot the quantification

  choose_threshold = ggplot(smallest_distance,aes(as.numeric(as.character(cutoff)),dist_small))+ geom_line(color="black", size=2.5) +
    theme_Publication() + geom_line(aes(as.numeric(as.character(cutoff)), as.integer(as.character(cluster))*scale), size = 2, color = "darkblue") +
    scale_y_continuous(sec.axis = sec_axis(~./scale,name="Cluster number")) + scale_x_reverse() +xlab("Threshold")

  ggsave(choose_threshold,filename =  paste(output_directory,sample_name, "_threshold_selection", ".png",sep=""),width = 9,height = 8, device =cairo_ps())
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

