#' Filtering the hierarchical clusters
#'
#' Once the hierarchical clustering is done, we keep only the clusters with at least "n_members" members.
#' By ignoring clusters with fewer than"n_members" members, we are potentially ignoring dominant clusters.
#' To prevent this, we ask the user to provide a minimum average frequency that must be reached by at least
#' one of the lineages of potentially ignored clusters for them to be taken into account.
#'
#' @name filterHC
#' @param series_filtered a dataframe filtered by filterData()
#' @param clusters a dataframe containing the clusters from a hierarchical clustering
#'  for one or multiple thresholds
#' @param n_members an integer indicating the minimum number of members per cluster
#' @return A list containing a dataframe with the resulting filtered clusters, and
#' the minimum average frequency that must be reached by at least
#' one of the lineages of potentially ignored clusters for them to be taken into account
#' @import dplyr
#' @export filterHC

filterHC <- function(series_filtered, clusters, n_members){

  ## rank is just a way to numerate each cluster
  nRank = nrow(clusters)
  clusters$rank = seq(1:nRank)
  clusters_long=reshape2::melt(clusters,id.vars = c("rank"))
  colnames(clusters_long)=c("rank","cutoff","cluster")

  series_filtered$points = NULL
  series_filtered$rank=seq(1:nRank)
  series_filtered_long=reshape2::melt(series_filtered,id.vars = c('ID','rank','mean'), variable.name = "Time", value.name = "Frequency")

  series_reshaped = merge(series_filtered_long,clusters_long,by.x = "rank",by.y = "rank", all = TRUE)
  series_reshaped$Time = as.numeric(as.character(series_reshaped$Time))

  ## Group by cluster and keep only the clusters with at least n_members members
  series_reshaped_1=series_reshaped %>%  dplyr::group_by(cluster,cutoff) %>% dplyr::filter(length(unique(ID)) >= n_members)
  clusters_filtered = series_reshaped_1

  ## To avoid ignoring the dominant barcodes, which might be in smaller clusters, we add a second criteria:
  if(nrow(series_reshaped_1) != nrow(series_reshaped)){

    cat(paste("By ignoring clusters with fewer than",n_members," members, you are potentially ignoring dominant clusters."))

    if (interactive()) {
      min_freq_ignored_clusters <- as.numeric(readline(prompt = "Please indicate a minimum average frequency that must be reached by at least one of the lines of potentially ignored clusters for them to be taken into account: "))
    } else {
      cat("Please indicate a minimum average frequency that must be reached by at least one of the lines of potentially ignored clusters for them to be taken into account: ")
      min_freq_ignored_clusters <- as.numeric(readLines("stdin", n=1))
    }

    series_reshaped_2 = series_reshaped %>%  dplyr::group_by(cluster,cutoff) %>% dplyr::filter(length(unique(ID)) < n_members) %>% mutate(mean_freq = mean(Frequency)) %>% dplyr::filter(mean_freq >= min_freq_ignored_clusters)
    series_reshaped_2$mean_freq = NULL

    clusters_filtered = rbind(clusters_filtered, series_reshaped_2)
    rm(series_reshaped, series_reshaped_1, series_reshaped_2)
  }

  return(clusters_filtered)
}
