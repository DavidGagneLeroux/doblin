#' Get the clusters resulting from a hierarchical clustering of the clonal lineages.
#'
#' This function uses the data filtered by filterData() to compute a distance matrix
#' according to pearson's or dtw's approach. From the resulting matrix, we proceed
#' to a hierarchical clustering of the data according to a given linkage method.
#' Then, in order to visualize the results,we plot a dendrogram and a heatmap.
#' Finally, if no threshold is provided, we compute the relative clusters for all
#' thresholds between 0.1 and max height of hierarchical clustering. This step allows the
#' user to visualize the possible clusters and make an informed choice about which
#' threshold to use.If a threshold is provided, the function returns a dataframe
#' containing the clusters related to that threshold.
#'
#' @param filtered_data a dataframe filtered by filterData()
#' @param sample_name a character string indicating the sample's name
#' @param linkage_method a character string indicating which method is to be used for linkage
#' @param clustering_method a character string indicating which method is to be used for clustering
#' @param covariance_method a character string indicating which method is to be used to compute the covariance
#' @param threshold an optional parameter to specify which threshold to use
#' @return A dataframe containing the clusters from a hierarchical clustering
#'  for one or multiple thresholds
#' @export get_clonal_lineage_clusters

## TODO:ward.D2?

get_clonal_lineage_clusters <- function(filtered_data, sample_name, linkage_method=c("average", "ward.D2", "centroid"), clustering_method=c("pearson","dtw"), covariance_method, threshold = NULL){

  #filtered_data = filtered_dataframes[[1]]
  #sample_name = cohort_names[[1]]
  #linkage_method = "average"
  linkage_method <- match.arg(linkage_method)
  #clustering_method = "dtw"
  clustering_method <- match.arg(clustering_method)
  #covariance_method ="pairwise.complete.obs"  # method for computing covariances in the presence of missing values
  #threshold = 0.5

  filtered_dataf=filtered_data[,!(colnames(filtered_data) %in% c("ID","mean","points"))]
  filtered_dataf[filtered_dataf == 0] <- NA
  mat=log10(filtered_dataf)

  my_palette <- grDevices::colorRampPalette(c("red", "white", "blue"))(n = nrow(mat))

  ## Compute the distance matrix according to the chosen clustering method and perform
  ## a hierarchical clustering
  if (clustering_method=="pearson") {

    distmat=(as.matrix(1 - stats::cor((t(mat)), use = covariance_method, method = clustering_method)))
    clust <- stats::hclust(stats::as.dist(distmat),method=linkage_method)

  } else if (clustering_method=="dtw") {

    ## Linear interpolation if threshold is NA value
    mat_interpolated= sapply(data.table::data.table(t(mat)), imputeTS::na_interpolation)
    if(!proxy::pr_DB$entry_exists("dtw_basic")){
      proxy::pr_DB$set_entry(FUN = dtwclust::dtw_basic, names=c("dtw_basic"), loop = TRUE, type = "metric", distance = TRUE)
    }
    ## dtw_basic is faster we  also normalize the distance
    distmat = proxy::dist(t(mat_interpolated), method = "dtw_basic", normalize = TRUE)
    clust <- stats::hclust(stats::as.dist(distmat),method=linkage_method )
  }

  ## Plot dendrogram:
  as.dendrogram(clust) -> dend
  png(paste(output_directory, sample_name,"_", clustering_method, ".png",sep=""),res = 300,width = 5.5,height = 5, units="in")
  par(mar = c(2,2,2,2))

  ## Plot heatmap:
  gplots::heatmap.2(distmat,Rowv = dend,Colv = dend,col=rev(my_palette),density.info = "none",trace = "none",
                    key.xlab="(1 - r)",cexRow = 0.5,cexCol = 0.5,LabCol=FALSE,labRow=FALSE)
  dev.off()

  if(is.null(threshold)){

    ## For all thresholds between 0.1 and max height of hierarchical clustering, we extract the relative
    ## clusters. This step allows the user to visualize the possible clusters and
    ## make an informed choice for the threshold.
    range<- seq(from=0.1, to=max(clust$height), by=0.01)
    cluster_file=list()
    for( i in 1:length(range)){
      cut_avg <- as.data.frame(cutree(clust, h=range[i]))
      names(cut_avg)[1]=range[i]
      cluster_file[[i]]=cut_avg
    }

    cluster_file=do.call(cbind,cluster_file)

  } else{
    ## When the user selected the desired threshold
    cluster_file = as.data.frame(cutree(clust, h=threshold))
    names(cluster_file)[1]=threshold
  }

  return(cluster_file)
}
