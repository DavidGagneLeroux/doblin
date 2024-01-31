#' Get the clusters resulting from the hierarchical clustering of the barcoded lineages.
#'
#' This function uses the data filtered by filterData() to compute a distance matrix
#' according to pearson's or dtw's approach. From the resulting matrix, we proceed
#' to a hierarchical clustering of the data according to a given linkage method.
#' Then, in order to visualize the results,we plot a dendrogram and a heatmap.
#' Finally, if no threshold is provided, we compute the relative clusters for ALL
#' thresholds between 0.1 and maximum height of hierarchical clustering tree. This step allows the
#' user to visualize the possible clusters and make an informed choice about which
#' threshold to use.
#'
#' @param filtered_data a dataframe filtered by filterData()
#' @param linkage_method a character string indicating which method is to be used for linkage
#' @param similarity_metric a character string indicating which metric to be used to measure similarity between two time-series
#' @param missing_values a character string indicating which method is to be used to manage missing values while computing covariances
#' @return A dataframe containing the resulting clusters of a hierarchical clustering
#'  for one or multiple thresholds
#' @export perform_hierarchical_clustering


perform_hierarchical_clustering <- function(filtered_data, linkage_method=c("average", "ward.D","ward.D2", "centroid", "single", "complete", "median", "mcquitty"), similarity_metric=c("pearson","dtw"), missing_values = NULL){

  linkage_method <- match.arg(linkage_method)
  similarity_metric <- match.arg(similarity_metric)

  filtered_dataf=filtered_data[,!(colnames(filtered_data) %in% c("ID","mean","points"))]
  filtered_dataf[filtered_dataf == 0] <- NA

  # Color palette for heatmap
  color_palette <- grDevices::colorRampPalette(c("red", "white", "blue"))(n = nrow(filtered_dataf))

  ## Compute the distance matrix according to the chosen similarity metric and perform
  ## a hierarchical clustering
  if (similarity_metric=="pearson") {

    mat=log10(filtered_dataf)
    distmat=(as.matrix(1 - stats::cor(t(mat), use = missing_values, method = similarity_metric)))

    tryCatch({
      clust <- stats::hclust(stats::as.dist(distmat), method = linkage_method)
    }, error = function(e) {
      if (grepl("NA/NaN/Inf", e$message)) {
        stop("Error in hierarchical clustering: NA/NaN/Inf values found in the distance matrix.\n",
             "Try a higher time point cut-off (i.e. '-c' parameter of cmd line).")
      } else {
        stop(e)
      }
    })


  } else if (similarity_metric=="dtw") {

    ## Linear interpolation if threshold is NA value
    linear_interpolation= sapply(data.table::data.table(t(filtered_dataf)), imputeTS::na_interpolation)
    mat=log10(linear_interpolation)
    if(proxy::pr_DB$entry_exists("dtw_basic") == FALSE){
      # Just a way to add dtwclust functions to the registry
      proxy::pr_DB$set_entry(FUN=(dtwclust::dtw_basic), names=c("dtw_basic_3"))
    }

    cat("Enter the norm for the LCM calculation ('L1' for Manhattan or 'L2' for (squared) Euclidean): ")
    dtw_norm <- readLines("stdin", n=1)
    dtw_norm <- match.arg(dtw_norm, c("L1","L2"))

    distmat = proxy::dist(t(mat), method = "dtw_basic", normalize = TRUE, norm=dtw_norm)
    clust <- stats::hclust(stats::as.dist(distmat),method=linkage_method )
    tryCatch({
      clust <- stats::hclust(stats::as.dist(distmat), method = linkage_method)
    }, error = function(e) {
      if (grepl("NA/NaN/Inf", e$message)) {
        stop("Error in hierarchical clustering: NA/NaN/Inf values found in the distance matrix.\n",
             "Try a higher time point threshold (i.e. '-c' parameter of cmd line).")
      } else {
        stop(e)
      }
    })
  }

  ## Plot dendrogram:
  as.dendrogram(clust) -> dend

  grDevices::postscript(paste(output_directory, input_name,"_", similarity_metric, ".eps",sep=""),width = 5.5,height = 5)
  par(mar = c(2,2,2,2))

  ## Plot heatmap:
  gplots::heatmap.2(distmat,Rowv = dend,Colv = dend,col=rev(color_palette),density.info = "none",trace = "none",
                    key.xlab="(1 - r)",cexRow = 0.5,cexCol = 0.5, labRow = FALSE, labCol = FALSE)
  grDevices::dev.off()

  rm(distmat)

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

  return(cluster_file)
}
