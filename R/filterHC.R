#' Filter Hierarchical Clusters Based on Size and Dominance
#'
#' This function filters the results of hierarchical clustering by retaining only clusters
#' that contain at least `n_members` unique lineages. To avoid excluding potentially dominant but small clusters,
#' the user is prompted to specify a minimum average frequency. Any small clusters with at least one lineage
#' exceeding this frequency threshold will be retained.
#'
#' @param series_filtered A data frame preprocessed using `filterData()`, containing lineage frequencies and metadata.
#' @param clusters A data frame containing hierarchical clustering assignments (e.g., from `cutree()`), possibly across multiple thresholds.
#' @param n_members An integer specifying the minimum number of members (lineages) required for a cluster to be retained.
#'
#' @return A data frame containing the filtered clusters, including both large clusters and small clusters with at least
#' one dominant member (based on average frequency threshold).
#' @import dplyr
#' @export
#' @name filterHC
#' 
#' @examples
#' \dontrun{ 
#' # Load demo barcode count data (installed with the package)
#' demo_file <- system.file("extdata", "demo_input.csv", package = "doblin")
#' input_dataframe <- readr::read_csv(demo_file, show_col_types = FALSE)
#'
#' # Filter data to retain dominant and persistent barcodes
#' filtered_df <- filterData(
#'   input_df = input_dataframe,
#'   freq_threshold = 0.00005,
#'   time_threshold = 5,
#'   output_directory = tempdir(),
#'   input_name = "demo"
#' )
#'
#' # Perform hierarchical clustering using Pearson correlation
#' # Note: If similarity_metric = "dtw" is used instead, note that it requires interactive input
#' cluster_assignments <- performHClustering(
#'   filtered_data = filtered_df,
#'   agglomeration_method = "average",
#'   similarity_metric = "pearson",
#'   missing_values = "pairwise.complete.obs",
#'   output_directory = tempdir(),
#'   input_name = "demo"
#' )
#' 
#' # Filter clusters to retain only those with at least 8 members,
#' #         unless they contain a dominant lineage
#' #         (this step prompts the user for an average frequency threshold)
#' filtered_clusters <- filterHC(
#'   series_filtered = filtered_df,
#'   clusters = cluster_assignments,
#'   n_members = 8
#' )
#' }

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

    warning(paste("By ignoring clusters with fewer than",n_members," members, you are potentially ignoring dominant clusters."))

    if (interactive()) {
      min_freq_ignored_clusters <- as.numeric(readline(prompt = "Please indicate a minimum average frequency that must be reached by at least one of the lines of potentially ignored clusters for them to be taken into account: "))
    } else if (pipeline_choice == "yes") {
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
