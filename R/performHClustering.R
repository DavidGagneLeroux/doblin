#' Perform Hierarchical Clustering on Barcoded Lineages
#'
#' This function performs hierarchical clustering on time-series data representing barcoded lineages.
#' A distance matrix is computed using either Pearson correlation or Dynamic Time Warping (DTW),
#' and hierarchical clustering is applied using a specified agglomeration method.
#' A dendrogram and heatmap are generated for visual inspection. If no threshold is specified,
#' clusters are computed for all possible thresholds between 0.1 and the maximum tree height.
#'
#' @param filtered_data A data frame preprocessed with `filterData()`, containing filtered lineage frequencies.
#' @param agglomeration_method A character string specifying the agglomeration method (e.g., `"ward.D"`, `"complete"`).
#' @param similarity_metric A character string specifying the similarity metric (`"pearson"` or `"dtw"`).
#' @param output_directory A string specifying the directory where plots will be saved.
#' @param input_name A string used as the base name for output files (e.g., "replicate1")
#' @param missing_values Optional. A character string specifying how missing values should be handled in Pearson correlation (e.g., `"pairwise.complete.obs"`).
#' @param dtw_norm Optional. A character string specifying the norm to use with DTW distance ("L1" for Manhattan, "L2" for Euclidean). Required if `similarity_metric = "dtw"`.
#' @param dtw_alignments Optional. Character string indicating whether to visualize 
#' DTW-aligned time series ("yes" or "no"). Required if `similarity_metric = "dtw"`. 
#' Use "yes" to display time series aligned according to their DTW warping paths, 
#' typically for quality control purposes.
#' 
#' @import ggplot2
#' @import dtw
#' @import dplyr
#' @import tidyr
#' @import readr
#' @importFrom gridExtra arrangeGrob
#'
#' @return A data frame with clustering assignments at multiple thresholds (columns named by height).
#' @export
#' @name performHClustering
#' 
#' @examples
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
#' cluster_assignments <- performHClustering(
#'   filtered_data = filtered_df,
#'   agglomeration_method = "average",
#'   similarity_metric = "pearson",
#'   output_directory = tempdir(),
#'   input_name = "demo",
#'   missing_values = "pairwise.complete.obs",
#'   dtw_norm = NULL,
#'   dtw_alignments = NULL
#' )


performHClustering <- function(filtered_data,
                               agglomeration_method,
                               similarity_metric,
                               output_directory,
                               input_name,
                               missing_values = NULL,
                               dtw_norm = NULL,
                               dtw_alignments = NULL){

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
      clust <- stats::hclust(stats::as.dist(distmat), method = agglomeration_method)
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

    if (is.null(dtw_norm)) {
      stop("You must provide a value for 'dtw_norm' when using similarity_metric = 'dtw'. Choose either 'L1' or 'L2'.")
    }

    distmat = proxy::dist(t(mat), method = "dtw_basic", normalize = TRUE, norm=dtw_norm)
    ## Convert distmat object to matrix
    distmat <- as.matrix(distmat)
    
    clust <- stats::hclust(stats::as.dist(distmat),method=agglomeration_method )
    tryCatch({
      clust <- stats::hclust(stats::as.dist(distmat), method = agglomeration_method)
    }, error = function(e) {
      if (grepl("NA/NaN/Inf", e$message)) {
        stop("Error in hierarchical clustering: NA/NaN/Inf values found in the distance matrix.\n",
             "Try a higher time point threshold (i.e. '-c' parameter of cmd line).")
      } else {
        stop(e)
      }
    })
    
    if (is.null(dtw_alignments)) {
      stop("You must provide a value for 'dtw_alignments' when using similarity_metric = 'dtw'. Choose either 'yes' or 'no'.")
    }
    
    if (dtw_alignments == "yes"){
      
      # Get only numeric column names
      numeric_cols <- colnames(filtered_dataf)[suppressWarnings(!is.na(as.numeric(colnames(filtered_dataf))))]
      x_labels <- as.numeric(numeric_cols)
      # Step 2: Choose which x-values you want labeled
      label_positions <- scales::breaks_pretty(n = 5)(x_labels)
      
      # Step 3: Convert label values to index positions (since x in the plot is 1:length(ts1))
      label_indices <- which(x_labels %in% label_positions)
      
      data=t(mat)
      
      # Step 1: Compute row means
      row_means <- rowMeans(data, na.rm = TRUE)
      ordered_indices <- order(row_means, decreasing = TRUE)
      
      # Step 3: Reorder matrix
      ranked_data <- data[ordered_indices, ]
      
      data=ranked_data[1:10,]
      n_series <- nrow(data)
      
      
      # Set distance type
      dist_method <- if (dtw_norm == "L1") "Manhattan" else "Euclidean"
      
      # Example: your data should be in a matrix or data frame format (n_series x timepoints)
      # data <- your_time_series_data
      # n_series <- nrow(data)
      
      # Replace this with your actual data and number of series
      # For demonstration only:
      # data <- matrix(rnorm(100 * 5), nrow = 5)  # 5 series, each of length 100
      # n_series <- nrow(data)
      
      for (i in 1:(n_series - 1)) {
        for (j in (i + 1):n_series) {
          
          ts1 <- data[i, ]
          ts2 <- data[j, ]
          
          # Compute DTW alignment
          alignment <- dtw(ts1, ts2, keep = TRUE, dist.method = dist_method)
          
          index1 <- alignment$index1
          index2 <- alignment$index2
          
          warped1 <- ts1[index1]
          warped2 <- ts2[index2]
          
          # Data for warped sequences
          warped_df <- data.frame(
            index = 1:length(warped1),
            series1 = warped1,
            series2 = warped2
          ) %>%
            pivot_longer(cols = starts_with("series"), names_to = "series", values_to = "value")
          
          # Data for alignment path (index mapping)
          align_df <- data.frame(
            ts1_index = alignment$index1,
            ts2_index = alignment$index2,
            ts1_value = ts1[alignment$index1],
            ts2_value = ts2[alignment$index2]
          )
          
          # Plot 1: DTW alignment path (index1 vs index2)
          p1 <- ggplot(align_df, aes(x = ts1_index, y = ts2_index)) +
            geom_path() +
            geom_point(size = 0.5) +
            labs(title = "DTW Alignment Path",
                 x = sprintf("Series %d Index", i),
                 y = sprintf("Series %d Index", j)) +
            theme_Publication_DTW()+
            scale_x_continuous(
              breaks = 1:length(x_labels),
              labels = function(x) ifelse(x %in% label_indices, x_labels[x], "")
            ) +
            scale_y_continuous(
              breaks = 1:length(x_labels),
              labels = function(y) ifelse(y %in% label_indices, x_labels[y], "")
            ) 
          
          # Plot 2: Warped sequences overlay
          p2 <- ggplot(warped_df, aes(x = index, y = value, color = series)) +
            geom_line() +
            scale_color_manual(
              values = c("series1" = "red", "series2" = "blue"),
              labels = c(sprintf("Series %d", i), sprintf("Series %d", j)),
              name = "Series"
            ) +
            labs(
              title = sprintf("Warped Sequences: %d vs %d", i, j),
              x = "Warped Index", y = "Value"
            ) +
            theme_Publication_DTW()
          
          
          # Plot 3: Original sequences with alignment lines
          p3 <- ggplot() +
            geom_line(data = data.frame(index = 1:length(ts1), value = ts1),
                      aes(x = index, y = value), color = "red") +
            geom_line(data = data.frame(index = 1:length(ts2), value = ts2),
                      aes(x = index, y = value), color = "blue") +
            geom_segment(data = align_df,
                         aes(x = ts1_index, y = ts1_value,
                             xend = ts2_index, yend = ts2_value),
                         alpha = 1, color = "gray") +
            labs(title = "Warped Sequences with Alignment Path",
                 x = "Index", y = "Value") +
            theme_Publication_DTW()  +
            scale_x_continuous(
              breaks = 1:length(x_labels),             # Show full data range
              labels = function(x) ifelse(x %in% label_indices, x_labels[x], "")  # Label only selected
            )
          
          # Save combined plot
          combined_plot <- gridExtra::arrangeGrob(p1, p2, p3, ncol = 3)
          
          # Save as EPS using ggsave
          ggsave(
            filename = sprintf("%s/%s_dtw_alignments_Series_%d_vs_%d_combined.eps", 
                               output_directory, input_name, i, j),
            plot = combined_plot,
            device = "eps",
            width = 35, height = 8
          )
        }
      }
      
    }
    
    
  }

  ## Plot dendrogram:
  stats::as.dendrogram(clust) -> dend

  grDevices::postscript(paste(output_directory, "/", input_name,"_", similarity_metric, ".eps",sep=""),width = 5.5,height = 5)
  #output_filename <- paste(output_directory, input_name, "_", similarity_metric, ".png", sep = "")
  #png(output_filename, width = 5.5, height = 5)
  
  # Storing old par()
  oldpar <- graphics::par(no.readonly = TRUE)
  # Restore old par() before exiting the function
  on.exit(graphics::par(oldpar))
  
  
  graphics::par(mar = c(2,2,2,2))
  
  

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
    cut_avg <- as.data.frame(stats::cutree(clust, h=range[i]))
    names(cut_avg)[1]=range[i]
    cluster_file[[i]]=cut_avg
  }

  cluster_file=do.call(cbind,cluster_file)

  return(cluster_file)
}
