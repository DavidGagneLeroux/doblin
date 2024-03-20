#' Filtering the input file
#'
#' This function is used to clean the input data given by the user in order to perform
#' the clustering. We set a minimum mean frequency and a minimum time-point threshold below which
#' the data is not retained for clustering. The function writes a csv file containing the ID,
#' frequency at each time point, average frequency and duration of each remaining barcode.
#'
#' @param input_df input file given by the user
#' @param freq_treshold a double representing a minimum mean frequency below which
#' the data is not retained for clustering.
#' @param time_threshold an integer indicating the time-point threshold (i.e. we keep the lineages with at least "time_threshold" non-zero time points)
#' @return a matrix containing the ID,
#' frequency at each time point, average frequency and duration of each remaining barcode
#' @export filterData

filterData <- function(input_df, freq_threshold, time_threshold){

  sample = reshape2::dcast(input_df, ID ~ Time, value.var = 'Reads')

  m = as.matrix(sample[,-1])
  mat = as.data.frame(sweep(m,2,colSums(m,na.rm = TRUE),`/`))
  mat$ID = sample$ID
  mat[,"mean"] = apply(mat[,-ncol(mat)],1, mean,na.rm=TRUE)

  z = is.na.data.frame(mat)
  mat[z]=0
  mat$points = apply(mat[,-c(ncol(mat)-1, ncol(mat))], 1, function(c)sum(c!=0))

  readr::write_csv(mat,file=paste(output_directory, input_name,"_unfiltered.csv",sep=""),col_names = TRUE) # Contains ALL the lineages from the input file

  sample.clustering = mat[mat$points>=time_threshold & mat$mean>=freq_threshold,]

  # Check if sample.clustering is empty
  if (nrow(sample.clustering) == 0) {
    stop("Error: With the given time & frequency thresholds, there is no eligible
         data for clustering. Please consider adjusting your thresholds.")
  } else {
    # Write sample.clustering to CSV file
    readr::write_csv(sample.clustering, file = paste(output_directory, input_name, "_filtered.csv", sep = ""), col_names = TRUE) # Contains ONLY the "dominant" and "persistent" lineages
  }

  return(sample.clustering)
}
