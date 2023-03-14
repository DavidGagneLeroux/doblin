#' Filtering the input files
#'
#' This function is used to clean the input data given by the user in order to perform
#' the clustering. We set a minimum mean frequency and a minimum time-point threshold below which
#' the data is not retained. The function writes a csv file containing the ID,
#' frequency at each time point, average frequency and duration of each remaining barcode.
#'
#' @param input_df input file processed by read_csv()
#' @param freq_treshold a double representing a minimum mean frequency below which
#' the data is not retained.
#' @param time_threshold an integer indicating the time-point threshold (keep barcodes with at least x non-zero time points)
#' @param name a character string indicating the sample's name
#' @return a matrix containing the ID,
#' frequency at each time point, average frequency and duration of each remaining barcode
#' @export filterData

filterData <- function(input_df, freq_treshold, time_threshold, name){

  # input_df = input_data[[1]]
  # time_threshold = 14
  # name="M1"

  sample = reshape2::dcast(input_df, ID ~ Time, value.var = 'Reads')

  m = as.matrix(sample[,-1])
  mat = as.data.frame(sweep(m,2,colSums(m,na.rm = TRUE),`/`))
  mat$ID = sample$ID
  mat[,"mean"] = apply(mat[,-ncol(mat)],1, mean,na.rm=TRUE)

  z = is.na.data.frame(mat)
  mat[z]=0
  mat$points = apply(mat[,-c(ncol(mat)-1, ncol(mat))], 1, function(c)sum(c!=0))

  sample.clustering = mat[mat$points>time_threshold & mat$mean>freq_treshold,]

  readr::write_csv(sample.clustering,file=paste(output_directory, name,"_filtered.csv",sep=""),col_names = TRUE)
  return(sample.clustering)
}
