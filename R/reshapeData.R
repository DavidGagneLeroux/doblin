#' A reshape function
#'
#' This function allows you to reshape data for plotting. The given input is a
#' long-data format with ID, Time and Reads columns. We reshape it to have a resulting
#' dataframe containing  each of ID's max, start, final and mean frequencies.
#'
#' @param sample a dataframe produced by readr::read_csv
#' @return A reshaped dataframe for efficient plotting.
#' @export reshapeDF

reshapeDF <- function(sample) {

  ## Error message if raw_df doesn't have the appropriate format
  testing_colnames <- identical(colnames(sample), c("ID", "Time", "Reads"))
  if (testing_colnames == FALSE){
    stop("# The input data format has not been respected. Make sure your input file contains 3 columns named: ID, Time and Reads.")
  }

  ## Error message if the values of 'Reads' are not numeric
  if (is.numeric(sample$Reads) == FALSE){
    stop("# The values in 'Reads' column must be numeric.")
  }

  ## Error message if the values of 'Reads' are not numeric
  if (is.numeric(sample$Time) == FALSE){
    stop("# The values in 'Time' column must be numeric.")
  }

  sample$Reads = as.numeric(as.character(sample$Reads))
  # "table" uses "sample"'s IDs as IDs, Time values as column variables and Reads values as table values
  table = reshape2::dcast(sample, ID ~ Time, value.var = 'Reads')
  # converts a 'data.table' into a 'matrix'
  m = as.matrix(table[,-1])
  # converts a 'matrix' into a 'dataframe'
  mat = as.data.frame(sweep(m,2,colSums(m,na.rm = TRUE),`/`))
  mat$ID = table$ID
  mat[,"mean"] = apply(mat[,-ncol(mat), drop=F],1, mean,na.rm=TRUE)
  mat[,"max"] = apply(mat[,-c(ncol(mat),ncol(mat)-1), drop=F],1, max,na.rm=TRUE)
  mat$start = mat[,1]
  mat$final = mat[,ncol(mat)-4]

  # reshaping mat dataframe into long-format data
  df = reshape2::melt(mat,id.vars = c('ID','max','start','final','mean'))
  df$variable = as.numeric(levels(df$variable))[df$variable]

  df$ID = as.factor(df$ID)

  if (any(is.na(df$value))){
    df[is.na(df$value),]$value = 0
  }

  tf = df[order(df$max),]

  return(tf)
}

