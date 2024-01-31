#' A reshape function
#'
#' This function allows you to reshape your data for further manipulation.
#' The given input is a format with ID, Time and Reads columns.
#' We reshape it to obtain a dataframe containing the max, start, final and average frequencies of each ID.
#'
#' @param input_data a dataframe with ID, Time and Reads columns.
#' @return A dataframe containing the max, start, final and average frequencies of each ID in decreasing order of maximum frequency.
#' @export reshapeData

reshapeData <- function(input_data) {

  ## Error message if input_data doesn't have the appropriate format
  testing_colnames <- identical(colnames(input_data), c("ID", "Time", "Reads"))
  if (testing_colnames == FALSE){
    stop("# The input data format has not been respected. Make sure your input file contains 3 columns named: ID, Time and Reads.")
  }

  ## Error message if the values of 'Reads' are not numeric
  if (is.numeric(input_data$Reads) == FALSE){
    stop("# The values in 'Reads' column must be numeric.")
  }

  ## Error message if the values of 'Reads' are not numeric
  if (is.numeric(input_data$Time) == FALSE){
    stop("# The values in 'Time' column must be numeric.")
  }

  # "table" uses "input_data"'s IDs as IDs, Time as variables and Reads as values
  table = reshape2::dcast(input_data, ID ~ Time, value.var = 'Reads')

  # converts a 'data.table' into a 'matrix'
  m = as.matrix(table[,-1])

  # converts a 'matrix' into a 'dataframe'
  mat = as.data.frame(sweep(m,2,colSums(m,na.rm = TRUE),`/`)) # we normalize READS to get the frequencies/abundances
  mat$ID = table$ID
  mat[,"mean"] = apply(mat[,-ncol(mat), drop=F],1, mean,na.rm=TRUE)
  mat[,"max"] = apply(mat[,-c(ncol(mat),ncol(mat)-1), drop=F],1, max,na.rm=TRUE)
  mat$start = mat[,1]
  mat$final = mat[,ncol(mat)-4]

  # reshaping mat dataframe into long-format data
  df = reshape2::melt(mat,id.vars = c('ID','max','start','final','mean'))
  names(df)[names(df) == 'variable'] <- 'Time'
  names(df)[names(df) == 'value'] <- 'Frequency'
  df$Time = as.numeric(levels(df$Time))[df$Time]
  df$ID = as.factor(df$ID)

  # Set NAs to 0
  if (any(is.na(df$Frequency))){
    df[is.na(df$Frequency),]$Frequency = 0
  }

  df = df[order(df$max, decreasing = TRUE),]

  return(df)
}

