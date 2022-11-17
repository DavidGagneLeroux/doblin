#' A reshape function
#'
#' This function allows you to reshape data for plotting. The given input is a
#' long-data format with ID, Time and Reads columns. We reshape it to have a resulting
#' dataframe containing  each of ID's max, start, final and mean frequencies.
#'
#' @param raw_df a dataframe produced by readr::read_csv
#' @return A reshaped dataframe for efficient plotting.
#' @export reshapeDF

reshapeDF <- function(raw_df) {

  sample = raw_df

  # "table" uses "sample"'s IDs as IDs, Time values as column variables and Reads values as table values
  table = reshape2::dcast(sample, ID ~ Time, value.var = 'Reads')
  # converts a 'data.table' into a 'matrix'
  m = as.matrix(table[,-1])
  # converts a 'matrix' into a 'dataframe'
  mat = as.data.frame(sweep(m,2,colSums(m,na.rm = TRUE),`/`))
  mat$ID = table$ID
  mat[,"mean"] = apply(mat[,-ncol(mat)],1, mean,na.rm=TRUE)
  mat[,"max"] = apply(mat[,-c(ncol(mat),ncol(mat)-1)],1, max,na.rm=TRUE)
  mat$start = mat[,1]
  mat$final = mat[,ncol(mat)-4]

  # reshaping mat dataframe into long-format data
  df = reshape2::melt(mat,id.vars = c('ID','max','start','final','mean'))
  df$variable = as.numeric(levels(df$variable))[df$variable]

  df$ID = as.factor(df$ID)
  df[is.na(df$value),]$value = 0

  tf = df[order(df$max),]

  return(tf)
}

