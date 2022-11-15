#' A reshape function
#' This function allows you to reshape data for plotting.
#' @export

reshapeDF <- function(raw_df) {

  sample = raw_df

  # reshaping from long- to wide-data format. test is a table that uses sample's IDs as IDs, sample's Time values as column variables (1@18) and sample's Reads values as table values.
  test = reshape2::dcast(sample, ID ~ Time, value.var = 'Reads')
  # converts a 'data.table' into a 'matrix'.
  m = as.matrix(test[,-1])
  # converts a 'matrix' into a 'dataframe'.
  mat = as.data.frame(sweep(m,2,colSums(m,na.rm = TRUE),`/`))
  mat$ID = test$ID
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

