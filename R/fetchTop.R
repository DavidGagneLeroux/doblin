#' A fetch function
#' This function allows you to fetch the top N barcodes per sample.
#' @export

fetchTop <- function(reshaped_df, n_intersect) {
  df.top = unique(reshaped_df[,1:4])
  df.top.final = df.top[order(-df.top$final),]
  df.top.max = df.top[order(-df.top$max),]

  df.top.final = df.top.final[1:n_intersect,]
  df.top.max = df.top.max[1:n_intersect,]

  return(list(df.top.final,df.top.max))
}
