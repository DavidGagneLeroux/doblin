#' A fetch function
#'
#' This function allows you to fetch the top N barcodes per sample.
#' The top N barcodes are recovered according to their maximum and final frequencies.
#'
#' @param reshaped_df a dataframe produced by reshapeDF()
#' @param n_intersect integer representing the number of barcodes to fetch
#' @return A list containing 2 dataframes: top N barcodes according to their final
#'  frequencies and top N barcodes according to their maximum frequencies.
#' @export fetchTop

fetchTop <- function(reshaped_df, n_intersect) {
  df.top = unique(reshaped_df[,1:4])
  df.top.final = df.top[order(-df.top$final),]
  df.top.max = df.top[order(-df.top$max),]

  df.top.final = df.top.final[1:n_intersect,]
  df.top.max = df.top.max[1:n_intersect,]

  return(list(df.top.final,df.top.max))
}
