#' A fetching function
#'
#' This function allows you to retrieve the top N_LINEAGES barcodes.
#' The top N_LINEAGES barcodes are recovered according to their maximum frequencies.
#'
#' @param reshaped_df a dataframe returned by reshapeData()
#' @param N_LINEAGES integer representing the number of barcodes to fetch
#' @return top 1000 barcodes according to their maximum frequencies.
#' @export fetchTop

fetchTop <- function(reshaped_df, N_LINEAGES) {

  df_top_max = unique(reshaped_df[,1:4])
  df_top_max = df_top_max[1:N_LINEAGES,]

  return(df_top_max)
}
