#' Here we calculate the running frequency correlations with initial library
#'  for all samples.
#'
#' @param index_dataframe dataframe returned by plotCorrelation()
#' @param sample_name the sample's name
#' @return A dataframe containing the running frequency correlations between a
#' sample and the initial library for all timepoints
#' @export calculateSpearmanCorr

calculateSpearmanCorr <- function(index_dataframe, sample_name){

  times=sort(unique(index_dataframe$Time))
  sample.cor = c()

  for(i in times){
    v = cor(index_dataframe[index_dataframe$Time==i,]$freq,index_dataframe[index_dataframe$Time==i,]$frequency,method = "spearman",use = "complete.obs")
    sample.cor = c(sample.cor,v)
  }
  sample.cor = as.data.frame(cbind(sample.cor,times))
  sample.cor$sample = sample_name
  colnames(sample.cor)[1] = "spearman"

  return(sample.cor)
}
