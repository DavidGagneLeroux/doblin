#' Here we compute the Jaccard index for all pairs of sets (either top barcodes
#' per sample by maximum frequency or by final frequency). The Jaccard similarity
#' index compares members from two sets to see: which members are shared and which
#' are distinct. The higher the percentage, the more similar the two populations are.
#'
#' This file contains multiple functions. The main function is: plotJaccardIndex()
#' and it uses jaccardIndex() and pairwiseJaccard() which are all defined here.
#'
#' @name jaccardIndex
#' @param topFinal a list of dataframes containing barcode frequencies (sorted by final freq.) of all samples
#' @param topMax a list of dataframes containing barcode frequencies (sorted by max freq.) of all samples
#' @param library_topN a dataframe containing the top N most frequent barcodes of the initial library
#' @param names a list of sample names
#' @param l1 a list containing the top N IDs of the samples and the initial library
#' @param s1 a set of top IDs
#' @param s2 a set of top IDs
#' @import ggplot2
#' @export plotJaccardIndex


## PLOT THE JACCARD INDICES

plotJaccardIndex <- function(topFinal, topMax, library_topN, names){


  dfFinal <- list()
  dfMax <- list()
  for(i in 1:length(topFinal)) {
    dfFinal <- append(dfFinal, list(as.character(topFinal[[i]]$ID)))
    dfMax <- append(dfMax, list(as.character(topMax[[i]]$ID)))
  }

  dfFinal <- append(dfFinal, list(library_topN$ID))
  dfMax <- append(dfMax, list(library_topN$ID))
  dfNames <- names
  dfNames <- append(dfNames, "reference")

  jaccardFinal = as.matrix(pairwiseJaccard(dfFinal))
  colnames(jaccardFinal) = dfNames
  rownames(jaccardFinal) = dfNames

  jaccardMax = as.matrix(pairwiseJaccard(dfMax))
  colnames(jaccardMax) = dfNames
  rownames(jaccardMax) = dfNames

  matFinal = reshape2::melt(jaccardFinal)
  matMax = reshape2::melt(jaccardMax)

  plot.final = ggplot(matFinal, aes(Var1, Var2, fill = value)) + geom_tile() + xlab("") + ylab("") +
    scale_fill_viridis_c(option = "C",breaks = seq(from=0, to=1,by=0.1),labels=seq(from=0, to=1,by=0.1),limits=c(0,1)) +
    guides(fill = guide_colourbar(title.position = 'right',barwidth = 1, barheight = 16,frame.colour = "black",
                                  ticks = TRUE,ticks.colour = "black",title = "Jaccard index",ticks.linewidth = 2,
                                  frame.linewidth = 2,draw.llim = TRUE)) +
    coord_cartesian(expand = FALSE) + theme_Matrix() + ggtitle("Top N barcodes by final frequency")

  ggsave(plot.final,filename = paste(output_directory,"jaccard_final.jpeg", sep=""),width = 9,height = 8, device ="jpeg")


  plot.max = ggplot(matMax, aes(Var1, Var2, fill = value)) + geom_tile() + xlab("") + ylab("") +
    scale_fill_viridis_c(option = "C",breaks = seq(from=0, to=1,by=0.1),labels=seq(from=0, to=1,by=0.1),limits=c(0,1)) +
    guides(fill = guide_colourbar(barwidth = 1, barheight = 16,frame.colour = "black",ticks = TRUE,ticks.colour = "black",
                                  title = "Jaccard index",ticks.linewidth = 2,frame.linewidth = 2,draw.llim = TRUE)) +
    coord_cartesian(expand = FALSE) + theme_Matrix() + ggtitle("Top N barcodes by max frequency")

  ggsave(plot.max,filename =  paste(output_directory,"jaccard_max.jpeg", sep=""),width = 9,height = 8, device ="jpeg")


}

#################
#' Applies jaccardIndex() over all elements of list l1
#' @export
#' @rdname jaccardIndex

pairwiseJaccard <- function(l1) {
  sapply(l1, function(i)
    sapply(l1,jaccardIndex,s2=i))
}

#################
#' Divides the # of intersecting elements between sets s1 and s2 by # of elements
#' in these two sets.
#' @export
#' @rdname jaccardIndex

jaccardIndex <- function(s1,s2) {
  i = length(intersect(s1,s2))
  u = length(union(s1,s2))
  return(i/u)
}
