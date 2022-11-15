#' This function allows you to compute barcode diversity for all samples and
#' create dataframes with diversities per cohort
#' @export

#################

format_sample <- function(sample){
  casted = reshape2::dcast(sample, ID ~ Time, value.var = 'Reads')

  casted[is.na(casted)] <- 0
  return(casted)
}

#####################################################

# CALCULATE DIVERSITY (q = 0): number of lineages with nonzero frequency (species richness)
calculate_q_0 <- function(mat) {
  matbool = mat
  matbool[] = TRUE
  matbool[mat==0] <- FALSE
  # Sum of clusters with nonzero frequency for each timepoint
  q_0 = as.data.frame(colSums(matbool))
  colnames(q_0)="q_0"
  return(q_0)
}

# CALCULATE DIVERSITY (q = 1): Shannon diversity - all lineages weighted by their frequencies
calculate_q_1 <- function(mat) {
  matbool = mat
  matbool[] = TRUE
  matbool[mat==0] <- FALSE
  # sapply() applies the Shannon entropy to mat's elements without explicitly iterating through the whole dataframe
  q_1 = as.data.frame(exp(sapply(mat, function(x) entropy::entropy.empirical(x,unit = "log"))))
  colnames(q_1)="q_1"
  return(q_1)
}

# CALCULATE DIVERSITY (q = infinity): reciprocal of the maximum lineage frequency - (entropy) gives information about the contribution of the most abundant lineage on diversity
calculate_q_inf <- function(mat) {
  colMax <- function(data) sapply(data, max, na.rm = TRUE)
  q_inf = as.data.frame(1/colMax(mat))
  colnames(q_inf)="q_inf"
  return(q_inf)
}

# GENERATE DIVERSITY DATAFRAME FOR ONE SIMULATION
calculate_diversity <- function(data){

  generations = format_sample(data)
  m = as.matrix(generations[,-1])

  mat = as.data.frame(sweep(m,2,colSums(m,na.rm = TRUE),`/`))

  mat$ID = generations$X1


  q0 = calculate_q_0(mat)
  q1 = calculate_q_1(mat)
  qinf = calculate_q_inf(mat)
  qall = cbind(q0,q1,qinf)
  qall$Generations = as.double(row.names(qall))
  return(qall)
}

