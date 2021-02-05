#' computes m two-sample t-tests (equal variance) between two groups
#'
#' This function computes m (m > 1) two-sample t-tests (equal variance) between two groups
#' using simple R functions for vector/matrix calculations, which is very fast
#'
#' @param x a m*n matrix of observations
#' @param f factors with 2 levels, each level has length n/2, can be generated using gl(2,n/2)
#'
#' @return a vector of t test statistics with length m
#'
#' @examples
#'
#' x <- matrix(rnorm(40*50),nrow=40,ncol=50)
#' f <- gl(2,25)
#' t_stat <- getT(x,f)
#'
#' @import matrixStats
#'
#' @export



getT <- function(x, f){
  if(length(unique(f)) != 2){
    stop("Error: more than 2 factors detected!")
  }
  f1=as.numeric(unique(f)[1])
  f2=as.numeric(unique(f)[2])
  if(sum(f==f1) != sum(f==f2)){
    stop("Error: Different numbers of 2 factors detected!")
  }
  n = dim(x)[2]
  little.n = n/2
  sp = (rowVars(x[,which(f==f1)]) + rowVars(x[,which(f==f2)]))*(little.n-1)/(n-2)
  return((rowMeans(x[,which(f==f1)])-rowMeans(x[,which(f==f2)]))/sqrt(sp/little.n*2))
}
