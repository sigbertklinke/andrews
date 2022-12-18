#' @rdname numarray
#' @title Numeric array
#' @description Extracts numeric array from data frame.
#'
#' @param df data frame.
#' @details Extracts numeric array from data frame.
#'
#' @return Returns numeric array.
#' @author Jaroslav Myslivec <jaroslav.myslivec@upce.cz>, Sigbert Klinke <sigbert@hu-berlin.de>
#' @export
#'
#' @examples
#' numarray(iris)
numarray <- function(df){
  stopifnot(is.data.frame(df))
  data.matrix(df[,sapply(df, is.numeric)])
# Jaroslav Myslivec version:
# Numeric attributes only
# dfm<-0
# m<-dim(df)[2]
# for (a in 1:m){
#  if (is.numeric(df[,a])){
#   if (length(dfm)>1) dfm<-cbind(dfm,df[,a]) else dfm<-df[,a]
#  }
# }
# dfm
}

