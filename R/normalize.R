#' @rdname normalize
#' @title Normalization
#' @aliases normalize
#' @description Normalization of variable.
#'
#' @param ar numeric variable.
#'
#' @details Normalization of variable: `ar<-(ar-min(ar))/(max(ar)-min(ar))`
#'
#' @return Returns normalized variable.

#' @export
#' @author Jaroslav Myslivec <jaroslav.myslivec@upce.cz>, Sigbert Klinke <sigbert@hu-berlin.de>
#'
#' @examples
#' normalize(iris[,1])
normalize <- function(ar) {
    # Normalization into <0,1>
    # ar<-(ar-mean(ar))/sd(ar)
    # SK - 10 Dec 2022: added na.rm=TRUE
    if (is.numeric(ar)) {
      ar <- (ar - min(ar, na.rm=TRUE)) / (max(ar, na.rm=TRUE) - min(ar, na.rm=TRUE))
    }
    ar
  }

