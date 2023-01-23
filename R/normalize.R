#' @rdname normalize
#' @title Normalization
#' @aliases normalize
#' @description Normalization of a variable:
#' * `type==1`: `ar` normalized into \eqn{[0,1]},
#' * `type==2`: `ar` is standardized,
#' * otherwise no normalization is done.
#'
#' @param ar numeric variable.
#' @param type integer: type of normalization (default: `1`)
#'
#' @details Normalization of variable: `ar<-(ar-min(ar))/(max(ar)-min(ar))`
#'
#' @return Returns normalized variable.
#'
#' @importFrom stats sd
#' @export
#' @author Jaroslav Myslivec <jaroslav.myslivec@upce.cz>, Sigbert Klinke <sigbert@hu-berlin.de>
#'
#' @examples
#' normalize(iris[,1])
normalize <- function(ar, type=1) {
  # Normalization into <0,1>
  # ar<-(ar-mean(ar))/sd(ar)
  # SK - 10 Dec 2022: added na.rm=TRUE
  # SK - 09 Jan 2022: added type
  if (is.numeric(ar)) {
    center <- 0
    scale  <- 1
    if (type==1) {
      center <- min(ar, na.rm=TRUE)
      scale  <- max(ar, na.rm=TRUE)-center
    }
    if (type==2) {
      center <- mean(ar, na.rm=TRUE)
      scale  <- sd(ar, na.rm=TRUE)
    }
    ar <- (ar-center)/scale
  }
  ar
}
