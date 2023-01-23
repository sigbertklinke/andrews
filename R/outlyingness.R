#' outlyingness
#'
#' Computes the Stahel-Donoho outlyingness. If `type` is any of the available types by [andrews()] then
#' the projection vectors are generated along the andrews curves. Otherwise `step` random directions
#' will be used. Note that the projection vectors are always normalized to length one.
#'
#' @param x data frame
#' @param type type of curve, see [andrews()]
#' @param step step	smoothness of curves
#' @param xlim the x limits (x1, x2)
#' @param normalize type of normalization, see [normalize()]
#'
#' @references
#' * Stahel, W. (1981), Robuste Schätzungen: infinitesimale Optimalität und Schätzungen von Kovarianzmatrizen, PhD thesis, ETH Z¨urich.
#' * Donoho, D. (1982), Breakdown properties of multivariate location estimators, Ph.D. Qualifying paper, Dept. Statistics, Harvard University, Boston.
#' @return the Stahel-Donoho outlyingness
#' @importFrom stats mad median rnorm
#' @export
#'
#' @examples
#' # use projection vectors from the Andrews curve
#' sdo <- outlyingness(iris)
#' col <- gray(1-sdo/max(sdo))
#' andrews(iris, clr=col, ymax=NA)
#' # use 1000 random projection vectors
#' sdo <- outlyingness(iris, type=0, step=1000)
#' col <- gray(1-sdo/max(sdo))
#' andrews(iris, clr=col, ymax=NA)
#' # use 1000 random projection vectors with adjusted outlyingness
#' library("robustbase")
#' x   <- numarray(iris)
#' x   <- scale(x, center=apply(x, 2, min), scale=apply(x, 2, max)-apply(x, 2, min))
#' sdo <- adjOutlyingness(x, ndir=1000, only.outlyingness=TRUE)
#' col <- gray(1-sdo/max(sdo))
#' andrews(as.data.frame(x), clr=col, ymax=NA)
outlyingness <- function(x, type=1, step=100, xlim=NULL, normalize=1) {
  x <- numarray(x)
  atype <- deftype(type)
  if (is.null(atype)) {
    mt <- matrix(rnorm(ncol(x)*step), ncol=ncol(x))
  } else {
    xlim  <- if(is.null(xlim)) atype$range else xlim
    coorx <- seq(xlim[1], xlim[2], by=diff(xlim)/step)
    mt    <- atype$fun(ncol(x), coorx)
  }
  mt <- mt/sqrt(rowSums(mt^2))
  absmax <- rep(0, nrow(x))
  for (i in 1:ncol(x)) x[,i] <- normalize(x[,i], type=normalize)
  for (i in 1:step) {
    xproj  <- x%*%matrix(mt[i,], ncol=1)
    xproj  <- abs(xproj-median(xproj))/mad(xproj)
    absmax <- ifelse(absmax<xproj, xproj, absmax)
  }
  absmax
}
