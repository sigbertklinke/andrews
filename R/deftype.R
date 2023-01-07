#' deftype
#'
#' Defines a function which can be used as basis for Andrews curves \eqn{f_t(t) = \sum_{j=1}^p x_{ij} f_i(t)}.
#'
#' @param index index/name of the function
#' @param FUN function of the form \code{function(n, t) {...}}
#' @param xlim default range for displaying curves (default: `c(-pi,pi)`)
#'
#' @return either a list of all functions or a single function
#' @export
#'
#' @examples
#' # define a new andrews curve, just with sine curves
#' deftype("sine", function(n, t) {
#'           n <- as.integer(if (n<1) 1 else n)
#'           m <- matrix(NA, nrow=length(t), ncol=n)
#'           for (i in 1:n) m[,i] <- sin(i*t)
#'           m
#'          })
#' andrews(iris, "sine")
#' # query
#' deftype()
#' deftype("sine")
deftype <- function(index=NULL, FUN=NULL, xlim=c(-pi,pi)) {
  if (is.null(index)) return(as.list(pkgenv))
  name <- as.character(index)
  if (is.null(FUN)) return(pkgenv[[name]])
  if (!is.null(pkgenv[[name]])) stop(sprintf("Function type '%s' exists already", name))
  pkgenv[[name]] <- list(fun=FUN, range=range(xlim, na.rm = TRUE))
  invisible(pkgenv[[name]])
}
