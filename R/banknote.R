#' Swiss banknotes data
#'
#' The data set contains six measurements made on 100 genuine and 100 counterfeit old-Swiss 1000-franc bank notes.
#' The data frame and the documentation is a copy of [mclust::banknote].
#'
#' @format
#' A data frame with 200 rows and 7 columns:
#' \describe{
#'   \item{Status}{the status of the banknote: `genuine` or `counterfeit`}
#'   \item{Length}{Length of bill (mm)}
#'   \item{Left}{Width of left edge (mm)}
#'   \item{Right}{Width of right edge (mm)}
#'   \item{Bottom}{Bottom margin width (mm)}
#'   \item{Top}{Top margin width (mm)}
#'   \item{Diagonal}{Length of diagonal (mm)}
#' }
#' @source Flury, B. and Riedwyl, H. (1988). Multivariate Statistics: A practical approach. London: Chapman & Hall, Tables 1.1 and 1.2, pp. 5-8.
"banknote"
