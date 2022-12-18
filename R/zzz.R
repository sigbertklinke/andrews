#' Comparison
#'
#' Creates and displays a temporary PDF file with different diagrams comparing `andrews` and `andrews0` plots.
#'
#' @return nothing
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics par
#' @importFrom utils browseURL
#' @export
#'
#' @examples
#' if (interactive()) zzz()
zzz <- function() {
  if (interactive()) {
    expr <- expression({
      andrews(iris)
      andrews0(iris)

      andrews(iris, type=2)
      andrews0(iris, type=2)

      andrews(iris, type=3)
      andrews0(iris, type=3)

      andrews(iris, type=4, ymax=3)
      andrews0(iris, type=4, ymax=3)

      andrews(iris, type=5, ymax=3)
      andrews0(iris, type=3, ymax=3)

      andrews(iris, type=6, ymax=3)
      andrews0(iris, type=4, ymax=3)

      andrews(iris, clr=5, ymax=3)
      andrews0(iris, clr=5, ymax=3)

      andrews(iris, clr=1, ymax=3)
      andrews0(iris, clr=1, ymax=3)
    })

    out <- tempfile(fileext=".pdf")
    pdf(out, paper="a4r")
    par(mfrow=c(1,2))
    for (i in 2:length(expr[[1]])) {
      eval(expr[[1]][[i]])
      title(main=deparse(expr[[1]][[i]]))
    }
    dev.off()
    browseURL(out)
  }
}
