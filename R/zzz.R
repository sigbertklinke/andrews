pkgenv <- new.env()

.onLoad <- function(libname, pkgname) {
  deftype(1, function(n, t) {
    n <- as.integer(if (n<1) 1 else n)
    m <- matrix(1/sqrt(2), nrow=length(t) , ncol=n)
    if (n>1) {
      for (i in 2:n) {
        m[,i] <- if (i%%2) cos((i%/%2)*t) else sin((i%/%2)*t)
      }
    }
    m
  })
  deftype(2, function(n, t) {
    n <- as.integer(if (n<1) 1 else n)
    m <- matrix(NA, nrow=length(t) , ncol=n)
    for (i in 1:n) {
      j     <- i+1
      m[,i] <- if (j%%2) cos((j%/%2)*t) else sin((j%/%2)*t)
    }
    m
  })
  deftype(3, xlim=c(0, 4*pi), function(n, t) {
    n <- as.integer(if (n<1) 1 else n)
    m <- matrix(NA, nrow=length(t), ncol=n)
    for (i in 1:n) m[,i] <- cos(sqrt(i*t))
    m
  })
  deftype(4, function(n, t) {
    n  <- as.integer(if (n<1) 1 else n)
    s2 <- 0.5^(n/2)
    m  <- matrix(s2, nrow=length(t), ncol=n)
    if (n>1) {
      ff <- (1:n)%/%2
      for (i in 2:n) {
        s2  <- s2*sqrt(2)
        fft <- ff[i]*t
        m[,i] <- s2*if (i%%2) sin(fft)-cos(fft) else sin(fft)+cos(fft)
      }
    }
    m
  })
  deftype(5, xlim=c(0,4*pi), function(n, t) {
    n  <- as.integer(if (n<1) 1 else n)
    pr <- sqrt(generate_n_primes(n, TRUE))
    m  <- matrix(NA, nrow=length(t), ncol=n)
    for (i in 1:n) {
      m[,i] <- cos(pr[i]*t)
    }
    m
  })
  deftype(6, function(n, t) {
    n  <- as.integer(if (n<1) 1 else n)
    m  <- matrix(1, nrow=length(t), ncol=n)
    if (n>1) {
      ff <- (1:n)%/%2
      for (i in 2:n) {
        fft   <- ff[i]*t
        m[,i] <- if (i%%2) sin(fft)-cos(fft) else sin(fft)+cos(fft)
      }
    }
    m/sqrt(2)
  })
}

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

.onAttach <- function(libname, pkgname) {
  packageStartupMessage('See the package vignette with `vignette("andrews")`')
}
