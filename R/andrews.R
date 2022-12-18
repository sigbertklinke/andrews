#' @title Andrews curves
#' @aliases andrews
#' @description Andrews curves for visualization of multidimensional data. For colouring the curves see the details.
#' For differences between `andrews` and `andrews0` see the `vignette("andrews").
#' With the same parameters called both functions should create the same plot.
#' `type==5` is a modification of `type==3` and `type==6` is a modification of `type==4`
#'
#' @details
#' If `clr` has length one then it is used as column number or column name
#' for coloring the curves:
#' * If `df[,clr]` is numeric then `palcol` must be function which returns
#' colors for values in the range `\[0, 1\]` using normalized variable.
#' The default is function `function(v) { hsv(0,1,v) }`.
#' * Otherwise `df[,clr]` is converted to a factor and `palcol` must be a function
#' which returns for each level a color. The parameter for `palcol` is the numbe of
#' levels and the default is [grDevices::rainbow()].
#' If the length of `clr` is the number of rows of `df` then `clr` is interpreted as
#' colors.
#' @param df data frame
#' @param type type of curve
#' * `1`: \eqn{f(t)=x_1/\sqrt{2}+x_2\sin(t)+x_3\cos(t)+x_4\sin(2t)+x_5\cos(2t)+...}
#' * `2`: \eqn{f(t)=x_1\sin(t)+x_2\cos(t)+x_3\sin(2t)+x_4\cos(2t)+...}
#' * `3`: \eqn{f(t)=x_1\cos(t)+x_2\cos(\sqrt{2t})+x_3\cos(\sqrt{3t})+...}
#' * `4`: \eqn{f(t)=0.5^{p/2}x_1+0.5^{(p-1)/2} x_2(\sin(t)+\cos(t))+0.5^{(p-2)/2} x_3(\sin(t)-\cos(t))+0.5^{(p-3)/2} x_4(\sin(2t)+\cos(2t))+0.5^{(p-4)/2}x_5(\sin(2t)-\cos(2t))+...)}
#' with $p$ the number of variables
#' * `5`: \eqn{f(t)=x_1\cos(\sqrt{p_0} t)+x_2\cos(\sqrt{p_1}t)+x_3\cos(\sqrt{p_2}t)+...} with $p_0=1$ and $p_i$ the i-th prime number
#' * `6`: \eqn{f(t)=1/\sqrt{2}(x_1+x_2(\sin(t)+\cos(t))+x_3(\sin(t)-\cos(t))+x_4(\sin(2t)+\cos(2t))+x_5(\sin(2t)-\cos(2t))+...)}
#'
#' @param clr number/name of column in the date frame for color of curves
#' @param step smoothness of curves
#' @param ymax maximum of `y` coordinate.
#' @param palcol a function which generates a set of colors, see details
#' @param alpha semi-transparent color (\eqn{0 < alpha < 1}) which are supported only on some devices
#' @param lwd line width, a positive number, defaulting to 1.
#' @param lty line type,  can either be specified as an integer (0=blank, 1=solid (default),
#'  2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash) or as one of the character strings
#'  "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash", where "blank"
#'  uses ‘invisible lines’ (i.e., does not draw them).
#' @param ... further parameters given to [graphics::plot.default()] except `x`, `y`, and `type`.
#' @details Andrews curves transform multidimensional data into curves. This package presents four types of curves.
#' @references
#' * Andrews, D. F. (1972) Plots of High-Dimensional Data. Biometrics, vol. 28, no. 1, pp. 125-136.
#' * Khattree, R., Naik, D. N. (2002) Andrews Plots for Multivariate Data: Some New Suggestions and Applications. Journal of Statistical Planning and Inference, vol. 100, no. 2, pp. 411-425.
#' @keywords hplot
#' @author Jaroslav Myslivec <jaroslav.myslivec@upce.cz>, Sigbert Klinke <sigbert@hu-berlin.de>
#' @return nothing
#' @importFrom grDevices hsv rainbow col2rgb rgb
#' @importFrom graphics plot.default
#' @importFrom primes generate_n_primes
#' @export
#'
#' @examples
#' data(iris)
#' op <- par(mfrow=c(1,2))
#' andrews0(iris,clr=5,ymax=3)
#' andrews(iris,clr=5,ymax=3)
#' par(op)
#' andrews(iris,type=4,clr=5,ymax=NA)
andrews <- function(df, type = 1, clr = NULL, step = 100, ymax = 10, alpha=NULL, palcol=NULL,
                    lwd=1, lty="solid", ...) {
  typematrix <- function(t, n, type) {
    m <- NULL
    if (type==1) {
      m <- matrix(1/sqrt(2), nrow=length(t), ncol=1)
      if (n>1) {
        ff <- (1:n)%/%2
        for (i in 2:n) {
          m <- cbind(m, if (i%%2) cos(ff[i]*t) else sin(ff[i]*t))
        }
      }
    }
    if (type==2) {
      m  <- matrix(NA, nrow=length(t), ncol=0)
      ff <- 1+((0:(n-1))%/%2)
      for (i in 1:n) {
        m <- cbind(m, if (i%%2) sin(ff[i]*t) else cos(ff[i]*t))
      }
    }
    if (type==3) {
      m <- cos(sqrt(t))
      if (n>1) {
        for (i in 2:n) {
          m <- cbind(m, cos(sqrt(i*t)))
        }
      }
    }
    if (type==4) {
      s2 <- 0.5^(n/2)
      m  <- matrix(s2, nrow=length(t), ncol=1)
      if (n>1) {
        ff <- (1:n)%/%2
        for (i in 2:n) {
          s2  <- s2*sqrt(2)
          fft <- ff[i]*t
          m <- cbind(m, s2*if (i%%2) sin(fft)-cos(fft) else sin(fft)+cos(fft))
        }
      }
    }
    if (type==5) {
      pr <- sqrt(c(1, generate_n_primes(n-1)))
      m  <- matrix(NA, nrow=length(t), ncol=0)
      for (i in 1:n) {
        m <- cbind(m, cos(pr[i]*t))
      }
    }
    if (type==6) {
      m  <- matrix(1, nrow=length(t), ncol=1)
      if (n>1) {
        ff <- (1:n)%/%2
        for (i in 2:n) {
          fft <- ff[i]*t
          m <- cbind(m, if (i%%2) sin(fft)-cos(fft) else sin(fft)+cos(fft))
        }
      }
      m <- m/sqrt(2)
    }
    if (is.null(m)) stop (sprintf("Unknown 'type': %s", as.character(type)))
    structure(m, dimnames=NULL)
  }
  # https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
  areColors <- function(x) {
    sapply(x, function(X) {
      tryCatch(is.matrix(col2rgb(X)),
               error = function(e) FALSE)
    })
  }
  #
  xrange <- function (type, xlim) {
    if (is.null(xlim)) {
      xlim <- c(-pi, pi)
      if (type %in% c(3,5)) {
        xlim <- c(0, 4*pi)
      }
    }
    xlim
  }
  #
  yrange <- function(mt, ymax, ylim, type) {
    if (is.null(ylim)) {
      if (is.na(ymax)) {
        for (i in 1:ndf) {
          ymax <- range(ymax, mt%*%dfm[i,], na.rm=TRUE)
        }
      } else {
        if (ymax<=0) {
          ymax <- ncol(mt)
          if (type %in% c(4,6)) ymax <- sqrt(2)*ymax
        }
      }
      ylim <- max(abs(ymax))*c(-1,1)
    }
    ylim
  }
  #
  color_alpha <- function(df, clr, alpha, palcol) {
    # Colors
    ndf <- nrow(df)
    clx <- NULL
    if (length(clr)>1) {
      clr <- as.character(clr)
      stopifnot(all(areColors(clr)))
      clx <- col2rgb(clr)
      clx <- rgb(clx[1,], clx[2,], clx[3,], maxColorValue = 255)
    }
    if (length(clr)==1) {
      if (is.numeric(df[,clr])) {
        if (is.null(palcol)) palcol <- function(v) { hsv(0, 1, v) }
        clx <- palcol(df[,clr])
      } else {
        dfclr <- df[,clr]
        if (!is.factor(dfclr)) dfclr <- as.factor(dfclr)
        if (is.null(palcol)) palcol <- rainbow
        col <- palcol(nlevels(dfclr))
        clx <- col[dfclr]
      }
    }
    if (is.null(clx)) clx <- rep("#000000", ndf)
    # add alpha if necessary
    if (!is.null(alpha)) {
      alpha[alpha<0] <- 0
      alpha[alpha>1] <- 1
      if (length(alpha)==1) alpha <- rep(alpha[1], ndf)
      clx <- col2rgb(clx)
      clx <- rgb(clx[1,], clx[2,], clx[3,], 256*alpha, maxColorValue = 255)
    }
    clx
  }
  stopifnot(is.data.frame(df))
  ndf <- nrow(df)
  stopifnot(length(alpha) %in% c(0, 1, ndf))
  stopifnot(length(clr) %in% c(0, 1, ndf))
  stopifnot(length(lwd) %in% c(1, ndf))
  stopifnot(length(lty) %in% c(1, ndf))
  ## Data
  # Normalization
  for (i in 1:ncol(df)) df[, i] <- normalize(df[, i])
  # Array
  dfm <- numarray(df)
  ## Plot
  # Set up plot
  if (step < 1) step <- 100
  #
  args      <- list(...)
  args$xlim <- xrange(type, args$xlim)
  coorx     <- seq(args$xlim[1], args$xlim[2], by=diff(args$xlim)/step)
  #
  mt        <- typematrix(coorx, ncol(dfm), type)
  args$ylim <- yrange(mt, ymax, args$ylim, type)
  if (is.null(args$xlab)) args$xlab <- ""
  if (is.null(args$ylab)) args$ylab <- ""
  args$x <- mean(args$xlim)
  args$y <- mean(args$ylim)
  args$type <- "n"
  do.call(plot.default, args)
  lines(args$xlim, c(0, 0))
  # Curves
  if (length(lwd)==1) lwd <- rep(lwd, ndf)
  if (length(lty)==1) lty <- rep(lty, ndf)
  clx <- color_alpha(df, clr, alpha, palcol)
  for (i in 1:ndf) {
    coory <- mt%*%dfm[i,]
    lines(coorx, coory, col = clx[i], lwd=lwd[i], lty=lty[i])
  }
  invisible(mt)
}