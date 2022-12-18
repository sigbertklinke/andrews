#' @rdname andrews0
#' @title Andrews curves
#' @aliases andrews0
#' @description Andrews curves for visualization of multidimensional data.
#' For differences between `andrews` and `andrews2` see the `vignette("andrews").
#' For colouring the curves see the details.
#' @details
#' Andrews curves transform multidimensional data into curves. This package presents four types of curves
#'
#' If `df[,clr]` is numeric then `hsv(1,1,v)` with the normalized values (on `\[0, 1\]`) of `df[,clr]` is used.
#' Otherwise the number of unique values in `nuv <- unique(df[,clr])` is used in connection with `rainbow(nuv)`.
#' @param df data frame
#' @param type type of curve
#' * `1`: \eqn{f(t)=x_1/\sqrt{2}+x_2\sin(t)+x_3\cos(t)+x_4\sin(2t)+x_5\cos(2t)+...}
#' * `2`: \eqn{f(t)=x_1\sin(t)+x_2\cos(t)+x_3\sin(2t)+x_4\cos(2t)+...}
#' * `3`: \eqn{f(t)=0.5^{p/2}x_1+0.5^{(p-1)/2} x_2(\sin(t)+\cos(t))+0.5^{(p-2)/2} x_3(\sin(t)-\cos(t))+0.5^{(p-3)/2} x_4(\sin(2t)+\cos(2t))+0.5^{(p-6)/2}x_5(\sin(2t)-\cos(2t))+...)}
#' with $p$ the number of variables
#' * `4`: \eqn{f(t)=1/\sqrt{2}(x_1+x_2(\sin(t)+\cos(t))+x_3(\sin(t)-\cos(t))+x_4(\sin(2t)+\cos(2t))+x_5(\sin(2t)-\cos(2t))+...)}
#' @param clr number/name of column in the date frame for color of curves
#' @param step smoothness of curves
#' @param ymax maximum of `y` coordinate.
#' @param main main title for the plot
#' @param sub sub title for the plot
#' @references
#' * Andrews, D. F. (1972) Plots of High-Dimensional Data. Biometrics, vol. 28, no. 1, pp. 125-136.
#' * Khattree, R., Naik, D. N. (2002) Andrews Plots for Multivariate Data: Some New Suggestions and Applications. Journal of Statistical Planning and Inference, vol. 100, no. 2, pp. 411-425.
#' @keywords hplot
#' @author Jaroslav Myslivec <jaroslav.myslivec@upce.cz>
#' @return nothing
#' @importFrom grDevices hsv rainbow
#' @importFrom graphics axis box lines plot.new plot.window title
#' @export
#'
#' @examples
#' data(iris)
#' andrews0(iris,clr=5,ymax=3)
#' andrews0(iris,type=4,clr=5,ymax=2)
andrews0 <- function(df, type = 1, clr = NULL, step = 100, ymax = 10, main = NULL, sub = NULL) {
  # Setting plot
  if (step < 1)
    step <- 100
  n <- dim(df)[1]
  m <- dim(df)[2]
  plot.new()
  if ((type == 1) | (type == 2) | (type == 4)) {
    xmin <- (-pi)
    xmax <- pi
  } else
    if (type == 3) {
      xmin <- 0
      xmax <- 4 * pi
    }
  plot.window(c(xmin, xmax), c(-ymax, ymax))
  title(main = main, sub = sub)
  axis(1)
  axis(2)
  box()
  lines(c(xmin, xmax), c(0, 0))
  # Normalization
  for (i in 1:m)
    df[, i] <- normalize(df[, i])
  # Colors
  clx <- rep(1, n)
  if (!is.null(clr)) {
    if (!is.numeric(df[, clr])) {
      for (i in 1:n) {
        for (a in 1:nlevels(df[, clr]))
          if (levels(df[, clr])[a] == df[i, clr])
            clx[i] <- rainbow(nlevels(df[, clr]))[a]
      }
    } else
    {
      for (i in 1:n)
        clx[i] <- hsv(0, 1, df[i, clr])
    }
  }
  # Array
  dfm <- numarray(df)
  m <- dim(dfm)[2]
  # Curves
  coorx <- 0:step
  for (p in 0:step)
    coorx[p + 1] <- (xmin + (xmax - xmin) / step * p)
  coory <- 0:step
  for (i in 1:n) {
    for (p in 0:step) {
      coory[p + 1] <- 0
      tt <- (xmin + (xmax - xmin) / step * p)
      for (a in 1:m) {
        if (type == 1) {
          if (a == 1) {
            coory[p + 1] <- dfm[i, a] / (2 ^ 0.5)
          } else
          {
            cnst <- (a - 2) %/% 2 + 1
            if ((a - 2) %% 2 == 0)
              coory[p + 1] <-
                coory[p + 1] + dfm[i, a] * sin(cnst * tt)
            else
              coory[p + 1] <- coory[p + 1] + dfm[i, a] * cos(cnst * tt)
          }
        } else
          if (type == 2) {
            cnst <- (a - 1) %/% 2 + 1
            if ((a - 1) %% 2 == 0)
              coory[p + 1] <-
                coory[p + 1] + dfm[i, a] * sin(cnst * tt)
            else
              coory[p + 1] <- coory[p + 1] + dfm[i, a] * cos(cnst * tt)
          } else
            if (type == 3) {
              coory[p + 1] <- coory[p + 1] + dfm[i, a] * cos((a * tt) ^ 0.5)
            } else
              if (type == 4) {
                if (a == 1) {
                  coory[p + 1] <- dfm[i, a]
                } else
                {
                  cnst <- (a - 2) %/% 2 + 1
                  if ((a - 2) %% 2 == 0)
                    coory[p + 1] <- coory[p + 1] + dfm[i, a] * (sin(cnst * tt) + cos(cnst *
                                                                                       tt))
                  else
                    coory[p + 1] <-
                      coory[p + 1] + dfm[i, a] * (sin(cnst * tt) - cos(cnst * tt))
                }
                coory[p + 1] <- coory[p + 1] / (2 ^ 0.5)
              }
      }
    }
    lines(coorx, coory, col = clx[i])
  }
}
