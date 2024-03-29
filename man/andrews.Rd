% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/andrews.R
\name{andrews}
\alias{andrews}
\title{Andrews curves}
\usage{
andrews(
  df,
  type = 1,
  clr = NULL,
  step = 100,
  ymax = 10,
  alpha = NULL,
  palcol = NULL,
  lwd = 1,
  lty = "solid",
  ...
)
}
\arguments{
\item{df}{data frame or an R object that can be converted into a data frame with \code{as.data.frame}}

\item{type}{type of curve
\itemize{
\item \code{1}: \eqn{f(t)=x_1/\sqrt{2}+x_2\sin(t)+x_3\cos(t)+x_4\sin(2t)+x_5\cos(2t)+...}
\item \code{2}: \eqn{f(t)=x_1\sin(t)+x_2\cos(t)+x_3\sin(2t)+x_4\cos(2t)+...}
\item \code{3}: \eqn{f(t)=x_1\cos(t)+x_2\cos(\sqrt{2t})+x_3\cos(\sqrt{3t})+...}
\item \code{4}: \eqn{f(t)=0.5^{p/2}x_1+0.5^{(p-1)/2} x_2(\sin(t)+\cos(t))+0.5^{(p-2)/2} x_3(\sin(t)-\cos(t))+0.5^{(p-3)/2} x_4(\sin(2t)+\cos(2t))+0.5^{(p-4)/2}x_5(\sin(2t)-\cos(2t))+...)}
with \eqn{p} the number of variables
\item \code{5}: \eqn{f(t)=x_1\cos(\sqrt{p_0} t)+x_2\cos(\sqrt{p_1}t)+x_3\cos(\sqrt{p_2}t)+...} with \eqn{p_0=1} and \eqn{p_i} the i-th prime number
\item \code{6}: \eqn{f(t)=1/\sqrt{2}(x_1+x_2(\sin(t)+\cos(t))+x_3(\sin(t)-\cos(t))+x_4(\sin(2t)+\cos(2t))+x_5(\sin(2t)-\cos(2t))+...)}
}}

\item{clr}{number/name of column in the data frame for color of curves}

\item{step}{smoothness of curves}

\item{ymax}{maximum of \code{y} coordinate}

\item{alpha}{semi-transparent color (\eqn{0 < alpha < 1}) which are supported only on some devices}

\item{palcol}{a function which generates a set of colors, see details}

\item{lwd}{line width, a positive number, defaulting to 1.}

\item{lty}{line type,  can either be specified as an integer (0=blank, 1=solid (default),
2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash) or as one of the character strings
"blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash", where "blank"
uses ‘invisible lines’ (i.e., does not draw them).}

\item{...}{further named parameters given to \code{\link[graphics:plot.default]{graphics::plot.default()}} except \code{x}, \code{y}, and \code{type}.}
}
\value{
nothing
}
\description{
Andrews curves for visualization of multidimensional data.
For colouring the curves see the details.
For differences between \code{andrews} and \code{andrews0} see the \code{vignette("andrews")}.
With the same parameters called both functions should create the same plot.
\code{type==5} is a modification of \code{type==3} and \code{type==6} is a modification of \code{type==4}.
}
\details{
If \code{clr} has length one then it is used as column number or column name
for coloring the curves:
\itemize{
\item If \code{df[,clr]} is numeric then \code{palcol} must be function which returns
colors for values in the range \verb{\[0, 1\]} using normalized variable.
The default is function \code{function(v) { hsv(0,1,v) }}.
\item Otherwise \code{df[,clr]} is converted to a factor and \code{palcol} must be a function
which returns for each level a color. The parameter for \code{palcol} is the numbe of
levels and the default is \code{\link[grDevices:palettes]{grDevices::rainbow()}}.
If the length of \code{clr} is the number of rows of \code{df} then \code{clr} is interpreted as
colors.
}

Andrews curves transform multidimensional data into curves. This package presents four types of curves.
}
\examples{
data(iris)
op <- par(mfrow=c(1,2))
andrews0(iris,clr=5,ymax=3)
andrews(iris,clr=5,ymax=3)
par(op)
andrews(iris,type=4,clr=5,ymax=NA)
}
\references{
\itemize{
\item Andrews, D. F. (1972) Plots of High-Dimensional Data. Biometrics, vol. 28, no. 1, pp. 125-136.
\item Khattree, R., Naik, D. N. (2002) Andrews Plots for Multivariate Data: Some New Suggestions and Applications. Journal of Statistical Planning and Inference, vol. 100, no. 2, pp. 411-425.
}
}
\author{
Sigbert Klinke \href{mailto:sigbert@hu-berlin.de}{sigbert@hu-berlin.de}, Jaroslav Myslivec \href{mailto:jaroslav.myslivec@upce.cz}{jaroslav.myslivec@upce.cz}
}
\keyword{hplot}
