\name{plot.sp.bias.eval}
\alias{plot.sp.bias.eval}

\title{
Plot the Evaluation of Spatial Bias in Collections
}
\description{
Plotting objects of the class \code{sp.bias.eval} as build by \code{\link{BiasEval}}.
}
\usage{
\method{plot}{sp.bias.eval}(x, ...)
}

\arguments{
  \item{x}{
an object of the class \code{sp.bias.eval} as build by \code{\link{BiasEval}}.
}
  \item{\dots}{
additional arguments passed to \code{\link{plot}}.
}
}

\value{
an R plot.
}

\seealso{
\code{\link{BiasEval}}, \code{\link{SamplingBias}}
}
\examples{
data(exampledata)

test.sim <- SimCoords(exampledata[, 2:3])

bias.dat <- SamplingBias(exampledata[, 2:3], troads = FALSE, tcoastlines = FALSE, 
                         verbose = FALSE)
bias.sim <- SamplingBias(test.sim, troads = FALSE,tcoastlines = FALSE, 
                         verbose = FALSE)

bias <- BiasEval(bias.dat, bias.sim)
par(mar = c(6,4,4,4))
plot(bias)
}

\keyword{ hplot }
