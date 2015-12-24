\name{BiasEval}
\alias{BiasEval}
\title{
Evaluate Geographic Sampling Bias in Biological Collections
}
\description{
Evaluate geographic sampling bias in biological collections by comparison to randomly generated records from the same area.  Based on input data created with \code{\link{SamplingBias}} and a simulated dataset created with \code{\link{SimCoords}} and \code{\link{SamplingBias}} global and partial Bs are calculated.
}
\usage{
BiasEval(x, y, output = c("sp.bias.eval", "B", "data.frame"))
}
\arguments{
  \item{x}{
an object of the class \dQuote{\code{samp.bias}} as created with \code{\link{SamplingBias}}.
}
  \item{y}{
an object of the class \dQuote{\code{samp.bias}} from simulated data, created with \code{\link{SimCoords}} and \code{\link{SamplingBias}}.
}
  \item{output}{
character.  Defining the output value, see details. Default is to \dQuote{\code{sp.bias.eval}}.
}
}
\details{
If \code{output = 'data.frame'} the value is a \code{data.frame} of the mean test results for each column in \code{x} and partial B values, if \code{value = 'B'} the global B value, if \code{value = 'sp.bias.eval'} an object of the class \code{sp.bias.eval} combining both.  
B is an empirical value to summarize the geographic sampling bias in a dataset compared to randomly sampled coordinates in the same geographic extent. Partial B values are calculated for each test as: \eqn{B = \frac{mean(t_{dat})}{mean(t_{sim})} - 1}, with \eqn{t_{dat}} being the results for a given test on the collection data (e.g. the distance of each point to the next road) and \eqn{t_{sim}} being the result for the same test on randomly simulated data. The Global B is the mean value of all partial Bs.  In a standard analysis seven different tests are used (see \code{\link{SamplingBias}} for details).

}

\value{
Either a \code{data.frame}, a \code{numeric} or an object of the class \code{sp.bias.eval}, depending on the output argument.
}

\seealso{
\code{\link{SamplingBias}}, \code{\link{SimCoords}}, \code{\link{BiasPlot}}
}
\examples{
data(exampledata)

test.sim <- SimCoords(exampledata[, 2:3])

bias.dat <- SamplingBias(exampledata[, 2:3], troads = FALSE, tcoastlines = FALSE, 
                         verbose = FALSE)
bias.sim <- SamplingBias(test.sim, troads = FALSE, tcoastlines = FALSE, verbose = FALSE)

BiasEval(bias.dat, bias.sim)
}

\keyword{ datagen }
