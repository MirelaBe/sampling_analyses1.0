\name{plot.samp.bias}
\alias{plot.samp.bias}

\title{
Plot Sampling Bias in Biological Collection Data
}
\description{
Plotting objects of the class \code{samp.bias} as created by \dQuote{\code{SamplingBias}}
}
\usage{
\method{plot}{samp.bias}(x, plottype = c("density", "histogram"),  
col = rgb(255, 69, 0, 150, maxColorValue = 255), border = "grey50", ...)
}

\arguments{
  \item{x}{
an object of the class \code{samp.bias} inheriting from \code{\link{SamplingBias}}.
}
  \item{plottype}{
character.  Type of plots to be produced.  Default is to \dQuote{density}.
}
  \item{col}{
character.  Colour of the plots.
}
  \item{border}{
character.  Colour of the plot borders.
}
  \item{\dots}{
additional arguments passed to \code{\link{plot}}.
}
}
\value{
a series of R plots.
}

\seealso{
\code{\link{SamplingBias}}
}
\examples{
data(exampledata)

simu <- SimCoords(exampledata)

bias.dat <- SamplingBias(exampledata[2:3], troads = FALSE, tcoastlines = FALSE, 
                         verbose = FALSE)
par(mar = c(6,4,4,4))
plot(bias.dat)

}

\keyword{ hplot }
