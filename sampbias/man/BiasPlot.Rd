\name{BiasPlot}
\alias{BiasPlot}

\title{
Plotting Spatial Bias Data
}
\description{
A plot to compare geographic sampling bias in biological collection datasets created with \code{\link{SamplingBias}} with a randomly sampled coordinates from the same extent created with \code{\link{SimCoords}} and \code{\link{SamplingBias}}.  
}
\usage{
BiasPlot(x, y, plottype = c("density", "histogram"), break.num = 50, boxplots = F,
         cols = c(rgb(255, 69, 0, 150, maxColorValue = 255),
         rgb(32, 178, 170, 150, maxColorValue = 255)),border = c("grey50", "grey50"))
}
\arguments{
  \item{x}{
an object of the class \code{samp.bias} created with \code{\link{SamplingBias}}.
}
  \item{y}{
a simulated referencedataset created with \code{\link{SimCoords}}.
}
  \item{plottype}{
character.  Indicating the plotting mode, either histograms or kernel density plots. Default = "density".
}
  \item{break.num}{
numeric. The number of breaking points if \code{plottype = 'histogram'}. Default = 50.
}
  \item{boxplots}{
logical. If TRUE, socioeconomic indicators are shown as boxplots, otherwise as bar plots.  Default = FALSE
}
  \item{cols}{
character. The colours to be used for the plots.  Default is to light green and light red.
}
  \item{border}{
character. The colours to be used for the borders in the plots. The default is to grey.
}
}
\details{
Consider using \code{par(ask = T)} or \code{par(mfrow = c(4, 4))}.
}

\value{
A series of R plots
}

\seealso{
\code{\link{SamplingBias}}, \code{\link{SimCoords}}
}
\examples{
data(exampledata)

test.sim <- SimCoords(exampledata[, 2:3])

bias.dat <- SamplingBias(exampledata[, 2:3], troads = FALSE, tcoastlines = FALSE, 
                         verbose = FALSE)
bias.sim <- SamplingBias(test.sim, troads = FALSE, tcoastlines = FALSE, 
                         verbose = FALSE)

par(ask = TRUE)
BiasPlot(bias.dat, bias.sim)
par(ask = FALSE)
}
\keyword{ hplot }
