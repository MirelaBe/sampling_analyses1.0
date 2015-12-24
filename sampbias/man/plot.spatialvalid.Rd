\name{plot.spatialvalid}
\alias{plot.spatialvalid}

\title{
Map the Spatial Validity of Coordinates
}
\description{
Plot a map of flagged coordinates from an object of the class \code{'spatialvalid'} created with \code{\link{CleanCoordinates}} or \code{\link{CleanCoordinatesLarge}}.
}
\usage{
\method{plot}{spatialvalid}(x, bgmap = NULL, clean = T, details = T, ...)
}
\arguments{
  \item{x}{
an object of the class \dQuote{\code{spatialvalid}}.
}
  \item{bgmap}{
any object with plot method.  The background map. Default = \code{\link{landmass}}.
}
  \item{clean}{
logical.  If TRUE, clean coordinates are shown. Default = TRUE.
}
  \item{details}{
logical. If TRUE, the specific test flagging a coordinate is shown by \code{pch}. Default = TRUE.
}
  \item{\dots}{
further arguments to be past to \code{\link{plot}}
}
}

\value{
An R plot.
}

\seealso{
\code{\link{CleanCoordinates}}, \code{\link{CleanCoordinatesLarge}}, \code{\link{landmass}}
}
\examples{
data(exampledata)

test <- CleanCoordinates(exampledata[, 2:4], species = exampledata[, 1],
                         verbose = FALSE)
plot(test)
plot(test, clean = FALSE, details = FALSE )
}
\keyword{ hplot }

