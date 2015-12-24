\name{SimCoordsLarge}
\alias{SimCoordsLarge}

\title{
Wrapper around SimCoords for Large Datasets
}
\description{
Generates random geographic coordinates in the area as \code{x}. The simulations can be used for bias evaluation.
}
\usage{
SimCoordsLarge(x, outp.path, verb = TRUE, ...)
}

\arguments{
  \item{x}{
a \code{matrix} or \code{data.frame} with the coordinates of the collection dataset, for which random data is to be simulated. Format: longitude, latitude.
}
  \item{outp.path}{
character. The complete path to a file on the hard drive. See details.
}
  \item{verb}{
logical.  If TRUE the progress of the testing is reported.
}
  \item{\dots}{
Further arguments passed to \code{\link{SimCoords}}. The arguments \code{poly} and \code{n} are disabled.
}
}
\details{
Simulates chunks of 10,000 records, writes to the file specified in the \code{outp.path} argument and reassembles the data to circumvent memory limitations.
}
\value{
a \code{data.frame} of the format longitude, latitude with simulated coordinates.
}

\seealso{
\code{\link{SimCoords}}, \code{\link{BiasEval}}
}
\examples{
data(exampledata_large)
\dontrun{
test <- SimCoordLarge(exampledata_large[, 2:4], outp.path = "sim_coords_int.txt")
}
}

\keyword{ datagen }
