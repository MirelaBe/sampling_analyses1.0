\name{BReplicate}
\alias{BReplicate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Replication of Global B Calculation
}
\description{
A wrapper to allow a calculation of B with multiple simulated datasets. Partial and global Bs are calculated for n simulated datasets.
}
\usage{
BReplicate(x, reps = 10, samp.outp.path, sim.outp.path, verb = T, 
          sim.type = c("random", "regular", "stratified", "nonaligned", 
                      "hexagonal", "clustered", "Fibonacci"), 
          sim.method = c("convexhull", "rectangle", "polygon", "intersect"), 
          sim.extent = c("regional", "global"), sim.model = c("planar", "spheric"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a \code{matrix} or a \code{data.frame} in the format longitude, latitude.
}
  \item{reps}{
numeric.  The number of replicates.
}
  \item{samp.outp.path}{
character. The complete path to a file on the hard drive for the samplingbias calculations (see details).
}
  \item{sim.outp.path}{
character. The complete path to a file on the hard drive for the simulated datasets(see details).
}
  \item{verb}{
logical.  If TRUE the progress of the testing is reported.
}
  \item{sim.type}{
character. The sampling method to be passed to \code{\link{spsample}}. Only if \code{model = 'planar'}. Default = \dQuote{random}.
}
  \item{sim.method}{
character. The method to choose the simulation area. See details for more information. Default = \dQuote{convexhull}.
}
  \item{sim.extent}{
character string. The extent of the data to be simulated. For performance purposes only. Set to regional for everything but global studies. Default = \dQuote{regional}.
}
  \item{sim.model}{
character string. The model for coordinate simulation. If \dQuote{planar}, \code{spsample{sp}} is used, if \dQuote{spheric}, \code{RandomCoordinates{geosphere}} is used. \dQuote{spheric} is recommended for large scale datasets to avoid bias towards the poles. Default = \dQuote{planar}.
}
  \item{\dots}{
further argumetns passed to \code{\link{SamplingBias}}.
}
}
\details{
If the \code{outp.path} argument is set, internal results are written to the indicated file instead of being saved internally (for large datasets).
}
\value{
An object of the class samp.bias.list, which is a list of \code{samp.bias} objects as  produced by \code{\link{BiasEval}}.
}

\seealso{
\code{\link{BiasEval}}, \code{\link{SimCoords}}, \code{\link{SamplingBias}}
}
\examples{
\dontrun{
data(exampledata)

test <- BReplicate(exampledata[, 2:3], samp.outp.path = "temp_samp.txt",
                   sim.outp.path = temp_sim.txt)
plot(test[[1]])
}
}
\keyword{ datgen }
