\name{SamplingBiasLarge}
\alias{SamplingBiasLarge}

\title{
Wrapper around SamplingBias for large datasets
}
\description{
A wrapper for \code{\link{SamplingBias}} for large datasets with more than 20,000 records.
}
\usage{
SamplingBiasLarge(x, outp.path = NULL, cellsize = 2, verb = T, ...)
}

\arguments{
  \item{x}{
a \code{matrix} or a \code{data.frame}, with the occurrence coordinates in the format: longitude, latitude.
}
  \item{outp.path}{
character. The complete path to a file on the hard drive (see details). Default = NULL.
}
  \item{cellsize}{
The cell size for the geographic raster in degrees. See details.Default = 2
}
  \item{verb}{
logical.  If TRUE the progress of the testing is reported.
}
  \item{\dots}{
Arguments to be passed on to \code{\link{SamplingBias}}.
}
}
\details{
This function cuts the dataset in rectangular geographic subsets with side length equals cellsize * 2, performs the bias tests and then reassembles the dataset to circumvent memory limitations. The inter-point standard deviation test is performed with 10,000 records selected at random from the input dataset.  If the \code{outp.path} argument is set, internal results are written to the indicated file instead of being saved internally (for very large datasets).
}
\value{
an object of the class \code{samp.bias} with the calculated results in a separate column for each test. The socioeconomic test has multiple columns.
}

\seealso{
\code{\link{SamplingBias}}
}
\examples{
\dontrun{
data(exampledata_large)
test <- SamplingBiasLarge(exampledata_large[, 2:3])
head(test)

#for very large datasets using outp.path
test2 <- SamplingBiasLarge(exampledata_large[, 2:3], outp.path = "int_bias_results.txt")
head(test)
}


}
\keyword{ datgen }

