\name{CleanCoordinatesLarge}
\alias{CleanCoordinatesLarge}

\title{
Wrapper around CleanCoordinates for large datasets
}
\description{
A wrapper for the \code{\link{CleanCoordinates}} function for large datasets with >20,000 records.
}
\usage{
CleanCoordinatesLarge(x, species = NULL, outp.path = NULL, verb = T, rpt = F, 
                      dupl = T, ...)
}

\arguments{
  \item{x}{
a \code{matrix} or \code{data.frame}, with the occurrence coordinates in the format: longitude, latitude.
}
  \item{species}{
character. A vector of the same length as x, with the species identity for each record.  If set to NULL, the outliers test is skipped.  Default = NULL 
}
  \item{outp.path}{
character. The complete path to a file on the hard drive. See details. Default is to NULL.
}
  \item{verb}{
logical.  If TRUE the progress of the testing is reported.
}
  \item{rpt}{
logical or character.  If TRUE a report file is written to the working directory, summarizing the cleaning results. If a character, the path to which the file should be written.  Default = F.
}
  \item{dupl}{
logical.  If TRUE, tests for duplicate records. THis checks for identical coordinates or if a species vector is provided for identical coordinates within a species. All but the first records are flagged as duplicates.  Default = FALSE.
}
  \item{\dots}{
Arguments to be passed on to \code{\link{CleanCoordinates}}.
}
}
\details{
This cuts the input data into chunks of 10,000 records, performs the tests and reassembles the data to circumvent memory limitations.  If the \code{outp.path} argument is set, internal results are written to the indicated file instead of being saved internally (for very large datasets).
}
\value{
Depending on the output argument. If \code{output = 'spatialvalid'} an object of the class \code{spatialvalid} where each column represent the results of one test (TRUE = non-problematic coordinate, FALSE = potentially problematic coordinates).  Otherwise a logical vector summarizing the results of all tests (set to FALSE if at least one test failed).
}

\seealso{
\code{\link{CleanCoordinates}}
}
\examples{

\dontrun{
data(exampledata_large)
test <- CleanCoordinatesLarge(exampledata_large[, 2:4], species = exampledata_large[, 1], 
                              verbose = FALSE)
plot(test)

#for very large datasets using outp.path
test2 <- CleanCoordinatesLarge(exampledata_large[, 2:4], species = exampledata_large[, 1], 
                              verbose = FALSE, outp.path = "int_results.txt")
plot(test2)
}
}
\keyword{ datagen }

