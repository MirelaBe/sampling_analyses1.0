\name{FindOutliers}
\alias{FindOutliers}

\title{
Outlier Coordinates per Species
}
\description{
Identifying outlier coordinates per species in a multispecies dataset.  Coordinates are tested by the distance to all other coordinates of the species. 
}
\usage{
FindOutliers(x, species, outp = c("data.frame", "summary", "outliers.list"), 
             outliers.mtp = NULL, outliers.td = 100)
}

\arguments{
  \item{x}{
an object of the class \code{spatialvalid} or a \code{data.frame} with columns \dQuote{longitude}, \dQuote{latitude}
}
  \item{species}{
a vector of the same length as x, with a species names for every record
}
  \item{outp}{
character string.  The output format. See value section for details.
}
  \item{outliers.mtp}{
numeric. The multiplier for the interquartile range of the outlier test.  If NULL \code{outliers.td} is used.  Default = 25.
}
  \item{outliers.td}{
numeric.  The minimum distance of a record to all other records of a species to be indentified as outlier, in km. If NULL \code{outliers.mtp} is used. Default = NULL
}
}
\details{
Depending on the \code{outliers.mtp} and \code{outliers.td} arguments  either flags records that are a minimum distance away from all other records of this species (\code{outliers.td}) or records that are outside a multiple of the interquartile range of minimum distances to the next neighbour of this species (\code{outliers.mtp}). Geographic distances are calculated with \code{distm{geosphere}}.  Default is to \dQuote{Haversine}.
}
\value{
Depending on the outp argument.  If outp = \code{'data.frame'}, a \code{data.frame} with the coordinates and species ID of all outliers If outp = \code{'summary'} a \code{data.frame} with the numbers of outlier coordinates per species and if outp = \code{'outliers.list'} an object of the class \code{outliers.list} with a list of all coordinates of all species that have flagged outlier coordinates.
}

\seealso{
\code{\link{CleanCoordinates}}, \code{\link{plot.outliers.list}}
}
\examples{
data(exampledata)

dat <- FindOutliers(exampledata[, 2:3], species = exampledata[, 1], 
                    outliers.mtp = NULL, outliers.td = 100,
                    outp = "outliers.list")
plot(dat)
}
\keyword{ datagen }
