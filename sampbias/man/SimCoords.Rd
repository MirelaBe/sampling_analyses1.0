\name{SimCoords}
\alias{SimCoords}

\title{
Simulate Geographic Occurrences Based on Real Data
}
\description{
Generates random geographic coordinates in the area as \code{x}. The simulations can be used for bias evaluation.
}
\usage{
SimCoords(x, polygon = NULL,
          type = c("random", "regular","stratified", "nonaligned", "hexagonal", 
                   "clustered", "Fibonacci"), 
          method = c("convexhull", "rectangle", "polygon", "intersect"),
	  terrestrial = T, extent = c("regional", "global"),
	  model = c("planar", "spheric"), n = NULL, refterrestrial = NULL)
}

\arguments{
\item{x}{
a \code{matrix} or \code{data.frame} with the coordinates of the collection dataset, for which random data is to be simulated. Format: longitude, latitude.
}
\item{polygon}{
a \code{SpatialPolygonsDataFrame} or \code{SpatialPolygons} object, to further limit the simulation. Default = NULL.
}
\item{type}{
character. The sampling method to be passed to \code{\link{spsample}}. Only if \code{model = 'planar'}. Default = \dQuote{random}.
}
\item{method}{
character. The method to choose the simulation area. See details for more information. Default = \dQuote{convexhull}.
}
\item{terrestrial}{
logical. If = TRUE coordinates are only simulated for land area. Default = TRUE.
}
\item{extent}{
character string. The extent of the data to be simulated. For performance purposes only. Set to regional for everything but global studies. Default = \dQuote{regional}.
}
\item{model}{
character string. The model for coordinate simulation. If \dQuote{planar}, \code{spsample{sp}} is used, if \dQuote{spheric}, \code{RandomCoordinates{geosphere}} is used. \dQuote{spheric} is recommended for large scale datasets to avoid bias towards the poles. Default = \dQuote{planar}.
}
\item{n}{
numeric. The number of data points to be generated. If NULL, the number equals the length of the reference dataset. For \code{model = 'spheric'} the number is an approximation. Default = NULL
}
\item{refterrestrial}{
a \code{SpatialPolygonsDataFrame} or \code{SpatialPolygons} object as alternate reference data for terrestrial surface. If NULL, \code{\link{landmass}} is used. Default = NULL.
}
}
\details{
Depending on the \code{method} argument the coordinates can be simulated in different extents. For \code{model = 'convexhull'} the coordinates are simulated within a convex hull around x, for \code{model = 'rectangle'} coordinates are simulated in a rectangle of the extent of x, if \code{model = 'polygon'} coordinates are simulated within a user-defined polygon provided by \code{poly} and if \code{model = 'intersect'} coordinates are simulated from the intersection of the convex hull of \code{x} and the polygons provided by \code{poly}.
}

\value{
a \code{data.frame} of the format longitude, latitude with simulated coordinates.
}

\seealso{
\code{\link{BiasEval}}, \code{\link{SimCoordsLarge}}
}
\examples{
data(exampledata)

test <- SimCoords(exampledata[, 2:3])

plot(test$longitude, test$latitude)
points(exampledata[, 2], exampledata[, 3], col = "red")

}

\keyword{ datagen }


