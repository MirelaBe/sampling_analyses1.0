\name{plot.outliers.list}
\alias{plot.outliers.list}

\title{Visualizing Outlier Coordinates}

\description{
The plotting method for the class outliers.list. Visualizes coordinates identified as ouliers by \code{\link{CleanCoordinates}} and \code{\link{FindOutliers}}.
}
\usage{
\method{plot}{outliers.list}(x, type = c("perspecies", "alloutliers"), 
                             pch = 4, col = "red", cex = 0.5, bg = NULL, ...)
}
\arguments{
  \item{x}{
an object of the class \code{outliers.list}.
}
  \item{type}{
character string. defining the type of plot. See details. Default = \dQuote{perspecies}.
}
  \item{pch}{
Either an integer specifying a symbol or a single character to be used as the default in plotting the outlier points.  See \code{\link{points}} for possible values.
}
  \item{col}{
character string. A specification for the default plotting color
}
  \item{cex}{
integer.  A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default.
}
  \item{bg}{
an object to be plotted as background map.
}
  \item{\dots}{
further arguments to be passed to \code{\link{plot}} for the background map.
}
}
\details{
If \code{type =  "perspecies")}, a map for each species that contains outliers, with both outlier and non-outlier coordinates. If \code{type =  "alloutliers")}, a map with all outlier coordinates.
}
\value{
a set of plots.
}

\seealso{
\code{\link{CleanCoordinates}}, \code{\link{FindOutliers}}
}
\examples{
data(exampledata)

dat <- FindOutliers(exampledata[, 2:3], species = exampledata[, 1], 
                    outp = "outliers.list")
plot(dat)

}

\keyword{ hplot }

