\name{SamplingBias}
\alias{SamplingBias}

\title{
Empirical Sampling Bias in Biological Collections
}
\description{
Calculating empirical covariates of sampling bias based on seven tests based on common biases in biological collections.  Individual tests can be switched on and off with the logical arguments. Subsequently the bias can be compared to a random sample coordinates with \code{\link{BiasEval}} and \code{\link{BiasPlot}}. 
}
\usage{
SamplingBias(x, tcities = T, tairports = T, troads = T, tcoastlines = T,
                twaterbodies = T, tprotectedarea = F, tsocioeconomic = T, 
                tinterpoint = T, 
                testmode = c("heuristicpoints", "points", "heuristiclines", "lines"), 
             method = c("Haversine", "Cosine", "VincentyEllipse"), 
             optim = c("standard", "largescale"), limits = NULL, 
             refinterpoint = NULL, refprotectedarea = NULL, refcities = NULL,
             refairports = NULL, refroads = NULL, refcoastlines = NULL,
             refwaterbodies = NULL, refsocioeconomic = NULL, verbose = T)
}

\arguments{
\item{x}{
a \code{matrix} or a \code{data.frame} in the format longitude, latitude.
}
\item{tcities}{
logical. If TRUE, calculates the minimum distance in km to a major city (\dQuote{as a crow flies}). Default = TRUE.
}
\item{tairports}{
logical. If TRUE, calculates the minimum distance in km to a major airport (\dQuote{as a crow flies}). Default = TRUE.
}
\item{troads}{
logical.  If TRUE, estimates the approximate minimum distance in km to a major road (\dQuote{as a crow flies}). Default = TRUE.
}
\item{tcoastlines}{
logical.  If TRUE, estimates the approximate minimum distance in km to the coastline (\dQuote{as a crow flies}). Default = TRUE.
}
\item{twaterbodies}{
logical.  If TRUE, estimates the approximate minimum distance to a major waterbody (\dQuote{as a crow flies}). Default = TRUE.
}
\item{tprotectedarea}{
logical.  If TRUE, tests if collected from inside a protected area. Note that no default reference dataset is distributed with the package. Data can be downloaded from \url{http://www.protectedplanet.net/}. Default = FALSE.
}
\item{tsocioeconomic}{
logical.  If TRUE, socioeconomic indices are calculated based on country scale data.  Includes country identity, GDP, GERD, Global Peace index, number of scientific publication and standard Hindex in organismic biology and number of herbaria. Default = TRUE.
}
\item{tinterpoint}{
logical. If TRUE, calculates the distance in km to the nearest neighbour in the dataset (\dQuote{as a crow flies}).  For datasets with more than 10,000 points the distance to the nearest neighbour of a random subsample is calculated (n = 10,000).  This is used by \code{BiasEval} to evaluate the clustering of the points.  Default = TRUE.
}
\item{testmode}{
character.  The mode to calculate the distance to linear structures. See details. Default = \dQuote{heuristicpoints}.
}
\item{method}{
character. The method for calculating geographic distances passed to \code{distm{geosphere}}. Default = \dQuote{Haversine}.
}
\item{optim}{
character.  Optimization for the point distance test.  If \dQuote{largescale}, a moving window is used to find minimum distances. This is only relevant for memory and computational optimization and largely unnecessary in the latest version due to \code{\link{SamplingBiasLarge}}.
}
\item{limits}{
numeric.  A vector of the format (xmin, xmax, ymin, ymax) giving strict geographic borders for the tests.  This can reduce computation time. If NULL, borders are calculated from the data. Default = NULL.
}
\item{refinterpoint}{
a reference for calculating the inter-point distance. If NULL the input data is used as reference (i.e. finding the nearest neighbour in the dataset). This can also be used to test the distance to other spacial structures. Default = NULL.
}
\item{refprotectedarea}{
a \code{SpatialPolygonsDataFrame} of protected areas as reference for \code{tprotectedarea}. No default data is provided. Data can be downloaded from \url{http://www.protectedplanet.net/}.
}
\item{refcities}{
a \code{data.frame} with alternative reference data for the cities distance. If set to NULL, the \code{\link{cities}} dataset is used. Alternatives must be identical in structure. Default = NULL.
}
\item{refairports}{
a \code{data.frame} with alternative reference data for the airports distance. If set to NULL, the \code{\link{airports}} dataset is used. Alternatives must be identical in structure. Default = NULL.
}
\item{refroads}{
a \code{SpatialPolygonsDataFrame} with alternative reference data for the roads distance. If set to NULL, the \code{\link{roads}} dataset is used. Default = NULL.
}
\item{refcoastlines}{
a \code{SpatialPolygonsDataFrame} with alternative reference data for the roads distance. If set to NULL, the \code{\link{landmass}} dataset is used. Default = NULL.
}
\item{refwaterbodies}{
a \code{SpatialPolygonsDataFrame} with alternative reference data for the coastline distance. If set to NULL, the \code{\link{landmass}} dataset is used. Default = NULL.
}
\item{refsocioeconomic}{
a \code{data.frame} with alternative reference data for the socioeconomic indicators test. If set to NULL, the \code{\link{socioeconomic}} dataset is used. Alternatives must be identical in structure. Default = NULL.
}
\item{verbose}{
logical. If TRUE the progress of the testing is reported.  Default = TRUE.
}
}
\details{
The \code{testmode} argument controls how the distance to linear features (i.e. coastlines or roads) is estimated.
\describe{
\item{heuristicpoints}{as points, but geographically limiting the search to the neighbourhood of each record, reducing computation time.}
\item{points}{uses \code{distm{geosphere}} to calculate the distance to the next vertex of the linear feature. The adequacy depends on the number of vertices (the level of detail) of the reference data and is thus rather an estimate then the real distance.}
\item{line}{uses \code{dist2Line{geosphere}}, to calculate the closest distance to the line, this is the most exact but, computationally most intensive option}
\item{heuristiclines}{as lines, but geographically limiting the search to the neighbourhood of each record, reducing computation time.}
}
}
\value{
an object of the class \code{samp.bias} with the calculated results in a separate column for each test. The socioeconomic test has multiple columns.
}

\note{
See package vignette for details and examples. 
}

\seealso{
\code{\link{SamplingBiasLarge}}, \code{\link{BiasEval}}, \code{\link{BiasPlot}}
}
\examples{
data(exampledata)

bias.dat <- SamplingBias(exampledata[,2:3], troads = FALSE, tcoastlines = FALSE, 
                         verbose = FALSE)

test.sim <- SimCoords(exampledata[,2:3])
bias.sim <- SamplingBias(test.sim, troads = FALSE, tcoastlines = FALSE, 
                         verbose = FALSE)

BiasPlot(bias.dat, bias.sim)

eva <- BiasEval(bias.dat, bias.sim)
plot(eva)

}

\keyword{ datagen }


