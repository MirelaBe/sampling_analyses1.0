\name{sampbias-package}
\alias{sampbias-package}
\alias{sampbias}
\docType{package}
\title{
\packageTitle{sampbias}
}
\description{
\packageDescription{sampbias}
}
\details{

\packageIndices{sampbias}

}
\author{Alexander Zizka}

\references{
Maldonado C, Molina CI, Zizka A, Persson C, Taylor CM, Alban J, Chilquillo E, Ronsted N, Antonelli A (2015) Estimating species diversity in the era of big data: to what extent can we trust public databases? \emph{Global Ecol Biogeogr} \bold{24}, 8: 973--984.

Yang W, Ma K, Kreft H (2014) Environmental and socio-economic factors shaping the geography of floristic collections in China. \emph{Global Ecology and Biogeography} \bold{23}, 1284--1292.

Meyer C, Kreft H, Guralnick R, Jetz W (2015) Global Priorities for an effective information basis of biodiveristy distributions. \emph{PeerJPreprints}.

Yang W, Ma, K, Kreft H (2013) Geographical sampling in a large distributional database and its effects on species richness-environment models. \emph{J Biogeogr} \bold{40}, 1415--1426.

}

\keyword{ package }

\examples{
data(exampledata)

clean <- CleanCoordinates(exampledata[, 2:4], species = exampledata[, 1], verbose = FALSE)

cleandata <- exampledata[clean$summary, ]

bias.dat <- SamplingBias(cleandata[, 2:3], troads = FALSE, tcoastlines = FALSE, verbose = FALSE)
test.sim <- SimCoords(cleandata[, 2:3])
bias.sim <- SamplingBias(test.sim, troads = FALSE, tcoastlines = FALSE, verbose = FALSE)

BiasPlot(bias.dat, bias.sim)

eva <- BiasEval(bias.dat, bias.sim)
par(mar = c(6,4,4,4))
plot(eva)

}
