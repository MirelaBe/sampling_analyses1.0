\name{socioeconomic}
\alias{socioeconomic}
\docType{data}
\title{
Global Socio-economic Information on Country-level
}
\description{
Country-level socioeconomic indices as reference data for bias testing.
}
\usage{data("socioeconomic")}
\format{
  A data frame with 187 observations on the following 11 variables.
}
\details{
\describe{
\item{Iso3}{Iso-3 letter code for country names.}
\item{Publications}{The number of publication in organismic biology according to [1].}
\item{hindex}{The h-index of scientific publications in organismic biology from [1].}
\item{Peace}{The global peace index according to [2].}
\item{GDP}{The Gross Domestic Product in 2014 according to  [3].}
\item{GERD}{The mean Gross Domestic Expenditure on Research and Experimental Development between 2000 - 2014 according to [4].}
\item{Herbaria}{The number of registered herbaria according to [5].}
\item{area}{The country area in square kilometres.}
\item{Publications.area}{The number of publication in organismic biology according to [1] per 1000 square kilometres country area.}
\item{Herbaria.area}{The number of registered herbaria according to [5] per 1000 square kilometers.}
}
}
\source{
\url{http://www.scimagojr.com/index.php}

\url{http://www.visionofhumanity.org/}

\url{http://databank.worldbank.org/data/download/GDP_PPP.pdf}

\url{http://data.uis.unesco.org/?queryid=74}

\url{http://sciweb.nybg.org/science2/IndexHerbariorum.asp}

\url{https://www.cia.gov/library/publications/the-world-factbook/fields/2147.html}
}

\examples{
data(socioeconomic)
str(socioeconomic)
}
\keyword{datasets}
