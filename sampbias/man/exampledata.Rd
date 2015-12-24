\name{exampledata}
\alias{exampledata}
\docType{data}
\title{
Plant Species Distributions in West Africa
}
\description{
An example dataset of species geographic occurrences in West Africa.
}
\usage{data("exampledata")}
\format{
  A data frame with 500 observations on the following 4 variables.
  \describe{
    \item{\code{species}}{a factor; with species names}
    \item{\code{longitude}}{a numeric vector}
    \item{\code{latitude}}{a numeric vector}
    \item{\code{country}}{a factor with levels \code{BFA}}
  }
}

\source{
Schmidt, Zizka et al 2015
}

\examples{
data(exampledata)
str(exampledata)
}
\keyword{datasets}
