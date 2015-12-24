.CountryCheck <- function(x, countries, poly = NULL) {
  pts <- SpatialPoints(x)
  
  if (length(poly) == 0) {
    testpolys <- sampbias::countryborders
  } else {
    testpolys <- poly
  }
  
  country <- over(x = pts, y = testpolys)[, "ISO3"]
  out <- as.character(country) == as.character(countries)
  
  return(out)
}