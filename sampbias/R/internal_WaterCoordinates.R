.WaterCoordinates <- function(x, poly = NULL) {
  pts <- SpatialPoints(x)
  
  if (length(poly) == 0) {
    testpolys <- sampbias::landmass
  } else {
    testpolys <- poly
  }
  land <- over(x = pts, y = testpolys)[, 1]
  out <- !is.na(land)
  
  return(out)
} 