.UrbanCoordinates <- function(x, poly = NULL, limits = NULL) {
  pts <- SpatialPoints(x)
  
  if (is.null(limits)) {
    testlimits <- bbox(pts)
    testlimits[, 1] <- testlimits[, 1] - 5
    testlimits[, 2] <- testlimits[, 2] + 5
    testlimits <- extent(testlimits)
  } else {
    testlimits <- extent(limits)
  }
  
  if (is.null(poly)) {
    testpolys <- sampbias::urbanareas
    testpolys <- crop(testpolys, testlimits)
  } else {
    testpolys <- poly
  }
  
  urban <- over(x = pts, y = testpolys)[, 1]
  out <- !is.na(urban)
  
  return(out)
} 