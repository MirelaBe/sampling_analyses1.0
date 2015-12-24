.ProtectedAreaBias <- function(x, poly = NULL, limits = NULL) {
  pts <- SpatialPoints(x)
  
  if (length(limits) == 0) {
    testlimits <- bbox(pts)
    testlimits[, 1] <- testlimits[, 1] - 5
    testlimits[, 2] <- testlimits[, 2] + 5
    testlimits <- extent(testlimits)
  } else {
    testlimits <- extent(limits)
  }
  
  if (is.null(poly)) {
    stop("'poly' not found; provide reference polygon")
  } else {
    testpolys <- poly
  }
  
  prot <- over(x = pts, y = testpolys)[, 1]
  out <- !is.na(prot)
  return(out)
} 