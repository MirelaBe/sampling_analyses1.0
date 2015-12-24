.CapitalCoordinates <- function(x, testdist = 0.1, limits = NULL, referencedat = NULL) {
  dat <- x
  if (is.null(referencedat)) {
    testdat <- sampbias::capitals
  } else {
    testdat <- referencedat
  }
  
  if (is.null(limits)) {
    testlimits <- c(min(dat$longitude) - 1, max(dat$longitude) - 1, 
                    min(dat$latitude) + 1, max(dat$latitude) + 1)
  } else {
    testlimits <- limits
  }
  
  # subset of testdatset according to limits
  testdat <- testdat[testdat$longitude > testlimits[1] & testdat$longitude < 
                       testlimits[2], ]
  testdat <- testdat[testdat$latitude > testlimits[3] & testdat$latitude < 
                       testlimits[4], ]
  
  # testing
  out <- apply(dat, 1, function(x) all(!(x["longitude"] > 
                                           (testdat$longitude - testdist / 2) & x["longitude"] < 
                                           (testdat$longitude + testdist / 2) & x["latitude"] > 
                                           (testdat$latitude - testdist / 2) & x["latitude"] < 
                                           (testdat$latitude + testdist / 2))))
  return(out)
}