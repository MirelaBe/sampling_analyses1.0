.InterpointDistance <- function(x, referencedata = NULL, method = "Haversine") {
  if (is.null(referencedata)) {
    if (dim(x)[1] > 10000) {
      testdat <- x[sample(x = 1:dim(x)[1], size = 10000), ]
      warnings("more than 10000 points, testing confined to 10000 random points of the dataset")
    } else {
      testdat <- x
    }
  } else {
    testdat <- referencedata
  }
  if (method[1] == "Cosine") {
    out <- distm(x, testdat, fun = distCosine)
  }
  if (method[1] == "Haversine") {
    out <- distm(x, testdat, fun = distHaversine)
  }
  if (method[1] == "VincentyEllipse") {
    out <- distm(x, testdat, fun = distVincentyEllipsoid)
  }
  out[out == 0] <- NA
  out <- apply(out, 1, function(x) min(x, na.rm = T))
  out <- round(out/1000, 1)
  
  return(out)
}