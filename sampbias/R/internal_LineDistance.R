.LineDistance <- function(x, referencedata, limits = NULL, 
                          method = c("Haversine","Cosine", "VincentyEllipse"), 
                          test = c("heuristicpoints", "points", "heuristiclines", "lines"), 
                          heurad = 1, verbose = F) {
  
  if(test[1] != "heuristicpoints"){
  if (is.null(limits)) {
    testlimits <- extent(c(min(x[, 1]) - 5, max(x[, 1]) + 5, 
                           min(x[,2]) - 5, max(x[, 2]) + 5))
  } else {
    testlimits <- extent(limits)
  }
  
  testdat <- crop(referencedata, testlimits)
  if (is.null(testdat)) {
    testdat <- crop(referencedata, testlimits * 2)
  }
  if (is.null(testdat)) {
    testdat <- crop(referencedata, testlimits * 5)
  }
  if (is.null(testdat)) {
    testdat <- referencedata
  }
  }
  if (test[1] == "heuristiclines") {
    out <- apply(x, 1, function(y) {
      lim2 <- extent(c(min(y[1]) - heurad, max(y[1]) + heurad, min(y[2]) - 
                         heurad, max(y[2]) + heurad))
      testdat2 <- crop(referencedata, lim2)
      if (method[1] == "Cosine") {
        res <- suppressWarnings(try(dist2Line(y, testdat2, distfun = distCosine), 
                                    silent = T))
      }
      if (method[1] == "Haversine") {
        res <- suppressWarnings(try(dist2Line(y, testdat2, distfun = distHaversine), 
                                    silent = T))
      }
      if (method[1] == "VincentyEllipse") {
        res <- suppressWarnings(try(dist2Line(y, testdat2, distfun = distVincentyEllipsoid), 
                                    silent = T))
      }
      if (class(res) == "try-error") {
        res <- NA
      }
      return(res[1])
    })
    
    mult <- c(2, 4, 6, 8, 10, 20, 200)
    
    for (i in mult) {
      out.nas <- apply(x[is.na(out), ], 1, function(y) {
        lim2 <- extent(c(min(y[1]) - heurad * i, max(y[1]) + heurad * 
                           i, min(y[2]) - heurad * i, max(y[2]) + heurad * i))
        testdat2 <- crop(referencedata, lim2)
        if (method[1] == "Cosine") {
          res <- suppressWarnings(try(dist2Line(y, testdat2, distfun = distCosine), 
                                      silent = T))
        }
        if (method[1] == "Haversine") {
          res <- suppressWarnings(try(dist2Line(y, testdat2, distfun = distHaversine), 
                                      silent = T))
        }
        if (method[1] == "VincentyEllipse") {
          res <- suppressWarnings(try(dist2Line(y, testdat2, distfun = distVincentyEllipsoid), 
                                      silent = T))
        }
        return(res[1])
      })
      out[as.numeric(names(out.nas))] <- out.nas
    }
    out <- round(out/1000, 2)
  }
  
  if (test[1] == "lines") {
    if (method[1] == "Cosine") {
      out <- dist2Line(x[1:10, ], testdat, distfun = distCosine)[, 1]
    }
    if (method[1] == "Haversine") {
      out <- dist2Line(x, testdat, distfun = distHaversine)[, 1]
    }
    if (method[1] == "VincentyEllipse") {
      out <- dist2Line(x, testdat, distfun = distVincentyEllipsoid)[, 1]
    }
    out <- round(out/1000, 2)
  }
  
  if (test[1] == "points") {
    testdat2 <- do.call("rbind", lapply(coordinates(testdat), 
                                        function(x) do.call("rbind",x)))
    if (method[1] == "Cosine") {
      out <- distm(x, testdat2, fun = distCosine)
    }
    if (method[1] == "Haversine") {
      out <- distm(x, testdat2, fun = distHaversine)
    }
    if (method[1] == "VincentyEllipse") {
      out <- distm(x, testdat2, fun = distVincentyEllipsoid)
    }
    out <- apply(out, 1, "min")
    out <- round(out/1000, 2)
  }
  
  if (test[1] == "heuristicpoints") {
    testdat <- referencedata
    refpoints <- do.call("rbind", lapply(coordinates(testdat),
                                         function(x) do.call("rbind",x)))
    out <- .PointDistance(x, refpoints, limits = limits, method = method, 
                          optim = "largescale", verbose = verbose)
  }
  
  return(out)
} 