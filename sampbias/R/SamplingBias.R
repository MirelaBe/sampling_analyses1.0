SamplingBias <- function(x, tcities = T, tairports = T, troads = T, tcoastlines = T, 
                         twaterbodies = T, tprotectedarea = F, tsocioeconomic = T, tinterpoint = T, 
                         testmode = c("heuristicpoints", "points", "heuristiclines", "lines"), 
                         method = c("Haversine", "Cosine", "VincentyEllipse"), 
                         optim = c("standard","largescale"), 
                         limits = NULL, refinterpoint = NULL, 
                         refprotectedarea = NULL, refcities = NULL, 
                         refairports = NULL, refroads = NULL, refcoastlines = NULL, 
                         refwaterbodies = NULL, refsocioeconomic = NULL, verbose = T) {
  match.arg(testmode)
  match.arg(method)
  match.arg(optim)
  
  if (class(x) == "matrix") {
    if (dim(x)[2] > 2) {
      x <- x[, c(1, 2)]
      warning("more than 2 columns, colmm 1 assumed longitude, column 2 assumed latitude")
    }
  }
  if (class(x) == "data.frame") {
    if (dim(x)[2] > 2) {
      x <- x[, c(1, 2)]
      warning("more than 2 columns, colmm 1 assumed longitude, column 2 assumed latitude")
    }
  }
  if (dim(x)[1] > 20000) {
    warning("large dataset, disable the tinterpoint or try SamplingBiasLarge()")
  }
  
  if (tcities == T) {
    if (verbose == TRUE) {
      cat("calculating city distance\n")
    }
    if (length(refcities) == 0) {
      refcities <- sampbias::cities[, 3:4]
    }
    cit <- .PointDistance(x, refcities, limits = limits, method = method, 
                          optim = optim, verbose = F)
    if (verbose == TRUE) {
      cat("Done\n")
    }
  } else {
    cit <- rep(NA, dim(x)[1])
  }
  
  if (tairports == T) {
    if (verbose == TRUE) {
      cat("calculating airport distance\n")
    }
    if (length(refairports) == 0) {
      refairports <- sampbias::airports[, 3:4]
    }
    arp <- .PointDistance(x, refairports, limits = limits, method = method, 
                          optim = optim, verbose = F)
    if (verbose == TRUE) {
      cat("Done\n")
    }
  } else {
    arp <- rep(NA, dim(x)[1])
  }
  
  if (troads == T) {
    if (verbose == TRUE) {
      cat("calculating road distance\n")
    }
    if (is.null(refroads)) {
      refroads <- sampbias::roads
    }
    rod <- .LineDistance(x, refroads, test = testmode, method = method)
    if (verbose == TRUE) {
      cat("Done\n")
    }
  } else {
    rod <- rep(NA, dim(x)[1])
  }
  
  if (tcoastlines == T) {
    if (is.null(refcoastlines)) {
      refcoastlines <- sampbias::landmass
    }
    if (verbose == TRUE) {
      cat("calculating coastline distance\n")
    }
    coastlines <- as(SpatialPolygons(refcoastlines@polygons), "SpatialLines")
    cls <- .LineDistance(x, coastlines, test = testmode, method = method)
    if (verbose == TRUE) {
      cat("Done\n")
    }
  } else {
    cls <- rep(NA, dim(x)[1])
  }
  
  if (twaterbodies == T) {
    if (verbose == TRUE) {
      cat("calculating waterbodies distance\n")
    }
    if (is.null(refwaterbodies)) {
      refwaterbodies <- sampbias::waterbodies
    }
    wbs <- .LineDistance(x, refwaterbodies, test = testmode, method = method)
    if (verbose == TRUE) {
      cat("Done\n")
    }
  } else {
    wbs <- rep(NA, dim(x)[1])
  }
  
  if (tinterpoint == T) {
    if (verbose == TRUE) {
      cat("calculating interpoint distance\n")
    }
    ipd <- .InterpointDistance(x, referencedata = refinterpoint, method = method)
    if (verbose == TRUE) {
      cat("Done\n")
    }
  } else {
    ipd <- rep(NA, dim(x)[1])
  }
  
  if (tprotectedarea == T) {
    if (verbose == TRUE) {
      cat("finding samples in protected areas\n")
    }
    if (is.null(refprotectedarea)) {
      warning("'refprotectedarea' not found; tprotectarea set to FALSE")
      tprotectedarea <- FALSE
    }
    pta <- .ProtectedAreaBias(x, poly = refprotectedarea, limits = limits)
    if (verbose == TRUE) {
      cat("Done\n")
    }
  } else {
    pta <- rep(NA, dim(x)[1])
  }
  
  if (tsocioeconomic == T) {
    if (verbose == TRUE) {
      cat("calculating socio-economic indeces\n")
    }
    if (length(refsocioeconomic) == 0) {
      refsocioeconomic <- sampbias::socioeconomic
    }
    sec <- .SocioEconomic(x, polyg = NULL, soc.eco = refsocioeconomic)
    if (verbose == TRUE) {
      cat("Done\n")
    }
  } else {
    sec <- rep(NA, dim(x)[1])
  }
  
  out <- data.frame(Cities = cit, Airports = arp, Roads = rod, Coastlines = cls, 
                    Rivers = wbs, Interpoint.sd = ipd, Protectedarea = pta, 
                    socioeconomic = sec)
  #out <- Filter(function(x) !all(is.na(x)), out)
  class(out) <- c("samp.bias", class(out))
  return(out)
  
} 