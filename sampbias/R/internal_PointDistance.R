.PointDistance <- function(x, referencedata, limits = NULL, 
                           method = c("Haversine","Cosine", "VincentyEllipse"), 
                           optim = c("standard", "largescale"), 
                           verbose = F, cellsize = 2) {
  
  if (dim(x)[1] < 100) {
    optim <- "standard"
  }
  
  if (optim[1] == "standard") {
    if (is.null(limits)) {
      testlimits <- c(max(c(min(x[, 1]) - 10, -180)), 
                      min(c(max(x[, 1]) + 10, 180)), 
                      max(c(min(x[, 2]) - 10, -90)), 
                      min(c(max(x[, 2]) + 10, 90)))
    } else {
      testlimits <- limits
    }
    
    # subset of testdatset according to limits
    testdat <- referencedata[referencedata[, 1] > testlimits[1] & 
                               referencedata[, 1] < testlimits[2], ]
    if (length(as.matrix(testdat)) == 2) {
      testdat <- testdat[testdat[2] > testlimits[3] & testdat[2] < 
                           testlimits[4]]
    } else {
      testdat <- testdat[testdat[, 2] > testlimits[3] & 
                           testdat[, 2] < testlimits[4], ]
    }
    
    if(class(testdat) == "numeric"){
      testdat <- matrix(testdat, ncol = 2)
    }
    
    multpl <- 0
    while(dim(testdat)[1] == 0){
      multpl <- multpl + 10
      testlimits <- c(max(c(min(x[, 1]) - multpl, -180)),
                      min(c(max(x[, 1]) + multpl, 180)), 
                      max(c(min(x[, 2]) - multpl, -90)), 
                      min(c(max(x[, 2]) + multpl, 90)))
      testdat <- referencedata[referencedata[, 1] > testlimits[1] & 
                                 referencedata[, 1] < testlimits[2], ]
      if (length(as.matrix(testdat)) == 2) {
        testdat <- testdat[testdat[2] > testlimits[3] & testdat[2] < 
                             testlimits[4]]
      } else {
        testdat <- testdat[testdat[, 2] > testlimits[3] & 
                             testdat[, 2] < testlimits[4], ]
      }
      if(class(testdat) == "numeric"){
        testdat <- matrix(testdat, ncol = 2)
      }
    }
# 
#     if (dim(testdat)[1] == 0) {
#       testlimits <- c(max(c(min(x[, 1]) - 20, -180)),
#                       min(c(max(x[, 1]) + 20, 180)), 
#                       max(c(min(x[, 2]) - 20, -90)), 
#                       min(c(max(x[, 2]) + 20, 90)))
#       testdat <- referencedata[referencedata[, 1] > testlimits[1] & 
#                                  referencedata[, 1] < testlimits[2], ]
#       if (length(as.matrix(testdat)) == 2) {
#         testdat <- testdat[testdat[2] > testlimits[3] & testdat[2] < 
#                              testlimits[4]]
#       } else {
#         testdat <- testdat[testdat[, 2] > testlimits[3] & 
#                              testdat[, 2] < testlimits[4], ]
#       }
#       if(class(testdat) == "numeric"){
#         testdat <- matrix(testdat, ncol = 2)
#       }
#     }
#     if (dim(testdat)[1] == 0) {
#       testlimits <- c(max(c(min(x[, 1]) - 40, -180)), 
#                       min(c(max(x[, 1]) + 40, 180)), 
#                       max(c(min(x[, 2]) - 40, -90)), 
#                       min(c(max(x[, 2]) + 40, 90)))
#       testdat <- referencedata[referencedata[, 1] > testlimits[1] & 
#                                  referencedata[, 1] < testlimits[2], ]
#       if (length(as.matrix(testdat)) == 2) {
#         testdat <- testdat[testdat[2] > testlimits[3] & testdat[2] < 
#                              testlimits[4]]
#       } else {
#         testdat <- testdat[testdat[, 2] > testlimits[3] & 
#                              testdat[, 2] < testlimits[4], ]
#       }
#       if(class(testdat) == "numeric"){
#         testdat <- matrix(testdat, ncol = 2)
#       }
#     }
#     if (dim(testdat)[1] == 0) {
#       testlimits <- c(max(c(min(x[, 1]) - 60, -180)), 
#                       min(c(max(x[, 1]) + 60, 180)), 
#                       max(c(min(x[, 2]) - 60, -90)), 
#                       min(c(max(x[, 2]) + 60, 90)))
#       testdat <- referencedata[referencedata[, 1] > testlimits[1] & 
#                                  referencedata[, 1] < testlimits[2], ]
#       if (length(as.matrix(testdat)) == 2) {
#         testdat <- testdat[testdat[2] > testlimits[3] & testdat[2] < 
#                              testlimits[4]]
#       } else {
#         testdat <- testdat[testdat[, 2] > testlimits[3] & 
#                              testdat[, 2] < testlimits[4], ]
#       }
#       if(class(testdat) == "numeric"){
#         testdat <- matrix(testdat, ncol = 2)
#       }
#     }
#     if (dim(testdat)[1] == 0) {
#       testlimits <- c(max(c(min(x[, 1]) - 100, -180)), 
#                       min(c(max(x[, 1]) + 100, 180)), 
#                       max(c(min(x[, 2]) - 100, -90)), 
#                       min(c(max(x[, 2]) + 100, 90)))
#       testdat <- referencedata[referencedata[, 1] > testlimits[1] & 
#                                  referencedata[, 1] < testlimits[2], ]
#       if (length(as.matrix(testdat)) == 2) {
#         testdat <- testdat[testdat[2] > testlimits[3] & testdat[2] < 
#                              testlimits[4]]
#       } else {
#         testdat <- testdat[testdat[, 2] > testlimits[3] & 
#                              testdat[, 2] < testlimits[4], ]
#       }
#       if(class(testdat) == "numeric"){
#         testdat <- matrix(testdat, ncol = 2)
#       }
#     }
#     
    if (method[1] == "Cosine") {
      out <- distm(x, testdat, fun = distCosine)
    }
    if (method[1] == "Haversine") {
      out <- distm(x, testdat, fun = distHaversine)
    }
    if (method[1] == "VincentyEllipse") {
      out <- distm(x, testdat, fun = distVincentyEllipsoid)
    }
    out <- apply(out, 1, "min")
  }
  
  if (optim[1] == "largescale") {
    x <- data.frame(x)
    if (min(x[, 1]) * max(x[, 1]) < 0) {
      lonspan <- abs(min(x[, 1])) + abs(max(x[, 1]))
    } else {
      lonspan <- abs(abs(max(x[, 1])) - abs(min(x[, 1])))
    }
    if (min(x[, 2]) * max(x[, 2]) < 0) {
      latspan <- abs(min(x[, 2])) + abs(max(x[, 2]))
    } else {
      latspan <- abs(abs(max(x[, 2])) - abs(min(x[, 2])))
    }
    
    ras <- coordinates(raster(nrows = ceiling(lonspan/(cellsize * 2)), 
                              ncols = ceiling(latspan/(cellsize * 2)), xmn = min(x[, 1]), 
                              xmx = max(x[, 1]), ymn = min(x[, 2]), ymx = max(x[, 2])))
    ras <- GridTopology(cellcentre.offset = c(min(x[, 1]), min(x[, 2])), 
                        cellsize = c((cellsize * 2), (cellsize * 2)), 
                        cells.dim = c(ceiling(lonspan/(cellsize *  2)), 
                                      ceiling(latspan/(cellsize * 2))))
    ras <- coordinates(ras)
    
    cr <- list()
    for (i in 1:dim(ras)[1]) {
      cr[[i]] <- extent(c(ras[i, 1] - (cellsize - 1e-08), ras[i,1] + 
                            cellsize, ras[i, 2] - (cellsize - 1e-08), ras[i, 2] + 
                            cellsize))
    }
    
    dat2 <- SpatialPoints(x)
    rownames(dat2@coords) <- rownames(x)
    dat <- lapply(cr, function(x) crop(dat2, x))
    dat <- dat[!unlist(lapply(dat, is.null))]
    dat <- lapply(dat, coordinates)
    
    # add entries missed by geographic splitting
    mis <- as.numeric(unlist(lapply(dat, rownames)))
    mis <- x[which(!rownames(x) %in% mis), ]
    if (dim(mis)[1] > 0) {
      dat[[length(dat) + 1]] <- mis
    }
    
    # test for empty elements
    siz <- lapply(dat, dim)
    dat <- dat[unlist(sapply(siz, "[", 1)) > 0]
    
    out <- lapply(dat, function(y) {
      testlimits <- c(max(c(min(y[, 1]) - 5, -180)), 
                      min(c(max(y[, 1]) + 5, 180)),
                      max(c(min(y[, 2]) - 5, -90)), 
                      min(c(max(y[, 2]) + 5, 90)))
      testdat <- referencedata[referencedata[, 1] > testlimits[1] & 
                                 referencedata[, 1] < testlimits[2], ]
      if (length(as.matrix(testdat)) == 2) {
        testdat <- testdat[testdat[2] > testlimits[3] &
                             testdat[2] < testlimits[4]]
      } else {
        testdat <- testdat[testdat[, 2] > testlimits[3] & 
                             testdat[, 2] < testlimits[4], ]
      }
      if(class(testdat) == "numeric"){
        testdat <- matrix(testdat, ncol = 2)
      }
      
      multpl <- 0
      while(dim(testdat)[1] == 0){
        multpl <- multpl + 10
        testlimits <- c(max(c(min(x[, 1]) - multpl, -180)),
                        min(c(max(x[, 1]) + multpl, 180)), 
                        max(c(min(x[, 2]) - multpl, -90)), 
                        min(c(max(x[, 2]) + multpl, 90)))
        testdat <- referencedata[referencedata[, 1] > testlimits[1] & 
                                   referencedata[, 1] < testlimits[2], ]
        if (length(as.matrix(testdat)) == 2) {
          testdat <- testdat[testdat[2] > testlimits[3] & testdat[2] < 
                               testlimits[4]]
        } else {
          testdat <- testdat[testdat[, 2] > testlimits[3] & 
                               testdat[, 2] < testlimits[4], ]
        }
        if(class(testdat) == "numeric"){
          testdat <- matrix(testdat, ncol = 2)
        }
      }
      
#       
#       if (dim(testdat)[1] == 0) {
#         testlimits <- c(max(c(min(y[, 1]) - 10, -180)), 
#                         min(c(max(y[, 1]) + 10, 180)), 
#                         (c(min(y[, 2]) - 10, -90)), 
#                         min(c(max(y[, 2]) + 10, 90)))
#         testdat <- referencedata[referencedata[, 1] > testlimits[1] & 
#                                    referencedata[, 1] < testlimits[2], ]
#         if (length(as.matrix(testdat)) == 2) {
#           testdat <- testdat[testdat[2] > testlimits[3] & testdat[2] < 
#                                testlimits[4]]
#         } else {
#           testdat <- testdat[testdat[, 2] > testlimits[3] & 
#                                testdat[, 2] < testlimits[4], ]
#         }
#         if(class(testdat) == "numeric"){
#           testdat <- matrix(testdat, ncol = 2)
#         }
#       }
#       if (dim(testdat)[1] == 0) {
#         testlimits <- c(max(c(min(y[, 1]) - 20, -180)),
#                         min(c(max(y[, 1]) + 20, 180)), 
#                         max(c(min(y[, 2]) - 20, -90)), 
#                         min(c(max(y[, 2]) + 20, 90)))
#         testdat <- referencedata[referencedata[, 1] > testlimits[1] & 
#                                    referencedata[, 1] < testlimits[2], ]
#         if (length(as.matrix(testdat)) == 2) {
#           testdat <- testdat[testdat[2] > testlimits[3] & testdat[2] < 
#                                testlimits[4]]
#         } else {
#           testdat <- testdat[testdat[, 2] > testlimits[3] & 
#                                testdat[, 2] < testlimits[4], ]
#         }
#         if(class(testdat) == "numeric"){
#           testdat <- matrix(testdat, ncol = 2)
#         }
#       }
#       if (dim(testdat)[1] == 0) {
#         testlimits <- c(max(c(min(y[, 1]) - 40, -180)), 
#                         min(c(max(y[, 1]) + 40, 180)), 
#                         max(c(min(y[, 2]) - 40, -90)), 
#                         min(c(max(y[, 2]) + 40, 90)))
#         testdat <- referencedata[referencedata[, 1] > testlimits[1] & 
#                                    referencedata[, 1] < testlimits[2], ]
#         if (length(as.matrix(testdat)) == 2) {
#           testdat <- testdat[testdat[2] > testlimits[3] & testdat[2] < 
#                                testlimits[4]]
#         } else {
#           testdat <- testdat[testdat[, 2] > testlimits[3] & 
#                                testdat[, 2] < testlimits[4], ]
#         }
#         if(class(testdat) == "numeric"){
#           testdat <- matrix(testdat, ncol = 2)
#         }
#       }
#       if (dim(testdat)[1] == 0) {
#         testlimits <- c(max(c(min(y[, 1]) - 60, -180)), 
#                         min(c(max(y[, 1]) + 60, 180)), 
#                         max(c(min(y[, 2]) - 60, -90)), 
#                         min(c(max(y[, 2]) + 60, 90)))
#         testdat <- referencedata[referencedata[, 1] > testlimits[1] & 
#                                    referencedata[, 1] < testlimits[2], ]
#         if (length(as.matrix(testdat)) == 2) {
#           testdat <- testdat[testdat[2] > testlimits[3] & testdat[2] < 
#                                testlimits[4]]
#         } else {
#           testdat <- testdat[testdat[, 2] > testlimits[3] & 
#                                testdat[, 2] < testlimits[4], ]
#         }
#         if(class(testdat) == "numeric"){
#           testdat <- matrix(testdat, ncol = 2)
#         }
#       }
#       if (dim(testdat)[1] == 0) {
#         testlimits <- c(max(c(min(y[, 1]) - 100, -180)), 
#                         min(c(max(y[, 1]) + 100, 180)), 
#                         max(c(min(y[, 2]) - 100, -90)), 
#                         min(c(max(y[, 2]) + 100, 90)))
#         testdat <- referencedata[referencedata[, 1] > testlimits[1] & 
#                                    referencedata[, 1] < testlimits[2], ]
#         if (length(as.matrix(testdat)) == 2) {
#           testdat <- testdat[testdat[2] > testlimits[3] & testdat[2] < 
#                                testlimits[4]]
#         } else {
#           testdat <- testdat[testdat[, 2] > testlimits[3] & 
#                                testdat[, 2] < testlimits[4], ]
#         }
#         if(class(testdat) == "numeric"){
#           testdat <- matrix(testdat, ncol = 2)
#         }
#       }
      
      if (method[1] == "Cosine") {
        out.int <- distm(y, testdat, fun = distCosine)
      }
      if (method[1] == "Haversine") {
        out.int <- distm(y, testdat, fun = distHaversine)
      }
      if (method[1] == "VincentyEllipse") {
        out.int <- distm(y, testdat, fun = distVincentyEllipsoid)
      }
      out.int <- apply(out.int, 1, "min")
      out.int <- round(out.int, 0)
      
      return(out.int)
    })
    out <- data.frame(as.numeric(unlist(lapply(dat, rownames))), unlist(out))
    out <- out[order(out[, 1]), ]
    out <- out[, 2]
  }
  
  out <- round(out/1000, 1)
  return(out)
} 