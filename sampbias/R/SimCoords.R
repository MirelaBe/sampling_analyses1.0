SimCoords <- function(x, polygon = NULL, reps = 3,
                      type = c("random", "regular", "stratified", "nonaligned", 
                               "hexagonal", "clustered", "Fibonacci"), 
                      method = c("convexhull", "rectangle", "polygon", "intersect"),
                      terrestrial = T, extent = c("regional", "global"), 
                      model = c("planar", "spheric"), n = NULL, 
                      refterrestrial = NULL) {
  
  match.arg(type)
  match.arg(method)
  match.arg(extent)
  match.arg(model)
  
  if (class(x) == "matrix" & dim(x)[2] > 2) {
    warning("more than 2 inputcolumns, column 1 assumed to be longitude, column 2 assumed to be latitude")
  }
  
  if (length(x) > 0 & class(x) == "data.frame") {
    if (dim(x)[2] > 2) {
      x <- x[, c("longitude", "latitude")]
      warning("more than two columns, inputcolumns guessed from column names")
    }
  }
  
  if (length(n) == 0) {
    n <- dim(x)[1]
  } else {
    n <- n
  }
  
  # prepare polygon for sampling
  if (method[1] == "convexhull") {
    poly <- x[chull(x), ]
    poly <- SpatialPolygons(list(Polygons(list(Polygon(poly)), 
                                          ID = paste(x[1, 1], "_convhull", sep = ""))), 
                            proj4string = CRS("+proj=longlat +datum=WGS84"))
  }
  if (method[1] == "rectangle") {
    poly <- rbind(c(min(x[, 1]), min(x[, 2])), c(min(x[, 1]), max(x[, 2])), 
                  c(max(x[, 1]), max(x[, 2])), c(max(x[, 1]), min(x[, 2])), 
                  c(min(x[, 1]), min(x[, 2])))
    poly <- SpatialPolygons(list(Polygons(list(Polygon(poly)), 
                                          ID = paste(x[1,1], "_convhull", sep = ""))), 
                            proj4string = CRS("+proj=longlat +datum=WGS84"))
  }
  
  if (method[1] == "polygon") {
    poly <- polygon
  }
  
  if (method[1] == "intersect") {
    poly <- x[chull(x), ]
    poly <- SpatialPolygons(list(Polygons(list(Polygon(poly)), 
                                          ID = paste(x[1, 1], "_convhull", sep = ""))), 
                            proj4string = CRS("+proj=longlat +datum=WGS84"))
    poly <- gIntersection(poly, polygon)
  }
  
  if (terrestrial == T) {
    
    if (is.null(refterrestrial)) {
      sampbias::landmass
    } else {
      landmass <- refterrestrial
    }
    poly <- suppressWarnings(gIntersection(poly, landmass))
  }
  
  if (model[1] == "planar") {
    out <- list()
    for(i in 1:reps){
      outi <- spsample(poly, n, type = type[1])
      outi <- data.frame(outi)
      names(outi) <- c("longitude", "latitude")
      out[[i]] <- outi
    }
  }
  if (model[1] == "spheric") {
    out <- list()
    for(i in 1:reps){
      if (extent[1] == "global") {
        outi <- randomCoordinates(n)
        names(outi) <- c("longitude", "latitude")
        out[[i]] <- outi
      } else {
        siz <- areaPolygon(poly)/1e+06
        mult <- round(510072000/siz, 0)
        outi <- SpatialPoints(randomCoordinates(n * mult), proj4string = CRS(proj4string(poly)))
        tt <- over(outi, poly)
        outi <- outi[!is.na(tt), ]
        outi <- coordinates(outi)
        outi <- data.frame(outi)
        names(outi) <- c("longitude", "latitude")
        out[[i]] <- outi
      }
    }
  }
  return(out)
} 