SimCoordsLarge <- function(x, outp.path, verb = TRUE, ...) {
  if (dim(x)[1] < 10000) {
    out <- SimCoords(x, ...)
  } else {
    convh <- x[chull(x), ]
    convh <- SpatialPolygons(list(
      Polygons(list(Polygon(convh)), ID = paste(x[1,1], "_convhull", sep = ""))), 
      proj4string = CRS("+proj=longlat +datum=WGS84"))
    
    len <- ceiling(dim(x)[1]/10000)
    
    tt <- SimCoords(x, n = 10000, ...)
    write.table(tt, outp.path, sep = "\t", row.names = F)
    if (verb == T) {
      print(paste("Simulating set ", 1, "/", len, sep = ""))
    }
    
    for (i in 2:len) {
      if (verb == T) {
        print(paste("Simulating set ", i, "/", len, sep = ""))
      }
      tt <- SimCoords(x, n = 10000, ...)
      write.table(tt, outp.path, append = T, col.names = F, sep = "\t", 
                  row.names = F)
      rm(tt)
    }
    
    out <- read.table(outp.path, sep = "\t", header = T, row.names = NULL)
    out <- out[!duplicated(out), ]
    
    dif <- dim(x)[1] - dim(out)[1]
    if (dif < 0) {
      out <- out[sample(1:dim(out)[1], dim(x)[1]), ]
    } else {
      add <- SimCoords(x, polygon = convh, n = dif * 2)
      out <- rbind(out, add)
      out <- out[sample(1:dim(out)[1], dim(x)[1]), ]
      rownames(out) <- NULL
    }
    
  }
  return(out)
} 