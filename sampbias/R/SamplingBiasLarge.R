SamplingBiasLarge <- function(x, outp.path = NULL, cellsize = 2, 
                              verb = T, ...) {
  
  if(dim(x)[1] < 10000){
    out <- SamplingBias(x, ...)
  }else{
  
  intpref <- x
  
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
                            ncols = ceiling(latspan/(cellsize * 2)), 
                            xmn = min(x[, 1]), xmx = max(x[, 1]),
                            ymn = min(x[, 2]), ymx = max(x[, 2])))
  ras <- ras <- GridTopology(cellcentre.offset = c(min(x[, 1]), min(x[, 2])), 
                             cellsize = c((cellsize * 2), (cellsize * 2)), 
                             cells.dim = c(ceiling(lonspan/(cellsize * 2)),
                                           ceiling(latspan/(cellsize * 2))))
  ras <- coordinates(ras)
  
  cr <- list()
  for (i in 1:dim(ras)[1]) {
    cr[[i]] <- extent(c(ras[i, 1] - (cellsize - 1e-08), ras[i, 1] + 
                          cellsize, ras[i, 2] - (cellsize - 1e-08), ras[i, 2] + cellsize))
  }
  
  dat2 <- SpatialPoints(x)
  rownames(dat2@coords) <- rownames(x)
  dat <- lapply(cr, function(x) crop(dat2, x))
  dat <- dat[!unlist(lapply(dat, is.null))]
  dat <- lapply(dat, coordinates)
  mis <- as.numeric(unlist(lapply(dat, rownames)))
  mis <- x[which(!rownames(x) %in% mis), ]  # add entries missed by geographic splitting
  dat[[length(dat) + 1]] <- mis
  dat <- lapply(dat, function(z) {
    splitter <- rep(1:ceiling(dim(z)[1]/10000), each = 10000)
    splitter <- splitter[1:dim(z)[1]]
    splitter <- split(data.frame(z), f = splitter)
    return(splitter)
  })
  dat <- list(unlist(dat, recursive = F))[[1]]
  
  if (is.null(outp.path)) {
    li <- lapply(dat, function(z) SamplingBias(z, refinterpoint = intpref, ...))
    out <- do.call("rbind.data.frame", li)
    tt <- as.numeric(unlist(lapply(li, "rownames")))
    out <- out[order(tt), ]
    rownames(out) <- rownames(x)
    
  } else {
    tt <- SamplingBias(dat[[1]], refinterpoint = intpref, verbose = F, ...)
    rownames(tt) <- rownames(dat[[1]])
    write.table(tt, outp.path, sep = "\t")
    for (i in 2:length(dat)) {
      if (verb == T) {
        print(paste(i, "/", length(dat)))
      }
      tt <- SamplingBias(dat[[i]], refinterpoint = intpref, verbose = F, ...)
      rownames(tt) <- rownames(dat[[i]])
      write.table(tt, outp.path, append = T, col.names = F, sep = "\t")
      rm(tt)
    }
    out <- read.table(outp.path, sep = "\t", row.names = NULL)

    cor <- x[which(!rownames(x) %in% out$row.names),]
    if( dim(cor)[1] > 0){
      if(verb == TRUE){
        cat("Applying corrections") # sometimes samples are missed so that dim(out) != dim(x), so here check for this
      }
    if(dim(cor)[1] > 10000){
    cor <- lapply(cor, function(z) {
      splitter <- rep(1:ceiling(dim(z)[1]/10000), each = 10000)
      splitter <- splitter[1:dim(z)[1]]
      splitter <- split(data.frame(z), f = splitter)})
      return(splitter)
      for (i in 2:length(cor)) {
        if (verb == T) {
          print(paste(i, "/", length(cor)))
        }
        tt <- SamplingBias(dat[[i]], refinterpoint = intpref, verbose = F, ...)
        rownames(tt) <- rownames(dat[[i]])
        write.table(tt, outp.path,  append = T, col.names = F, sep = "\t")
        rm(tt)
      }
    }else{
      tt <- SamplingBias(cor, refinterpoint = intpref, verbose = F, ...)
      rownames(tt) <- rownames(cor)
      write.table(tt, outp.path, append = T, col.names = F, sep = "\t")
      rm(tt)
    }
    }
    out <- read.table(outp.path, sep = "\t", row.names = NULL)
    
    out <- out[!duplicated(out$row.names),]
    rownames(out) <- out$row.names
    out <- out[, -1]
    out <- out[order(as.numeric(rownames(out))), ]
  }
  }
  return(out)
} 