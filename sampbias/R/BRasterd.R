BRasterd <- function(x, sim.outp.path, verb = T, 
                     sim.type = c("random", "regular","stratified",
                                  "nonaligned", "hexagonal","clustered",
                                  "Fibonacci"),
                     sim.method = c("convexhull", "rectangle", 
                                    "polygon", "intersect"),
                     sim.extent = c("regional", "global"), 
                     sim.model = c("planar", "spheric"), ...){
  
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
  
  ras <- coordinates(raster(nrows = ceiling(lonspan/(cellsize)), 
                            ncols = ceiling(latspan/(cellsize)), 
                            xmn = min(x[, 1]), xmx = max(x[, 1]),
                            ymn = min(x[, 2]), ymx = max(x[, 2])))
  ras <- ras <- GridTopology(cellcentre.offset = c(min(x[, 1]), min(x[, 2])), 
                             cellsize = c((cellsize), (cellsize)), 
                             cells.dim = c(ceiling(lonspan/(cellsize)),
                                           ceiling(latspan/(cellsize))))
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
  
  
  samps <- lapply(dat, function(x) SamplingBiasLarge(x, ...))
  sims <- lapply(dat, function(X) SimCoords(x))
  lis <- list(1:length(samps))
  eva <- lapply(lis, BiasEval(samps[[lis]], SimCoords[[lis]]))
}