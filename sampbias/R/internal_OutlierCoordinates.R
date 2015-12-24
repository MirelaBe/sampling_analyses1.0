.OutlierCoordinates <- function(x, species, multiplier, tdi, method = "Haversine") {
  out <- rep(TRUE, dim(x)[1])
  
  splist <- split(x, f = as.character(species))
  
  flags <- lapply(splist, function(y, td = tdi, mltpl = multiplier) {
    y <- data.frame(y)
    mupo <- y
    y <- y[!duplicated(y), ]
    if (dim(y)[1] < 3) {
      return(NA)
    } else {
      if (method[1] == "Cosine") {
        dist <- distm(y, fun = distCosine)
      }
      if (method[1] == "Haversine") {
        dist <- distm(y, fun = distHaversine)
      }
      if (method[1] == "VincentyEllipse") {
        dist <- distm(y, fun = distVincentyEllipsoid)
      }
      
      dist[dist == 0] <- NA
      
      if (!is.null(mltpl) & !is.null(td)) {
        stop("set outliers.td OR outliers.mtp, the other one to NULL")
      }
      if (!is.null(mltpl)) {
        mins <- apply(dist, 1, min, na.rm = T)
        quo <- quantile(mins, c(0.99), na.rm = T)
        # ind <- which(mins > (quo + IQR(mins, na.rm = T) * mltpl))
        ind <- which(mins > (quo + mean(mins, na.rm = T) * mltpl))
      }
      if (!is.null(td)) {
        mins <- apply(dist, 1, min, na.rm = T)
        ind <- which(mins > td * 1000)
      }
      
      # pick duplicates of flagged coordinates
      
      if (length(ind) == 0) {
        mupo <- NA
      }
      if (length(ind) == 1) {
        sel <- merge(y[ind, ], mupo, by.x = c(1, 2), by.y = c(1, 2))
        mupo <- mupo[mupo[, 1] %in% sel[1] & mupo[, 2] %in% sel[2], 
                     ]
      }
      if (length(ind) > 1) {
        sel <- merge(y[ind, ], mupo, by.x = c(1, 2), by.y = c(1, 2))
        mupo <- mupo[as.logical(rowSums(apply(sel, 1, function(z) {
          mupo[, 1] %in% z[1] & mupo[, 2] %in% z[2]
        }))), ]
      }
      return(rownames(mupo))
    }
  })
  flags <- unlist(flags)
  flags <- flags[!is.na(flags)]
  out[which(rownames(x) %in% flags)] <- FALSE
  return(out)
}