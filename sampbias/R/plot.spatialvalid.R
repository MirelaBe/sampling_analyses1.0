plot.spatialvalid <- function(x, bgmap = NULL, clean = T, details = T, 
                              ...) {
  if (min(x[, 1]) * max(x[, 1]) < 0) {
    lonspan <- abs(min(x[, 1])) + abs(max(x[, 1]))
  } else {
    lonspan <- abs(max(x[, 1])) - abs(min(x[, 1]))
  }
  if (min(x[, 2]) * max(x[, 2]) < 0) {
    latspan <- abs(min(x[, 2])) + abs(max(x[, 2]))
  } else {
    latspan <- abs(max(x[, 2])) - abs(min(x[, 2]))
  }
  
  if (is.null(bgmap)) {
    bgmap <- sampbias::landmass
  }
  
  plot(bgmap, ...)
  axis(1)
  axis(2)
  if (details == TRUE) {
    poi <- x[, !names(x) %in% c("longitude", "latitude", "summary")]
    for (i in 1:dim(poi)[2]) {
      points(x[poi[, i] == FALSE, 1], x[poi[, i] == FALSE, 2], col = "darkred", 
             pch = i, ...)
    }
    if (clean == TRUE) {
      points(x[x[, "summary"] == TRUE, 1],
             x[x[, "summary"] == TRUE,2], 
             col = "darkgreen", pch = 16, ...)
      legend("bottom", c(names(poi), "Clean"), 
             pch = c(1:dim(poi)[2], 16), 
             col = c(rep("darkred", dim(poi)[2]), 
                     "darkgreen"), 
             ncol = 4, bg = "white")
    } else {
      legend("bottom", names(poi), pch = 1:dim(poi)[2], 
             col = rep("darkred", dim(poi)[1]), ncol = 4, bg = "white")
      
    }
  }
  if (details == FALSE) {
    points(x[x[, "summary"] == FALSE, 1], 
           x[x[, "summary"] == FALSE,2], 
           col = "darkred", pch = 4, ...)
    if (clean == T) {
      points(x[x[, "summary"] == TRUE, 1], 
             x[x[, "summary"] == TRUE, 2], 
             col = "darkgreen", pch = 16, ...)
      legend("bottomleft", c("Flagged", "Clean"), 
             col = c("darkred", "darkgreen"), pch = c(4,16))
    }
  }
  box("plot")
} 