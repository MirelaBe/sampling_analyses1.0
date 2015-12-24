plot.outliers.list <- function(x, type = c("perspecies", "alloutliers"), 
                               pch = 4, col = "red", cex = 0.5, bg = NULL, 
                               ...) {
  match.arg(type)
  if (is.null(bg)) {
    mins <- c(min(unlist(lapply(x, function(y) min(y$longitude)))) - 5, 
              max(unlist(lapply(x, function(y) max(y$longitude)))) + 
                5)
    maxs <- c(min(unlist(lapply(x, function(y) min(y$latitude)))) - 5, 
              max(unlist(lapply(x, function(y) max(y$latitude)))) + 
                5)
  }
  
  
  if (type[1] == "alloutliers") {
    if (is.null(bg)) {
      plot(mins, maxs, type = "n", xlab = "", ylab = "", ...)
    } else {
      plot(bg, ...)
    }
    dat <- lapply(x, function(y) y[!y$outliers, ])
    lapply(dat, function(y) {
      points(y$longitude, y$latitude, pch = pch, col = col, cex = cex)
    })
    box("plot")
  }
  
  if (type[1] == "perspecies") {
    dat <- as.list(1:length(x))
    lapply(dat, function(y) {
      if (is.null(bg)) {
        plot(mins, maxs, type = "n", xlab = "", ylab = "", ...)
      } else {
        plot(bg, ...)
      }
      points(x[[y]][x[[y]]$outliers, ]$longitude, 
             x[[y]][x[[y]]$outliers, ]$latitude, 
             col = "green", pch = 16, 
             cex = cex)
      points(x[[y]][!x[[y]]$outliers, ]$longitude, 
             x[[y]][!x[[y]]$outliers, ]$latitude, 
             col = col, pch = pch, 
             cex = cex)
      title(names(x)[[y]])
      legend("bottomleft", c("Outlier", "Clean"), 
             col = c(col, "green"), pch = c(pch, 16))
      box("figure")
    })
  }
} 