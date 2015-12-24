.ZeroCoordinates <- function(x, pointlim = 0.5) {
  out <- vector("logical", length = dim(x)[1])
  out <- !out
  
  # plain zero in coordinates
  out[which(x$longitude == 0 | x$latitude == 0)] <- FALSE
  
  # rectangle around point 0/0
  loncap <- x$longitude > (0 - pointlim / 2) & x$longitude < (0 + pointlim / 2)
  latcap <- x$latitude > (0 - pointlim / 2) & x$latitude < (0 + pointlim / 2)
  
  out[which(loncap == T & latcap == T)] <- FALSE
  
  # lat == long
  out[which(x$longitude == x$latitude)] <- FALSE
  
  return(out)
} 