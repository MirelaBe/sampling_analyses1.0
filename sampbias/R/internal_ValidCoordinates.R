.ValidCoordinates <- function(x) {
  out <- vector("logical", length = dim(x)[1])
  out <- !out
  
  # NA test
  out[which(is.na(x$longitude) | is.na(x$latitude))] <- FALSE
  
  # is numeric
  out[which(suppressWarnings(is.na(as.numeric(as.character(x$longitude)))))] <- FALSE
  out[which(suppressWarnings(is.na(as.numeric(as.character(x$latitude)))))] <- FALSE
  
  # coordinate validity
  out[which(suppressWarnings(as.numeric(as.character(x$longitude))) > 
              180 | suppressWarnings(as.numeric(as.character(x$longitude))) < 
              -180)] <- FALSE
  out[which(suppressWarnings(as.numeric(as.character(x$latitude))) > 
              90 | suppressWarnings(as.numeric(as.character(x$latitude))) < -90)] <- FALSE
  
  return(out)
}