FindOutliers <- function(x, species, outp = c("data.frame", "summary","outliers.list"), 
                         outliers.mtp = NULL, outliers.td = 100) {
  match.arg(outp)
  dat <- x
  if (!all(c("longitude", "latitude", "outliers") %in% names(x))) {
    if (!all(c("longitude", "latitude") %in% names(x))) {
      stop("x must be of class spatialvalid or a data.frame with columns 'longitude', 'latitude'")
    } else {
      dat <- CleanCoordinates(x, species = species, validity = F, zeros = F, 
                            capitals = F, centroids = F, seas = F, 
                            urban = F, countrycheck = F, outliers = T, 
                            GBIF = F, duplicates = F, verbose = F, outliers.mtp = outliers.mtp,
                            outliers.td = outliers.td, 
                            output = "spatialvalid")
    }
  }
  dat <- data.frame(species = species, longitude = dat$longitude, 
                    latitude = dat$latitude, outliers = dat$outliers)

  if (outp[1] == "summary") {
    out <- dat[!dat$outliers, ]
    out$species <- as.character(out$species)
    out <- data.frame(table(out$species))
    names(out) <- c("species", "n.outliers")
  }
  if (outp[1] == "data.frame") {
    out <- dat[!dat$outliers, ]
    out <- out[, -4]
  }
  if (outp[1] == "outliers.list") {
    out <- split(dat, f = dat$species)
    sel <- lapply(out, function(x) all(x$outliers))
    out <- out[!unlist(sel)]
    class(out) <- c("outliers.list", class(out))
  }
  return(out)
}