CleanCoordinates <- function(x, species = NULL, validity = T, zeros = T, 
                             capitals = T, centroids = T, seas = T, 
                             urban = F, countrycheck = T, outliers = T,
                             GBIF = T, duplicates = F, verbose = T, limits = NULL, 
                             output = c("spatialvalid","summary", "cleaned"), 
                             zeros.rad = 0.5, capitals.rad = 0.05, 
                             outliers.mtp = 25, outliers.td = NULL, 
                             centroids.rad = 0.01, capitals.ref = NULL, 
                             centroids.detail = c("both", "country", "provinces"),
                             centroids.ref = NULL, seas.ref = NULL, 
                             urban.ref = NULL, country.ref = NULL, 
                             method = c("Haversine", "Cosine", "VincentyEllipse"), report = F) {
  
  match.arg(output)
  match.arg(centroids.detail)
  match.arg(method)
  
  if (class(x) == "matrix") {
    if (dim(x)[2] > 2) {
      warning("more than 2 inputcolumns, column 1 assumed to be longitude, column 2 assumed to be latitude")
    }
    if (countrycheck == TRUE) {
      countrycheck <- FALSE
      warning("inputformat matrix, countrycheck set to FALSE")
    }
  }
  if (class(x) == "data.frame") {
    if (dim(x)[2] == 2) {
      if (countrycheck == TRUE) {
        countrycheck <- FALSE
        warning("found two columns, countrycheck set to FALSE")
      }
    }
    if (dim(x)[2] == 3) {
      if (!"country" %in% names(x)) {
        warning("assuming third column to contain country information")
      }
    }
    if (dim(x)[2] > 3) {
      x <- try(x[, c("longitude", "latitude", "country")])
      if (class(x) == "data.frame") {
        warning("more than three columns, inputcolumns guessed from column names")
      }
      if (class(x) == "try-error") {
        x <- try(x[, c("longitude", "latitude")])
        countrycheck <- FALSE
        if (class(x) == "data.frame") {
          warning("more than three columns, inputcolumns guessed from column names, no country column found, countrycheck set to FALSE")
        }
        if (class(x) == "try-error") {
          stop("to many input columns")
        }
      }
    }
  }
  names(x)[1:2] <- c("longitude", "latitude")
  
  if (class(x) == "SpatialPoints") {
    x <- coordinates(x)
  }
  
  if (validity == TRUE) {
    if (verbose == TRUE) {
      cat("running validity test\n")
    }
    val <- .ValidCoordinates(x[, c(1, 2)])
    if (all(val) == FALSE) {
      warning("invalid coordinates found, clean dataset before further tests:\n", 
              paste(which(val == FALSE), "\n"))
      return(val)
    } else {
      val <- rep(TRUE, dim(x)[1])
      
      if (zeros == T) {
        if (verbose == TRUE) {
          cat("running zero coordinate test\n")
        }
        zer <- .ZeroCoordinates(x[, c(1, 2)], pointlim = zeros.rad)
        if (verbose == TRUE) {
          cat(sprintf("flagged %s records \n", sum(!zer)))
        }
      } else {
        zer <- rep(NA, dim(x)[1])
      }
      
      if (capitals == TRUE) {
        if (verbose == TRUE) {
          cat("running capitals test\n")
        }
        cap <- .CapitalCoordinates(x[, c(1, 2)], testdist = capitals.rad, 
                                   limits = limits, referencedat = capitals.ref)
        if (verbose == TRUE) {
          cat(sprintf("flagged %s records \n", sum(!cap)))
        }
      } else {
        cap <- rep(NA, dim(x)[1])
      }
      
      if (centroids == TRUE) {
        if (verbose == TRUE) {
          cat("running centroids test\n")
        }
        cen <- .CentroidCoordinates(x[, c(1, 2)], testdist = centroids.rad, 
                                    limits = limits, testtype = centroids.detail, 
                                    referencedat = centroids.ref)
        if (verbose == TRUE) {
          cat(sprintf("flagged %s records \n", sum(!cen)))
        }
      } else {
        cen <- rep(NA, dim(x)[1])
      }
      
      if (seas == T) {
        if (verbose == TRUE) {
          cat("running seas test\n")
        }
        sea <- .WaterCoordinates(x[, c(1, 2)], poly = seas.ref)
        if (verbose == TRUE) {
          cat(sprintf("flagged %s records \n", sum(!sea)))
        }
      } else {
        sea <- rep(NA, dim(x)[1])
      }
      
      if (urban == TRUE) {
        if (verbose == TRUE) {
          cat("running urban test\n")
        }
        urb <- !.UrbanCoordinates(x[, c(1, 2)], poly = urban.ref, 
                                  limits = limits)
        if (verbose == TRUE) {
          cat(sprintf("flagged %s records \n", sum(!urb)))
        }
      } else {
        urb <- rep(NA, dim(x)[1])
      }
      
      if (countrycheck == TRUE) {
        if (verbose == TRUE) {
          cat("running countrycheck test\n")
        }
        dat <- x[, c(1, 2)]
        countries <- x[, 3]
        con <- .CountryCheck(dat, countries, poly = country.ref)
        if (verbose == TRUE) {
          cat(sprintf("flagged %s records \n", sum(!con, na.rm = T)))
        }
      } else {
        con <- rep(NA, dim(x)[1])
      }
      if (outliers == TRUE) {
        if (is.null(species)) {
          otl <- rep(NA, dim(x)[1])
          warning("No species argument found, outliers test skipped")
        } else {
          if (verbose == TRUE) {
            cat("running outliers test\n")
          }
          otl <- .OutlierCoordinates(x[, c(1, 2)], species = species, 
                                     multiplier = outliers.mtp, 
                                     tdi = outliers.td, method = method)
          if (verbose == TRUE) {
            cat(sprintf("flagged %s records \n", sum(!otl)))
          }
        }
      } else {
        otl <- rep(NA, dim(x)[1])
      }
      if (GBIF == TRUE) {
        if (verbose == TRUE) {
          cat("running GBIF test\n")
        }
        gbf <- apply(x, 1, function(y) 
          all(!(y["longitude"] >(12.58 - 0.5) & y["longitude"] <
                  (12.58 + 0.5) & y["latitude"] >
                  (55.67 - 0.5) & y["latitude"] < 
                  (55.67 + 0.5))))
        if (verbose == TRUE) {
          cat(sprintf("flagged %s records \n", sum(!gbf)))
        }
      } else {
        gbf <- rep(NA, dim(x)[1])
      }
      if(duplicates == T){
        cat("running duplicates test\n")
        if(is.null(species)){
          dpl.test <- x
        }else{
          dpl.test <- data.frame(x, species)
        }
        dpl <- !duplicated(dpl.test)
        if (verbose == TRUE) {
          cat(sprintf("flagged %s records \n", sum(!dpl)))
        }
      } else {
        dpl <- rep(NA, dim(x)[1])
      }
    }
  }
  if (validity == FALSE) {
    val <- rep(NA, dim(x)[1])
    if (zeros == T) {
      if (verbose == TRUE) {
        cat("running zero coordinate test\n")
      }
      zer <- .ZeroCoordinates(x[, c(1, 2)], pointlim = zeros.rad)
      if (verbose == TRUE) {
        cat(sprintf("flagged %s records \n", sum(!zer)))
      }
    } else {
      zer <- rep(NA, dim(x)[1])
    }
    
    if (capitals == TRUE) {
      if (verbose == TRUE) {
        cat("running capitals test\n")
      }
      cap <- .CapitalCoordinates(x[, c(1, 2)], testdist = capitals.rad, 
                                 limits = limits, referencedat = capitals.ref)
      if (verbose == TRUE) {
        cat(sprintf("flagged %s records \n", sum(!cap)))
      }
    } else {
      cap <- rep(NA, dim(x)[1])
    }
    
    if (centroids == TRUE) {
      if (verbose == TRUE) {
        cat("running centroids test\n")
      }
      cen <- .CentroidCoordinates(x[, c(1, 2)], testdist = centroids.rad, 
                                  limits = limits, testtype = centroids.detail, 
                                  referencedat = centroids.ref)
      if (verbose == TRUE) {
        cat(sprintf("flagged %s records \n", sum(!cen)))
      }
    } else {
      cen <- rep(NA, dim(x)[1])
    }
    
    if (seas == T) {
      if (verbose == TRUE) {
        cat("running seas test\n")
      }
      sea <- .WaterCoordinates(x[, c(1, 2)], poly = seas.ref)
      if (verbose == TRUE) {
        cat(sprintf("flagged %s records \n", sum(!sea)))
      }
    } else {
      sea <- rep(NA, dim(x)[1])
    }
    
    if (urban == TRUE) {
      if (verbose == TRUE) {
        cat("running urban test\n")
      }
      urb <- !.UrbanCoordinates(x[, c(1, 2)], poly = urban.ref,
                                limits = limits)
      if (verbose == TRUE) {
        cat(sprintf("flagged %s records \n", sum(!urb)))
      }
    } else {
      urb <- rep(NA, dim(x)[1])
    }
    
    if (countrycheck == TRUE) {
      if (verbose == TRUE) {
        cat("running countrycheck test\n")
      }
      dat <- x[, c(1, 2)]
      countries <- x[, 3]
      con <- .CountryCheck(dat, countries, poly = country.ref)
      if (verbose == TRUE) {
        cat(sprintf("flagged %s records \n", sum(!con, na.rm = T)))
      }
    } else {
      con <- rep(NA, dim(x)[1])
    }
    if (outliers == TRUE) {
      if (is.null(species)) {
        otl <- rep(NA, dim(x)[1])
        warning("no species argument found, outliers test skipped")
      } else {
        if (verbose == TRUE) {
          cat("running outliers test\n")
        }
        otl <- .OutlierCoordinates(x[, c(1, 2)], species = species, 
                                   multiplier = outliers.mtp, 
                                   tdi = outliers.td, method = method)
        if (verbose == TRUE) {
          cat(sprintf("flagged %s records \n", sum(!otl)))
        }
      }
    } else {
      otl <- rep(NA, dim(x)[1])
    }
    if (GBIF == TRUE) {
      if (verbose == TRUE) {
        cat("running GBIF test\n")
      }
      gbf <- apply(x, 1, function(y) 
        all(!(y["longitude"] > (12.58 -0.5) & y["longitude"] < 
                (12.58 + 0.5) & y["latitude"] >
                (55.67 - 0.5) & y["latitude"] < 
                (55.67 + 0.5))))
      if (verbose == TRUE) {
        cat(sprintf("flagged %s records \n", sum(!gbf)))
      }
    } else {
      gbf <- rep(NA, dim(x)[1])
    }
    if(duplicates == T){
      cat("running duplicates test\n")
      if(is.null(species)){
        dpl.test <- x
      }else{
        dpl.test <- data.frame(x, species)
      }
      dpl <- !duplicated(dpl.test)
      if (verbose == TRUE) {
        cat(sprintf("flagged %s records \n", sum(!dpl)))
      }
    } else {
      dpl <- rep(NA, dim(x)[1])
    }
  }
  
  out <- list(val, zer, cap, cen, sea, urb, con, otl, gbf, dpl)
  out <- Filter(function(x) !all(is.na(x)), out)
  out <- Reduce("&", out)
  
  if (verbose == TRUE) {
    cat(sprintf("flagged %s of %s records, EQ = %s \n", sum(!out, na.rm = T), 
                length(out), round(sum(!out, na.rm = T)/length(out), 2)))
  }
  if (output[1] == "spatialvalid") {
    out <- data.frame(x[, 1:2], validity = val, zeros = zer, capitals = cap, 
                      centroids = cen, sea = sea, urban = urb, countrycheck = con, 
                      outliers = otl, gbif = gbf, duplicates = dpl, summary = out)
    out <- Filter(function(x) !all(is.na(x)), out)
    class(out) <- c("spatialvalid", class(out))
  }
  if (output[1] == "cleaned"){
    if(is.null(species)){
    out <- data.frame(x[, 1:2], validity = val, zeros = zer, capitals = cap, 
                      centroids = cen, sea = sea, urban = urb, countrycheck = con, 
                      outliers = otl, gbif = gbf, duplicates = dpl, summary = out)
    out <- Filter(function(x) !all(is.na(x)), out)
    out <- out[,1:2]
    }else{
      out <- data.frame(x[, 1:2], species = species, validity = val, zeros = zer, 
                        capitals = cap, centroids = cen, sea = sea, urban = urb, 
                        countrycheck = con, outliers = otl, gbif = gbf, duplicates = dpl, summary = out)
      out <- Filter(function(x) !all(is.na(x)), out)
      out <- out[,1:3]
    }
  }
  
  if(report == T){
    report <- "CleanCoordinates_report.txt"
  }
  if(is.character(report)){
    suma <- data.frame(test = as.character(names(out[-c(1:2)])), flagged.records = colSums(!out[-c(1:2)]))
    suma <- rbind(suma, c("Error Quotient", round(sum(out$summary, na.rm = T)/length(out$summary), 2)))
    write.table(suma, report, sep = "\t", row.names = F)
  }
  
  return(out)
}