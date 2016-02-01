CleanCoordinatesLarge <- function(x, species = NULL, outp.path = NULL, verb = T, rpt = F, dupl = F, 
                                  ...) {
  
  val.test <- .ValidCoordinates(x)
  if (!all(val.test)) {
    stop("invalid coordinates found, clean the following records before further tests:\n", 
         paste(which(val.test == FALSE), "\n"))
  }
  if (dim(x)[1] < 10000) {
    out <- CleanCoordinates(x, species, ...)
  } else {
    report <- FALSE
    if (!is.null(species)) {
      
      dat <- data.frame(x, species)
      dat <- dat[order(dat$species), ]
      splitter <- rep(1:ceiling(dim(dat)[1]/10000), each = 10000)
      splitter <- splitter[1:dim(dat)[1]]
      dat <- split(dat, f = splitter)
      
      for (i in 1:(length(dat) - 1)) {
        dat[[i]] <- rbind(dat[[i]], dat[[i + 1]][which(as.character(dat[[i +1]]$species) %in% 
                                                         as.character(dat[[i]]$species)), ])
        dat[[i + 1]] <- dat[[i + 1]][!as.character(dat[[i + 1]]$species) %in% 
                                       as.character(dat[[i]]$species), ]
      }
      if (is.null(outp.path)) {
        li <- list()
        for (i in 1:length(dat)) {
          li[[i]] <- CleanCoordinates(dat[[i]][, -which(names(dat[[i]]) == 
                                                          "species")], dat[[i]][, "species"], ...)
          if (verb == T) {
            print(paste(i, "/", length(dat), "Done. Local EQ = ", 
                        round(sum(!li[[i]]$summary)/length(li[[i]]$summary), 2)), sep = "")
          }
        }
        out <- do.call("rbind.data.frame", li)
        tt <- as.numeric(unlist(lapply(li, "rownames")))
        out <- out[order(tt), ]
        rownames(out) <- rownames(x)
      } else {
        tt <- CleanCoordinates(dat[[1]][, -which(names(dat[[1]]) == 
                                                   "species")], dat[[1]][, "species"], ...)
        if (verb == T) {
          print(paste(1, "/", length(dat), "Done. Local EQ = ", 
                      round(sum(!tt$summary)/length(tt$summary), 2)), sep = "")
        }
        write.table(tt, outp.path, sep = "\t")
        for (i in 2:length(dat)) {
          tt <- CleanCoordinates(dat[[i]][, -which(names(dat[[1]]) == 
                                                     "species")], dat[[i]][, "species"], ...)
          write.table(tt, outp.path, append = T, col.names = F, sep = "\t")
          if (verb == T) {
            print(paste(i, "/", length(dat), "Done. Local EQ = ", 
                        round(sum(!tt$summary)/length(tt$summary), 2)), sep = "")
          }
        }
        rm(tt)
        out <- read.table(outp.path, sep = "\t")
        tt <- as.numeric(rownames(out))
        out <- out[order(tt), ]
        rownames(out) <- rownames(x)
        class(out) <- c("spatialvalid", class(out))
      }
    } else {
      splitter <- rep(1:ceiling(dim(x)[1]/10000), each = 10000)
      splitter <- splitter[1:dim(x)[1]]
      dat <- split(x, f = splitter)
      
      if (is.null(outp.path)) {
        li <- list()
        for (i in 1:length(dat)) {
          li[[i]] <- CleanCoordinates(dat[[i]][, -which(names(dat[[i]]) == 
                                                          "species")], dat[[i]][, "species"], ...)
          if (verb == T) {
            print(paste(i, "/", length(dat), "Done. Local EQ = ", 
                        round(sum(!li[[i]]$summary)/length(li[[i]]$summary), 2)), sep = "")
          }
        }
        out <- do.call("rbind.data.frame", li)
        tt <- as.numeric(unlist(lapply(li, "rownames")))
        out <- out[order(tt), ]
        rownames(out) <- rownames(x)
      } else {
        tt <- CleanCoordinates(dat[[1]], ...)
        if (verb == T) {
          print(paste(1, "/", length(dat), "Done. Local EQ = ", 
                      round(sum(!tt$summary)/length(tt$summary), 2)), sep = "")
        }
        write.table(tt, outp.path, sep = "\t")
        for (i in 2:length(dat)) {
          tt <- CleanCoordinates(dat[[i]], ...)
          if (verb == T) {
            print(paste(i, "/", length(dat), "Done. Local EQ = ", 
                        round(sum(!tt$summary)/length(tt$summary), 2)), sep = "")
          }
          write.table(tt, outp.path, append = T, col.names = F, sep = "\t")
          rm(tt)
        }
        out <- read.table(outp.path, sep = "\t", header = T)
        tt <- as.numeric(rownames(out))
        out <- out[order(tt), ]
        rownames(out) <- rownames(x)
        class(out) <- c("spatialvalid", class(out))
      }
    }
  }
  if (verb == T) {
    print(paste("Global EQ = ", 
                round(sum(!out$summary)/length(out$summary), 2), sep = ""))
  }
  
  if(dupl == T){
    cat("checking for duplicates\n")
    if(is.null(species)){
      dpl.test <- x
    }else{
      dpl.test <- data.frame(x, species)
    }
    dpl <- !duplicated(dpl.test)
    cat(sprintf("flagged %s records \n", sum(!dpl)))
    out <- data.frame(out, duplicates = dpl)
    out$summary <- Reduce("&", out[,-c(1,2)])
    class(out) <- c("spatialvalid", class(out))
  }
  
  if(rpt == T){
    rep.nam <- "CleanCoordinates_report.txt"
  }
  if(is.character(rpt)){
    rep.nam <- rpt
    suma <- data.frame(test = c(as.character(names(out[-c(1:2)])), "Error Quotient"), 
                       flagged.records = c(colSums(!out[-c(1:2)]), 
                                           round(sum(out$summary, na.rm = T)/length(out$summary), 2)))
    write.table(suma, rep.nam, sep = "\t", row.names = F)
  }
  
  return(out)
} 