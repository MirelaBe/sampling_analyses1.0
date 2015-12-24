.SocioEconomic <- function(x, polyg = NULL, soc.eco = NULL) {
  pts <- SpatialPoints(x)
  
  if (length(polyg) == 0) {
    testpolys <- sampbias::countryborders
  } else {
    testpolys <- polyg
  }
  country <- over(x = pts, y = testpolys)
  country <- data.frame(x, Iso3 = country$ISO3)
  
  if (length(soc.eco) == 0) {
    socioeconomic <- sampbias::socioeconomic
  } else {
    socioeconomic <- soc.eco
  }
  
  out <- merge(country, socioeconomic, by = "Iso3", all.x = T)
  out <- out[, -c(2:4, 11)]
  out[, 2:9] <- apply(out[, 2:9], 2, 
                      function(x) as.numeric(as.character(x)))
  
  return(out)
} 