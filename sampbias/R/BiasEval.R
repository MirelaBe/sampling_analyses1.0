BiasEval <- function(x, y, output = c("sp.bias.eval", "B", "data.frame")) {
  match.arg(output)
  
  x <- Filter(function(z) !all(is.na(z)), x)
  y <- Filter(function(z) !all(is.na(z)), y)
  
  avg.x <- x[, sapply(x, is.numeric)]
  out.x <- data.frame(t(apply(avg.x, 2, mean, na.rm = T)))
  out.x <- round(out.x, 0)
  
  avg.y <- y[, sapply(y, is.numeric)]
  out.y <- data.frame(t(apply(avg.y, 2, mean, na.rm = T)))
  out.y <- round(out.y, 0)
  
  if ("Interpoint.sd" %in% names(x)) {
    out.x$Interpoint.sd <- round(sd(x$Interpoint.sd, na.rm = T), 1)
    out.y$Interpoint.sd <- round(sd(y$Interpoint.sd, na.rm = T), 1)
  }
  if ("Protectedarea" %in% names(x)) {
    out.x$Protectedarea <- round(sum(x$Protectedarea, na.rm = T)/
                                   length(x$Protectedarea),2)
    out.y$Protectedarea <- round(sum(y$Protectedarea, na.rm = T)/
                                   length(y$Protectedarea),2)
  }
  out.x <- t(out.x)
  out.y <- t(out.y)
  
#   # ANOVA
#   test <- rbind(x, y)
#   ds <- c(rep("org", dim(x)[1]), rep("sim", dim(y)[1]))
#   test <- test[, -which(names(test) %in% c("Protectedarea", "socioeconomic.Iso3"))]
#   if (nas[1] == "max.pred") {
#     warning(sprintf("%s datapoints omitted due to missing values \n", 
#                     dim(test[!complete.cases(test), ])[1]))
#     ds <- ds[complete.cases(test)]
#     test <- test[complete.cases(test), ]
#   }
#   if (nas[1] == "max.samp") {
#     warning(sprintf("%s datapoints omitted due to missing values \n", 
#                     dim(test[, apply(test, 2, function(x) any(is.na(x)))])[1]))
#     test <- test[, !apply(test, 2, function(x) any(is.na(x)))]
#   }
#   
#   anv.res <- apply(test, 2, function(x) summary(aov(x ~ ds)))
#   anv.res <- lapply(anv.res, function(x) unlist(x)["Pr(>F)1"])
#   anv.res <- do.call("rbind.data.frame", anv.res)
#   
  out <- merge(out.x, out.y, by = "row.names", all = T)
  rownames(out) <- out$Row.names
  out <- out[, -1]
#   out <- merge(out, anv.res, by = "row.names", all = T)
  
  class(out) <- c(class(out), "samp.bias.eval")
#   rownames(out) <- out$Row.names
#   out <- out[, -1]
#   names(out) <- c("data", "simulated", "ANOVA.Pr(>F)")
  names(out) <- c("data", "simulated")
  out$partial.B <- round((out$data - out$simulated)/out$simulated, 2)
  
  if ("socioeconomic.Iso3" %in% names(x)) {
    countr <- merge(data.frame(t(table(as.character(x$socioeconomic.Iso3)))), 
                    data.frame(t(table(as.character(y$socioeconomic.Iso3)))),
                    by = "Var2", all = T)
    countr <- countr[, c("Var2", "Freq.x", "Freq.y")]
    countr[is.na(countr)] <- 0
    countrB <- round(mean(abs(round((countr$Freq.x - countr$Freq.y)/
                                      countr$Freq.y, 2))), 2)
    countr <- data.frame(NA, NA, countrB, row.names = "country")
    names(countr) <- names(out)
    
    out <- rbind(out, countr)
  }
  
  global.B <- mean(abs((out$data - out$simulated)/out$simulated), na.rm = T)
  
  if (output[1] == "data.frame") {
    return(out)
  }
  if (output[1] == "B") {
    return(global.B)
  }
  if (output[1] == "sp.bias.eval") {
    out <- (list(global.B = global.B, details = out))
    class(out) <- c("sp.bias.eval", class(out))
  }
  return(out)
}