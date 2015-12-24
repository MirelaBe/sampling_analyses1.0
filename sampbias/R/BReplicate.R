BReplicate <- function(x, reps = 10, samp.outp.path, 
                       sim.outp.path, verb = T, 
                       sim.type = c("random", "regular","stratified",
                                    "nonaligned", "hexagonal","clustered",
                                    "Fibonacci"),
                       sim.method = c("convexhull", "rectangle", 
                                        "polygon", "intersect"),
                       sim.extent = c("regional", "global"), 
                       sim.model = c("planar", "spheric"), ...){
  #calculate SampBiasLarge for X
  sampl.x <- SamplingBiasLarge(x, outp.path = samp.outp.path, ...)
  out <- list()
  for(i in 1:reps){
    simul.coords <- SimCoordsLarge(x, sim.outp.path, type = sim.type, 
                                   method = sim.method, 
                                   extent = sim.extent, model = sim.model)
    simul.sampb <- SamplingBiasLarge(simul.coords, outp.path = samp.outp.path, ...)
    eva <- BiasEval(simul.coords, simul.sampb)
    out[[i]] <- eva
  }
  class(out) <- c("samp.bias.list", class(out))
  return(out)
  
}
  