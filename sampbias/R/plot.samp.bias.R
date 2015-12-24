plot.samp.bias <- function(x, plottype = c("density", "histogram"), col = rgb(255, 69, 0, 150, maxColorValue = 255), 
                           border = "grey50",  ...) {
  match.arg(plottype)
  x <- Filter(function(z) !all(is.na(z)), x)
    
  reflist <- data.frame(col = c("Cities", "Airports", "Roads", "Coastlines", 
                                "Rivers", "Interpoint.sd", "Protectedarea", 
                                "socioeconomic.Iso3", 
                                "socioeconomic.Publications", 
                                "socioeconomic.Hindex", 
                                "socioeconomic.Peace", 
                                "socioeconomic.GDP", 
                                "socioeconomic.GERD", 
                                "socioeconomic.Herbaria", 
                                "socioeconomic.Publications.area", 
                                "socioeconomic.Herbaria.area"), 
                        titles = c("Distance to next city",
                                   "Distance to next Airport",
                                   "Distance to next Road",
                                   "Distance to next Coastline",
                                   "Distance to next Major Waterbody",
                                   "Distance to next Neighbour",
                                   "Fraction in Protected Area",
                                   "Fraction per country",
                                   "Publications",
                                   "Mean Hindex", "Mean Peace Index",
                                   "GDP", "GERD", "Number of Herbaria", 
                                   "Publications/1000 km2", 
                                   "Number of Herbaria / 1000 skm"), 
                        axeslable = c("[km]", "[km]", "[km]", "[km]", "[km]", "[km]", 
                                      "Fraction", "Fraction", "", "", "", "", "", "", "", ""), 
                        plottype = c(rep("hist", 6), c("bar", "bar"), rep("box", 8)))
  
  if (plottype[1] == "histogram") {
    histo.x <- x[, names(x) %in% reflist[reflist$plottype == "hist", 
                                         "col"]]
    
    for (i in 1:length(histo.x)) {
      hist(histo.x[, names(histo.x)[i]], 
           xlab = reflist[reflist$col == names(histo.x)[i], "axeslable"], 
           main = "", col = rgb(255, 69, 0, 150, maxColorValue = 255), 
           border = border, ...)
      title(reflist[reflist$col == names(histo.x)[i], "titles"])
      box("plot")
      
    }
  }
  
  if (plottype[1] == "density") {
    histo.x <- x[, names(x) %in% reflist[reflist$plottype == "hist", "col"]]
    
    for (i in 1:length(histo.x)) {
      a <- density(histo.x[, names(histo.x)[i]])
      plot(a, xlab = reflist[reflist$col == names(histo.x)[i], 
                             "axeslable"], type = "n", axes = F, main = "", ylab = "")
      polygon(a, col = col, 
              border = border, ...)
      axis(1)
      axis(2, las = 2)
      box("plot")
      title(reflist[reflist$col == names(histo.x)[i], "titles"])
    }
  }
  
  bar.x <- x[, names(x) %in% reflist[reflist$plottype == "bar", "col"]]
  
  if ("Protectedarea" %in% names(bar.x)) {
    protec <- sum(bar.x[, "Protectedarea"])/length(bar.x[, "Protectedarea"])
    barplot(protec, col = col, 
            border = border, ...)
    title(reflist[reflist$col == "Protectedarea", "titles"])
    box("plot")
  }
  
  if ("socioeconomic.Iso3" %in% names(bar.x)) {
    pol <- sort(table(as.character(bar.x[, "socioeconomic.Iso3"])))
    barplot(pol, col = col, 
            border = border, ...)
    title(reflist[reflist$col == "socioeconomic.Iso3", "titles"])
    box("plot")
  }
  
  box.x <- x[, names(x) %in% reflist[reflist$plottype == "box", "col"]]
  
  #histogramms for socioeconomic indicators
  for (i in 1:length(box.x)) {
    hist(box.x[, i], main = "", xlab = "", ylab = "", col = col, 
         border = border, ...)
    title(reflist[reflist$col == names(box.x)[i], "titles"])
    box("plot")
  }
}