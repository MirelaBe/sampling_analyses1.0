BiasPlot <- function(x, y, plottype = c("density", "histogram"), 
                     break.num = 50, boxplots = F, 
                     cols = c(rgb(255, 69, 0, 150, maxColorValue = 255), 
                              rgb(32,178, 170, 150, maxColorValue = 255)), 
                     border = c("grey50", "grey50")) {
  
  match.arg(plottype)
  
  y <- Filter(function(z) !all(is.na(z)), y)
  x <- Filter(function(z) !all(is.na(z)), x)
  
  if (!all(names(x) == names(y))) {
    y <- y[, names(y) %in% names(x)]
    warning(sprintf("column names differ, dropped columns %s from y", 
                    names(y)[!names(y) %in% names(x)]))
  }
  
  if (dim(x)[2] != dim(y)[2]) {
    y <- y[, names(y) %in% names(x)]
    warning(sprintf("column numer differs, dropped columns %s from y", 
                    names(y)[!names(y) %in% names(x)]))
  }
  
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
                        titles = c("Distance to next City",
                                   "Distance to next Airport",
                                   "Distance to next Road",
                                   "Distance to next Coastline",
                                   "Distance to next Major Waterbody",
                                   "Distance to next Neighbour", 
                                   "Fraction in Protected Area",
                                   "Fraction per Country", 
                                   "Publications in Org. Biology",
                                   "Hindex", "Peace Index", 
                                   "GDP", "GERD", "Number of Herbaria",
                                   "Publications / 1000 sqkm",
                                   "Number of Herbaria / 1000 sqkm"),
                        axeslable = c("[km]","[km]", "[km]", "[km]", 
                                      "[km]", "[km]", "Fraction", "Fraction", 
                                      "", "", "", "", "", "", "", ""), 
                        plottype = c(rep("hist", 6), c("bar", "bar"), 
                                     rep("box", 8)))
  
  if (plottype[1] == "histogram") {
    histo.x <- x[, names(x) %in% reflist[reflist$plottype == "hist", 
                                         "col"]]
    histo.y <- y[, names(y) %in% reflist[reflist$plottype == "hist", 
                                         "col"]]
    
    for (i in 1:length(histo.x)) {
      a <- hist(histo.x[, names(histo.x)[i]], plot = F)
      b <- hist(histo.y[, names(histo.x)[i]], plot = F)
      
      hist(histo.y[, names(histo.x)[i]], 
           breaks = seq(min(c(a$breaks, b$breaks)), 
                        max(c(a$breaks, b$breaks)), 
                        max(c(a$breaks, b$breaks))/break.num),
           ylim = c(0, max(c(a$counts, b$counts))), 
           xlim = c(0, max(c(a$breaks, b$breaks))), col = cols[2], 
           main = "", ylab = "", axes = F, border = border[2],
           xlab = reflist[reflist$col == names(histo.x)[i], "axeslable"])
      axis(1)
      axis(2, las = 2)
      
      hist(histo.x[, names(histo.x)[i]], add = T, 
           breaks = seq(min(c(a$breaks,b$breaks)), 
                        max(c(a$breaks, b$breaks)), 
                        max(c(a$breaks, b$breaks))/break.num), 
           ylim = c(0, max(c(a$counts, b$counts))), 
           xlim = c(0, max(c(a$breaks, b$breaks))), col = cols[1], 
           border = border[1], axes = F)
      box("plot")
      title(reflist[reflist$col == names(histo.x)[i], "titles"])
      legend("topright", c("simulated", "data"), fill = c(cols[2],cols[1]), 
             bg = "white")
    }
  }
  
  if (plottype[1] == "density") {
    histo.x <- x[, names(x) %in% reflist[reflist$plottype == "hist", 
                                         "col"]]
    histo.y <- y[, names(y) %in% reflist[reflist$plottype == "hist", 
                                         "col"]]
    
    for (i in 1:length(histo.x)) {
      a <- density(histo.x[, names(histo.x)[i]])
      b <- density(histo.y[, names(histo.x)[i]])
      
      plot(a, ylim = c(0, max(c(a$y, b$y)) + max(c(a$y, b$y))/10), 
           xlim = c(0, max(c(a$x, b$x)) + max(c(a$x, b$x))/10), axes = F, 
           xlab = reflist[reflist$col == names(histo.x)[i], "axeslable"],
           ylab = "",
           type = "n", main = "")
      
      polygon(b, col = cols[2], border = border[1])
      polygon(a, col = cols[1], border = border[2])
      axis(1)
      axis(2, las = 2)
      box("plot")
      title(reflist[reflist$col == names(histo.x)[i], "titles"])
      legend("topright", c("simulated", "data"), fill = c(cols[2], cols[1]), 
             bg = "white")
    }
  }
  
  bar.x <- x[, names(x) %in% reflist[reflist$plottype == "bar", "col"]]
  bar.y <- y[, names(y) %in% reflist[reflist$plottype == "bar", "col"]]
  
  if ("Protectedarea" %in% names(bar.x)) {
    protec <- matrix(c(sum(bar.y[, "Protectedarea"])/
                         length(bar.y[,"Protectedarea"]), 
                       sum(bar.x[, "Protectedarea"])/length(bar.x[, "Protectedarea"])), ncol = 2)
    
    barplot(t(protec), beside = T, ylab = "", las = 2,
            col = c(cols[2],cols[1]), 
            ylim = c(0, max(protec) + max(protec)/10), border = border)
    title(reflist[reflist$col == "Protectedarea", "titles"])
    legend("topright", c("simulated", "data"), fill = c(cols[2], cols[1]), bg = "white")
    box("plot")
  }
  if ("socioeconomic.iso3" %in% names(bar.x)) {
    pol <- merge(data.frame(t(table(as.character(bar.y[, "socioeconomic.Iso3"])))), 
                 data.frame(t(table(as.character(bar.x[, "socioeconomic.Iso3"])))), 
                 by = "Var2", all = T)
    pol <- pol[, c("Var2", "Freq.x", "Freq.y")]
    pol[is.na(pol)] <- 0
    rownames(pol) <- pol$Var2
    pol <- pol[, -1]
    pol <- pol[order(rowSums(pol)), ]
    barplot(t(pol), beside = T, ylab = "", las = 2, 
            col = c(cols[2],cols[1]), ylim = c(0, max(pol) + max(pol)/10), 
            border = c(cols[3], cols[4]))
    title(reflist[reflist$col == "socioeconomic.Iso3", "titles"])
    legend("topleft", c("simulated", "data"), fill = c(cols[2], cols[1]), bg = "white")
    box("plot")
  }
  
  box.x <- x[, names(x) %in% reflist[reflist$plottype == "box", "col"]]
  box.y <- y[, names(y) %in% reflist[reflist$plottype == "box", "col"]]
  
  if (boxplots == T) {
    box <- rbind(box.x, box.y)
    box$ds <- c(rep("data", dim(box.x)[1]), rep("asim", dim(box.y)[1]))
    
    for (i in 1:length(box.x)) {
      boxplot(as.numeric(as.character(box[, i])) ~ box$ds, 
              col = c(cols[2],cols[1]), 
              names = c("Simulation", "Data"))
      title(reflist[reflist$col == names(box.x)[i], "titles"])
    }
  } else {
    for (i in 1:length(box.x)) {
      dat <- matrix(c(mean(as.numeric(as.character(box.y[, names(box.x)[i]])), 
                           na.rm = T), 
                      mean(as.numeric(as.character(box.x[, names(box.x)[i]])),
                           na.rm = T)), ncol = 2)
      barplot(t(dat), beside = T, ylab = "", las = 2, 
              col = c(cols[2], cols[1]), 
              ylim = c(0, max(dat) + max(dat)/10), border = border)
      title(reflist[reflist$col == names(box.x)[i], "titles"])
      legend("topright", c("simulated", "data"), 
             fill = c(cols[2],cols[1]), bg = "white")
      box("plot")
    }
  }
  
} 