#What is sampbias?

An R package for data cleaning an assessment of sampling bias in biological collcetion databases.

#Description

Empirical tests to clean erroneous coordinates and to measure sampling bias (B-index) in biological collection datasets. The tests detect the most common errors and biases in collection data. Cleaning tests include: numeric coordinate validity, zero coordinates, assignment to capital coordinates, assignment to country or province centroids, sea coordinates, coordinates from urban areas, agreement between coordinates and country assignment, outlier coordinates per species, GBIF headquarters. Bias evaluation includes: minimum distance to a city, minimum distance to an airport, minimum distance to a road, minimum distance to the coastline, minimum distance to a waterbody, fraction of species collected in protected areas, fraction of species collected in each country and socioeconomic indicators (country specific GDP, GERD, mean peace index, number of herbaria, number of relevant scientific publications). Includes plotting generics to visualize partial Bs and to compare values to simulated data. Additionally includes wrapper functions for large scale data sets and a vignette with an example work-flow.


#Examples

Example datasets and demo scripts for the major functionalities are provided in the examples folder. A vignette with further exmplanations is available in the vignettes folder.

#For the impatient

```R
data(exampledata)

clean <- CleanCoordinates(exampledata[, 2:4], species = exampledata[, 1], verbose = FALSE)

cleandata <- exampledata[clean$summary, ]

bias.dat <- SamplingBias(cleandata[, 2:3], troads = FALSE, tcoastlines = FALSE, verbose = FALSE)
test.sim <- SimCoords(cleandata[, 2:3])
bias.sim <- SamplingBias(test.sim, troads = FALSE, tcoastlines = FALSE, verbose = FALSE)

BiasPlot(bias.dat, bias.sim)

eva <- BiasEval(bias.dat, bias.sim)
par(mar = c(6,4,4,4))
plot(eva)
```

#Citation
