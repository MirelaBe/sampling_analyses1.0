---
title: "Evaluation of Sampling Bias and Cleaning of Erroneous Coordinates in Biological Collections"
author: "Alexander Zizka"
date: '2015-09-04'
output:
  html_document:
    toc: yes
  pdf_document:
    toc: yes
    toc_depth: 2
header-includes: \usepackage{graphicx}
vignette: |
  %\VignetteIndexEntry{Vignette Title} %\VignetteEngine{knitr::rmarkdown} %\VignetteEncoding{UTF-8}
---


\newpage

#Introduction
Species occurrence records are essential in studies of ecology, evolution and biogeography. More and more of these data collected by explorers, researches over centuries are now easily available from public aggregators such as GBIF or eBird. These data are heavily used in a large variety of studies. However these, and virtually all datasets of biological collections are geographically biased by sampling efford. This is generally acknowledge and to some point inevetible but the issues is rarely specifically addressed or discussed. The sampbias package implements the empirical approach presented by Zizka & Antonlli (2016) to evaluate **Geographic Sampling Bias** and give an estimate of the magnitude of its effect. Additionally, the package offers tests to flag **Erroneous Coordinates**, an issue particularly problematic in large datasets compiled from multiple sources.

1) **Erroneous coordinates** are coordinates that for some reason are wrong or do not reflect the precision expected. They are particularly problematic in large datasets compiled from different sources, where manual data curation is not feasible, or when manual data curation is unreproducable. For example, common problems are swapped latitude and longitude or records that get coordinates assigned to rpovince or country centroids without anotation.The `CleanCoordinates` function provides an easy-to-use, reproducible method to test for common reasons for erroneous coordinates, including: numeric coordinate validity, zero coordinates, assignment to capital coordinates, assignment to country or province centroids, sea coordinates, coordinates from urban areas, agreement between coordinates and country assignment, outlier coordinates in a species and the false assignement to the GBIF headquarters.

2) **Geographic sampling bias** is inherit to almost all biological collection datasets, and is due to differences in sampling effort between locations. While this is generally accepted, sampling bias is rarely explicitly addressed. The sampbias package offers a quick and user-friendly way to empirically evaluate geographic sampling bias and summarize it in the B-index. The `SamplingBias` and `BiasEval` functions calculate B value as presented by Zizka & Antonlli (2016). B is a empirical value for the evaluation of severity of geographic sampling bias in a dataset by comparison to simulated data. It is based on seven tests of common reasons for sampling bias and the comparison to randomly sampled coordinates in the same area. The bias evaluation includes: minimum distance to a city, minimum distance to an airport, minimum distance to a road, minimum distance to the coastline, minimum distance to a water body, fraction of species collected in protected areas, fraction of species collected per country and socio-economic indicators (country specific GDP, GERD, Mean Peace index, mean number of Herbaria, mean number of relevant scientific publications publication)

This vignette gives examples for a standard work-flow to and explains the most important options for the major functions of the package. 

\newpage
This section gives an example for a complete workflow to automatically clean geographic coordinates (here they are excluded) and evaluate sampling bias in the dataset. The next sections include more detailed information. We use plant occurrences in West Africa provided with the package as example data.

#In a nutshell


```r
library(sampbias)
data(exampledata)

clean <- CleanCoordinates(exampledata[, 2:3], species = exampledata[, 1], verbose = FALSE)
cleandata <- exampledata[clean$summary, ]

bias.dat <- SamplingBias(cleandata[, 2:3], verbose = FALSE)
test.sim <- SimCoords(cleandata[, 2:3])
bias.sim <- SamplingBias(test.sim, verbose = FALSE)
## BiasPlot(bias.dat, bias.sim, plottype = "density") #plots not shown, see section 3)

eva <- BiasEval(bias.dat, bias.sim)
plot(eva)
```



\begin{center}\includegraphics{Clean-Coordinates-and-Sampling-Bias_files/figure-latex/unnamed-chunk-1-1} \end{center}

\newpage

#Automatic Cleaning of Geographic Coordinates

Invalid or erroneous geographic coordinates are a common problem in datasets of biological collections, especially when they are compiled from different source. From a certain dataset size a manual curation becomes increasingly difficult. The `CleanCoordinates` function identifies common sources of erroneous coordinates and flags the respective records.  Flagged records can then be further investigated or discarded. The tests include: formal coordinate validity, zeros, capitals, centroids, seas, urban, countrycheck, outliers, GBIF (see `?CleanCoordinates` for more information).

##1. Input
The specific test can be switch on and off using the function arguments. `CleanCoordinates` need a `matrix` or `data.frame` of longitude and latitude coordinates, an optional third column can include country information if countrycheck test should be done. With the additional `species` argument the species name for each record can be provided as a vector in the same order as the coordinates to test for outlier coordinates. See `data(exampledata)` for an example. The results can be visualized using `plot()`.


```r
test <- CleanCoordinates(exampledata[,2:4], species = exampledata[,1])
## running validity test
## running zero coordinate test
## flagged 6 records 
## running capitals test
## flagged 0 records 
## running centroids test
## flagged 6 records 
## running seas test
## flagged 5 records 
## running urban test
## flagged 0 records 
## running countrycheck test
## flagged 9 records 
## running outliers test
## flagged 1 records 
## running GBIF test
## flagged 0 records 
## flagged 21 of 31 records, EQ = 0.68
plot(test, xlim = c(-10, 1), ylim = c(-2, 16))
```



\begin{center}\includegraphics{Clean-Coordinates-and-Sampling-Bias_files/figure-latex/unnamed-chunk-2-1} \end{center}

##2. Output
The standard value returned by `CleanCoordinates` is a `data.frame` with one column per test and "FALSE" flagging potentially erroneous coordinates. Alternatively, if `output = 'summary'` a single vector in the same order as the input data is returned with FALSE if at least one test flagged the respective record:


```r
head(test)
##   longitude latitude validity zeros capitals centroids  sea urban
## 1 -2.662170 13.63868     TRUE  TRUE     TRUE      TRUE TRUE  TRUE
## 2 -0.600000 14.03333     TRUE  TRUE     TRUE      TRUE TRUE  TRUE
## 3  2.460222 11.58056     TRUE  TRUE     TRUE      TRUE TRUE  TRUE
## 4 -7.010000  7.30000     TRUE  TRUE     TRUE      TRUE TRUE  TRUE
## 5  1.434264 12.02712     TRUE  TRUE     TRUE      TRUE TRUE  TRUE
## 6 -4.430000  8.36000     TRUE  TRUE     TRUE      TRUE TRUE  TRUE
##   countrycheck outliers gbif summary
## 1         TRUE     TRUE TRUE    TRUE
## 2         TRUE     TRUE TRUE    TRUE
## 3        FALSE     TRUE TRUE   FALSE
## 4        FALSE     TRUE TRUE   FALSE
## 5         TRUE     TRUE TRUE    TRUE
## 6        FALSE     TRUE TRUE   FALSE

test.sum <- CleanCoordinates(exampledata[,2:4], species = exampledata[,1], 
                             output = 'summary', verbose = F)
head(test.sum)
## [1]  TRUE  TRUE FALSE FALSE  TRUE FALSE

clean <- exampledata[test.sum,]
```


If `verbose = TRUE` progress and results of the testing are printed to the screen, EQ indicates the fraction of records flagged as potentially erroneous, and can be seen as a measure of reliability of the data. Individual tests can be included or excluded using the respective logical argument. Here for example we only run the tests for capital coordinates, centroid coordinates and coordinates assigned in the ocean.


```r
test.red <- CleanCoordinates(exampledata[,2:4], species = exampledata[,1], validity = F,
                             zeros = F, capitals = T, centroids = T, seas = T, 
                             urban = F, countrycheck = F, outliers = F, GBIF = F)
## running capitals test
## flagged 0 records 
## running centroids test
## flagged 6 records 
## running seas test
## flagged 5 records 
## flagged 11 of 31 records, EQ = 0.35
head(test.red)
##   longitude latitude capitals centroids  sea summary
## 1 -2.662170 13.63868     TRUE      TRUE TRUE    TRUE
## 2 -0.600000 14.03333     TRUE      TRUE TRUE    TRUE
## 3  2.460222 11.58056     TRUE      TRUE TRUE    TRUE
## 4 -7.010000  7.30000     TRUE      TRUE TRUE    TRUE
## 5  1.434264 12.02712     TRUE      TRUE TRUE    TRUE
## 6 -4.430000  8.36000     TRUE      TRUE TRUE    TRUE
```


Further arguments can be used to control the radius around capital and country centroids, to change the mode and sensibility of the outlier test, to set the method to calculate spheric distances or to provide alternative reference datasets for each test.

Arguments of the plot function can be used to customize the plot and to control the level of detail or to provide a custom background map.


```r
plot(test, xlim = c(-10, 1), ylim = c(-2, 16))
plot(test, clean = FALSE, details = TRUE, xlim = c(-10, 1), ylim = c(-2, 16))
plot(test, clean = FALSE, details = FALSE, xlim = c(-10, 1), ylim = c(-2, 16))
plot(test, bgmap = countryborders, clean = FALSE, details = FALSE, 
     xlim = c(-10, 1), ylim = c(-2, 16))
```



\begin{center}\includegraphics{Clean-Coordinates-and-Sampling-Bias_files/figure-latex/unnamed-chunk-5-1} \end{center}


##3. Large datasets
For datasets with more than 20,000 records the `CleanCoordinatesLarge()` function is recommended. This is a wrapper around `CleanCoordinates` which cuts the datasets in smaller chunks, processes them and the reassembles them to a return value simialr to `CleanCoordinates`. The arguments are identical to `CleanCoordinates`. An exception is outp.path, which is a character string with the complete path to a file on the hard drive. If set, the function will use this file to store results for the individual chunks and thereby circumvent memory limitations. This is useful for very large datasets (e.g. > 500,000 records).


```r
data(exampledata)

test.large <- CleanCoordinatesLarge(x = exampledata_large[,2:3], species = exampledata_large[,1], 
                                    outp.path = NULL, verb = FALSE)
## running validity test
## running zero coordinate test
## flagged 17 records 
## running capitals test
## flagged 0 records 
## running centroids test
## flagged 2 records 
## running seas test
## flagged 0 records 
## running urban test
## flagged 147 records 
## running outliers test
## flagged 745 records 
## running GBIF test
## flagged 0 records 
## flagged 880 of 10011 records, EQ = 0.09 
## running validity test
## running zero coordinate test
## flagged 7 records 
## running capitals test
## flagged 0 records 
## running centroids test
## flagged 3 records 
## running seas test
## flagged 0 records 
## running urban test
## flagged 87 records 
## running outliers test
## flagged 334 records 
## running GBIF test
## flagged 0 records 
## flagged 413 of 4989 records, EQ = 0.08
plot(test.large,  xlim = c(-10, 1), ylim = c(-2, 16))
# bb <- CleanCoordinatesLarge(x = exampledata[,2:4], 
#                             species = exampledata[,1], outp.path = "testest2.txt")
```



\begin{center}\includegraphics{Clean-Coordinates-and-Sampling-Bias_files/figure-latex/unnamed-chunk-6-1} \end{center}


# Geographic Sampling Bias
Geographic sampling bias is an issue in virtually all biological datasets and can have important influence on the conclusions drawn, especially in macro-ecological studies or when investigating ecological niche characteristics. However, it is hardly ever quantified. This package implements an empirical quantification of sampling bias by testing the influence of common bias factors on the dataset and comparring it to randomly sampled records from the same area (the B-index). This can be done very easily with only three steps: 1. calculating sampling bias co-variants (`SamplingBias()`), 2. creating a similar sized dataset with randomly simulated coordinates in the same extent (`SimCoords()`) and 3. comparing and visualizing the results (`BiasEval()`, `BiasPlot()`). The following examples go through these steps. See the documentation of the functions for additional information.


##1. Calculating Sampling Bias Covariants
The `SamplingBias()` function calculates a set of bias measures that have been shown to be common causes of bias in biological collection data. These are for each record: minimum distance to a city, minimum distance to an airport, minimum distance to a road, minimum distance to the coastline, minimum distance to a water body, fraction of species collected in protected areas, fraction of species collected in each country and socio-economic indicators (country specific GDP, GERD, Mean Peace index, mean number of Herbaria, mean number of relevant scientific publications publication). The input dataset can be a `data.frame` or `matrix` with longitude and latitude for each record. The results are  calculated from a set of global gazetteers that are provided with the package. Customized reference datasets for all tests can be provided via the 'ref' arguments of the function (in the same format as the default). Additional arguments allow to set the methods used to calculate distances and to tweak performance issues. Not all standard test will be adequate for all datasets, so each test can be switched on and off via the "t..." arguments of the function. The test for the fraction of records collected from protected areas is switched off by default, as freely distributable reference data is not available.  Reference data for this test uses the format of \url{http://www.protectedplanet.net/} and can be downloaded from there. The results of `SamplingBias` can be visualized using a plotting generic.



```r
bias.dat <- SamplingBias(cleandata[, 2:3], troads = FALSE, 
                         tcoastlines = FALSE, verbose = FALSE) 
plot(bias.dat, plottype = "density")
```



\begin{center}\includegraphics{Clean-Coordinates-and-Sampling-Bias_files/figure-latex/unnamed-chunk-7-1} \end{center}


##2. Creating a Reference Dataset with Randomly Sampled Coordinates
The `SimCoords()` function simulates a similar number of geographic coordinates (WGS84 lon/lat) in the same extent as the input data and provides a set of options on how to do so (see `?SimCoords` for details). As default the function simulates the same number of coordinates in a convex hull polygon around the input data. The `method` argument allows to change this and coordinates can instead be sampled in a rectangle around the input data, in a user provided polygon or in the intersection of a user defined polygon and the convex hull around the input data. A user defined polygon is for example a good option if the species are limited to certain biomes. The `model` argument defines if a planar sampling model (for regional data) or a geosphere model shall be applied. The logical `terrestrial` argument defines if only coordinates on land should be sampled. Subsequently we use the `SamplingBias` function on the simulated data just as on the input data.



```r
test.sim <- SimCoords(cleandata[, 2:3])
bias.sim <- SamplingBias(test.sim, troads = FALSE, tcoastlines = FALSE, verbose = FALSE)
```


##3. Visualization of Sampling Bias
We can use the `BiasPlot()`function to get a visual impression of the effect of individual bias factors and to visualize the differences between the input data and the simulated (ideal) random sampling. The distance test calculated for each records a shown as density plots or histograms, socio-economic indices as box plot or bar plots.


```r
BiasPlot(bias.dat, bias.sim, plottype = "density")
```



\begin{center}\includegraphics{Clean-Coordinates-and-Sampling-Bias_files/figure-latex/unnamed-chunk-9-1} \end{center}

##4. Evaluation of Sampling bias
For an quantification of the total sampling bias in the dataset and the influence of each biasing factor we use the `BiasEval()` function to calculate partial and global Bs. B is the Empirical Bias index  calculated for each bias factor as: 

$$B = \frac{mean(t_{dat}) - mean(t_{sim})}{mean(t_{sim})}$$


```r
par(mfrow = c(1,1), mar = c(6,4,4,4))
eva <- BiasEval(bias.dat, bias.sim)
plot(eva)
eva
## $global.B
## [1] 0.2173118
## 
## $details
##            data simulated partial.B
## 1         225.0     233.0     -0.03
## 2          50.0      58.0     -0.14
## 3         105.2      47.4      1.22
## 4          66.0      47.0      0.40
## 5          34.0      44.0     -0.23
## 6       42940.0   59021.0     -0.27
## 7           3.0       3.0      0.00
## 8           0.0       0.0       NaN
## 9          29.0      28.0      0.04
## 10       2011.0    2029.0     -0.01
## 11        265.0     279.0     -0.05
## 12          1.0       1.0      0.00
## country      NA        NA      0.32
## 
## attr(,"class")
## [1] "sp.bias.eval" "list"
```



\begin{center}\includegraphics{Clean-Coordinates-and-Sampling-Bias_files/figure-latex/unnamed-chunk-10-1} \end{center}

#References
Maldonado C, Molina CI, Zizka A, Persson C, Taylor CM, Alban J, Chilquillo E, Ronsted N, Antonelli A (2015) Estimating species diversity in the era of big data: to what extent can we trust public databases? *Global Ecol Biogeogr* **24**, 8: 973--984.

Yang W, Ma K, Kreft H (2014) Environmental and socio-economic factors shaping the geography of floristic collections in China. *Global Ecology and Biogeography* **23**, 1284--1292.

Meyer C, Kreft H, Guralnick R, Jetz W (2015) Global Priorities for an effective information basis of biodiveristy distributions. *PeerJPreprints*.

Yang W, Ma, K, Kreft H (2013) Geographical sampling in a large distributional database and its effects on species richness-environment models. *J Biogeogr* **40**, 1415--1426.




