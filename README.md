---
title: "Spatial regression questions"
author: "EPS"
output: html_document
---

# Set up

```{r message=F, warning=F}
library(tidyverse)
library(ggExtra)
library(spdplyr)
library(spdep)
library(spatialreg)
library(patchwork)
```

Today we will work with a dataset that merges the EPA's [air quality data](https://www.epa.gov/outdoor-air-quality-data/air-quality-index-report) with Census estimates of population.  We will be building toy regressions with some simple datasets, so be sure to take whatever results you may generate with a grain of salt. 
 sdasd asd
# Q1

* First, load the data (`AQI_POP.RDS` and `county.RDS`).  
* Filter rows so our new datasets *only* includes air quality and population data from `Year == 2003` for the state of `California`.  To make sure you filtered correctly, be sure that `mean(aqi@data$Median.AQI) == 50.62963`.
* Load the county shapefile and filter counties in the state of California.  You may have to look up California's `STATEFP` to do this.
* Append the AQI data to the California county shapefile using the `merge()`.  Make *sure* the result of your `merge()` is a new shapefile for counties in California that now includes attributes stored in the AQI dataset.
* Use `spplot()` to create a map of the `Median.AQI` in each county.  Median AQI is the median value of the air quality index for the year 2003.  High values of AQI are bad, low values are good.

```{r}
aqi <- readRDS("./data/AQI_POP.RDS")
# filter
aqi_CA2003 <- filter(aqi, State == "California" & Year == 2003)
#check
mean(aqi_CA2003$Median.AQI)

cty <- readRDS("./data/county.RDS")
# CA FIPS == 06
# filter
cty_CA <- filter(cty, cty@data$STATEFP == "06")
# merge
cty.aqi_CA2003 <- merge(cty_CA, aqi_CA2003)
# spplot
median.aqi <- spplot(cty.aqi_CA2003, "Median.AQI", main = "Median AQI")
png(file = "C:/Users/Paige/Desktop/geospatial/geospatial_analysis/10_SpatialRegression/median_aqi.png") 
plot(median.aqi)
dev.off()
median.aqi
```
![median_aqi](https://github.com/user-attachments/assets/515b9345-6d59-40a6-9fd2-05b256f54677)

# Q2

Create a new shapefile in which you remove all rows in the merged AQI-county shapefile in which `Median.AQI` is `NA`.  Use this clean shapefile in *all* subsequent analyses.  Use the `ggExtra` package function `ggMarginal()` to create a visualization of your `Median.AQI` and `POP2010` with histograms on each axis and a scatter plot in the center (see tutorial for hints). 

```{r}
# filter
clean_cty.aqi <- filter(cty.aqi_CA2003, is.na(cty.aqi_CA2003$Median.AQI) == FALSE)

# plot
p <- ggplot(data = clean_cty.aqi@data, aes(x = clean_cty.aqi$Median.AQI, y = clean_cty.aqi$POP2010, color = clean_cty.aqi$County)) +
  geom_point() +
  theme(legend.position = "none") +
  xlab("Median AQI") +
  ylab("2010 Population")

ggmarg <- ggMarginal(p, type = "histogram")

png(file = "C:/Users/Paige/Desktop/geospatial/geospatial_analysis/10_SpatialRegression/ggmarg.png") 
plot(ggmarg)
dev.off()

ggmarg
```
![ggmarg](https://github.com/user-attachments/assets/b2383195-84ca-4ab1-b753-8dac34a435c6)

# Q3

Let's estimate a simple OLS regression with `lm()`. We will use the formula `Median.AQI ~ log(POP2010)`.  Population data is not normally distributed, so we will use a log transformation of this dataset to address this.  To read about interpreting coefficients that have been log-transformed, see [this](https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-do-i-interpret-a-regression-model-when-some-variables-are-log-transformed/).

Print the `summary()` of your regression results.  What is the value of the coefficient estimate for `log(POP2010)`?

```{r}
# run OLS, print summary
reg1 <- lm(clean_cty.aqi$Median.AQI ~ log(clean_cty.aqi$POP2010))
summary(reg1)
```

# Q4

Plot the residuals from this regression using `spplot()`.  Based on your knowledge of California, what might explain spatial patterns in the residuals?

```{r}
#print(reg1[2])
clean_cty.aqi$resid <- residuals(reg1)
ols_resid <- spplot(clean_cty.aqi, "resid")

png(file = "C:/Users/Paige/Desktop/geospatial/geospatial_analysis/10_SpatialRegression/ols_resid.png") 
plot(ols_resid)
dev.off()

ols_resid
```
![ols_resid](https://github.com/user-attachments/assets/c00ace5f-6cf0-4a9c-b9dd-73c96d910ba7)

The counties with high residuals have large areas and large populations. They are also highly urbanized, so a lot of air pollutants compared to the rest of the state's rural areas. These few points would be significantly different from the majority if the counties, and that shows up in their high residuals.

# Q5

* Create a queen neighborhood with the filtered AQI-population data you constructed in Q2.
* Create a weights matrix using `nb2lsitw()` with the default row standardized weights (`style=W`).
* Visualize this weight matrix.
* Use `lm.morantest()` to test for spatial autocorrelation in the residuals of the OLS regression you ran in Q3.
* Is there spatial autocorrelation in the residuals?

```{r}
# create queen nb
cty.aqi_queen <- poly2nb(clean_cty.aqi)
coords <- coordinates(clean_cty.aqi)
plot(clean_cty.aqi);plot(cty.aqi_queen, coords, add = T)
```
```{r}
# create weights
cty.aqi_queen.weights <- nb2listw(cty.aqi_queen)
# visualize
hist(unlist(cty.aqi_queen.weights$weights), main = "Weights of AQI values", xlab = "Weights")
# lm.morantest() 
lm.morantest(reg1, cty.aqi_queen.weights)
```

p <<< 0.05 so *yes* AQI data residuals are spatially correlated.

# Q6 

* Run a spatial *error* model using the clean data you created in Q2, the weights object created in Q5, and following formula `formula = Median.AQI ~ log(POP2010)`.
* What is the coefficient estimate for `log(POP2010)`?

```{r}
# run spatial error model
cty_aqi.sem <- errorsarlm(formula = Median.AQI ~ log(POP2010), data = clean_cty.aqi, listw = cty.aqi_queen.weights, quiet = T)
summary(cty_aqi.sem)


```

log(POP2010) estimate: 6.8259

# Q7

* Use `spplot()` to visualize the county-level fitted values from the regression you ran in Q6.

```{r}
clean_cty.aqi$fitted_sem <- cty_aqi.sem$fitted.values
fitted_sem <- spplot(clean_cty.aqi, "fitted_sem", main = "Trend")

png(file = "C:/Users/Paige/Desktop/geospatial/geospatial_analysis/10_SpatialRegression/fitted_sem.png") 
plot(fitted_sem)
dev.off()

fitted_sem
```
![fitted_sem](https://github.com/user-attachments/assets/663c9c3c-b228-44fe-9b86-b8247b68d524)

# Q8

* Use `spplot()` to visualize the county-level residuals from the regression you ran in Q6.  Visually compare this map to the map of `Median.AQI`.

```{r}
clean_cty.aqi$resid_sem <- cty_aqi.sem$residuals
reg.sem <- spplot(clean_cty.aqi, "resid_sem", main = "SEM Residuals")
png(file = "C:/Users/Paige/Desktop/geospatial/geospatial_analysis/10_SpatialRegression/resid_sem.png") 
plot(reg.sem)
dev.off()
par(mfrow=c(2,1)) 
reg.sem;median.aqi
```
![resid_sem](https://github.com/user-attachments/assets/17327a2b-fa9d-48bd-bcc9-fa4e3f265aaf)

Overall the residuals map is darker colored than the raw median map. The scales are not very comparable since they are very offset from each other.

# Q9

* Run a spatial *lag* model using the clean data you created in Q2, the weights object created in Q5, and following formula `formula = Median.AQI ~ log(POP2010)`.  Set `zero.policy = T`.
* What is the coefficient estimate for `log(POP2010)`?

```{r}
cty_aqi.lag <- lagsarlm(formula = Median.AQI ~ log(POP2010), data = clean_cty.aqi, cty.aqi_queen.weights, zero.policy = T)
summary(cty_aqi.lag)
```

log(POP2010) estimate: 5.0543

# Q10

* Use `lm.LMtests` to determine which spatial model best fits the data, formula, and weights object we used in Q6 and Q9.

```{r}
LM <- lm.LMtests(reg1, cty.aqi_queen.weights, test = "all")
LM


```

The spatial error model fits best.
