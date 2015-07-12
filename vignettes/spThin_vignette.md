---
title: "spThin example"
author: "Matthew E. Aiello-Lammens"
date: "July 25, 2014"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteEngine{knitr::knitr}
  %\VignetteIndexEntry{spThin example}
---

# Introduction

This vignette goes through the spatial thinning example presented in
"spThin: An R package for spatial thinning of species occurrence records
for use in ecological niche models". Here we demonstrate how `spThin` can
be used to spatially thin species occurence records, we test how many
repetitions of the thinning algorithm are necessary to achieve the optimal
number of thinned records for a dataset previously thinned "by hand", and 
we examine whether there is a notable increase in efficiency if an occurence
dataset is thinned as multiple smaller groups of occurrences, 
rather than a single large set of occurrences.

# Load the `spThin` R package

Here we load the R package from source code. This source code will soon be
submitted to CRAN, so that this package can be loaded using standard 
package management methods


```r
## Install package from source, then load package into workspace
install.packages( type = "source", pkgs = "spThin_0.1.0.tar.gz", repos = NULL )
```

```
## Warning: running command '"C:/R/R-3.2.1/bin/x64/R" CMD INSTALL -l "C:\R
## \R-3.2.1\library" "spThin_0.1.0.tar.gz"' had status 1
```

```
## Warning in install.packages(type = "source", pkgs =
## "spThin_0.1.0.tar.gz", : installation of package 'spThin_0.1.0.tar.gz' had
## non-zero exit status
```

```r
library( spThin )
```

# Example dataset

To demonstrate the use of `spThin` we used a set of 201 verified, georeferenced
occurrence records for the Caribbean spiny pocket mouse *Heteromys anomalus*. 
These occurrences are from Columbia, Venezuela, and
three Caribbean islands: Trinidad, Tobago, and Margarita. This dataset
is included as part of the `spThin` package.

#### Load *H. anomalus* dataset



```r
data( Heteromys_anomalus_South_America )
head( Heteromys_anomalus_South_America )
```

```
##       SPEC       LAT      LONG   REGION
## 1 anomalus  7.883333 -75.20000 mainland
## 2 anomalus  8.000000 -76.73333 mainland
## 3 anomalus 10.616667 -75.03333 mainland
## 4 anomalus  8.633333 -74.06667 mainland
## 5 anomalus  9.966667 -75.06667 mainland
## 6 anomalus 10.216667 -73.38333 mainland
```

Here we load and examine the dataset. The name assigned to this dataset
is `Heteromys_anomalus_South_America`.
Note that this dataset includes a column indicating which REGION the 
occurrences was collected. Regions here refer to either the mainland or
three islands in which an occurrence was collected. We can see that
there are many more occurrences collected for the mainland than for
the three islands. Note that Trinidad has been shortened to 'trin'
an Margarita has been shortened to 'mar'.


```r
table( Heteromys_anomalus_South_America$REGION )
```

```
## 
## mainland      mar   tobago     trin 
##      174        2        4       21
```

# Run `spThin::thin` on the full dataset

`thin` involves multiple settings. This allows for extensive 
flexibility in how the user spatially thins a dataset.
However, many have
default values. See `?thin` for further information.













