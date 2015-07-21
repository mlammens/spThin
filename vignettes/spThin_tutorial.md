---
title: "spThin Tutorial"
author: "Matthew E. Aiello-Lammens & Jeffrey O. Hanson"
date: "2015-07-16"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteEngine{knitr::knitr}
  %\VignetteIndexEntry{spThin}
---

# Introduction

The `spThin` package contains functions to spatially thin and rarefy occurrence records. Here we provide a tutorial on this package can be used to process datasets for environmental niche modelling.

## Install and load the `spThin` R package

First we will install the R package from source and load it into the workspace.


```r
## install package from source
# windows systems require Rtool to compile source files: 
# https://cran.r-project.org/bin/windows/Rtools/
if (!require(devtools))
	install.packages('devtools')
devtools:::install_github('mlammens/spThin')

## load package into workspace
library(spThin)
```

## Example dataset

To demonstrate the use of `spThin` we will use a dataset containing 201 verified, georeferenced occurrence records for the Caribbean spiny pocket mouse (*Heteromys anomalus*). These records are from Columbia, Venezuela, and three Caribbean islands: Trinidad, Tobago, and Margarita. This dataset is included as part of the `spThin` package.


```r
# load the dataset
data(Heteromys_anomalus_South_America)

# inspect first six rows
head(Heteromys_anomalus_South_America)
```

The dataset is stored in the `data.frame` `Heteromys_anomalus_South_America`. Each row represents a different record. The `LONG` and `LAT` columns contain the longitude and latitude of each record. The `REGION` column indicates the location where the record was collected, either in the mainland (`mainland`), Trinidad (`trin`), Tobago (`tobago`), and Margarita (`mar`).

```r
# number of records in each region
table(Heteromys_anomalus_South_America$REGION)
```

# Thin data

Thinning data is an optimisation problem. The objective is to identify the maximum number of points that are at least a minimum distance apart. The `spThin` function can be used to thin data. This function can use several optimisation routines to acheive this. It can use exact algorithms, using the LpSolve and Gurobi programs, and it can also use a stingy heuristic algorithm. 


## lp_solve

First, we will use [lp_solve](http://lpsolve.sourceforge.net/) to optimally thin the (*Heteromys anomalus*) dataset. Here, we will thin the records such that each record is at least 10Km apart. We also will use great circle distances to compute the distance between records because the coordinates' are in longitudes and latitudes (aka WGS1984). 

To optimally thin the entire dataset, lp_solve can take up 10-20 minutes. Here, we will thin the dataset but allow lp_solve 10 seconds to find the best solution that it can. This solution is unlikely to be optimal.


```r
# thin records using lp_solve
thin1 <- spThin(
	Heteromys_anomalus_South_America, 
	lon.col = "LONG", 
	lat.col = "LAT",
	mindist = 100000,
	method="lpsolve",
	great.circle.distance=TRUE,
	timeout=10
)

# summary of thinned dataset
summary(thin1)
```

To visualise which records were retained, we can plot `thin1`. The hexagons show the distribution of all the occurrence records and the red points show the location of the retained records. 


```r
plot(thin1)
```

## Gurobi

The LpSolve program is a freely available software (LGPL 2), however it cannot thin large datasets in a feasible amount of time. To optimally thin large datasets quickly, the `spThin` function can also use the Gurobi software package. [Gurobi](http://www.gurobi.com) is a commercial software package. Academics can obtain a [special license](http://www.gurobi.com/academia/academia-center) for no cost. Following the installation of Gurobi, the `gurobi` R package must also be [installed](http://www.gurobi.com/documentation/6.0/quickstart_windows/r_installing_the_r_package).

Here, we will attempt to optimally thin the dataset using Gurobi. If Gurobi is not fully installed on your system this will return an error.


```r
# thin records using gurobi
thin2 <- spThin(
	Heteromys_anomalus_South_America, 
	lon.col = "LONG", 
	lat.col = "LAT",
	mindist = 100000,
	method="gurobi",
	great.circle.distance=TRUE
)

# summary of thinned dataset
summary(thin2)

# plot thinned dataset
plot(thin2)
```

## Stingy heuristic algorithm

The `spThin` function can also use a stingy heuristic algorithm to thin data. Unlike LpSolve and Gurobi, this routine does not guaranteeably identify the optimal thinned dataset. This means that the algorithm can thin a dataset, but we cannot not know if it is the optimal solution. On the other hand, an advantage of the `heuristic` routine is that it can produce multiple near-optimal thinned datasets. These may be useful for subdividing data for bootstrap replicates.

This algorithm starts with all the records and sequentially removes them. At each step, the number of neighbours within a specified distance is computed for each record. One of the remaining records is then sampled for removal, weighted by the number of neighbours. This process is repeated until all the remaining records are at least a minimum distance apart

 To maximise our chances of identifying the optimal thinned dataset, we can run this algorithm multiple times. Here we will generate `100` replicates.


```r
# thin records using heuristic
thin3 <- spThin(
	Heteromys_anomalus_South_America, 
	lon.col = "LONG", 
	lat.col = "LAT",
	mindist = 100000,
	method="heuristic",
	nrep=100,
	great.circle.distance=TRUE
)

# summary of thinned dataset
summary(thin3)
```

We can use the `plot` method to investigate effectiveness of the heuristic algorithm. 


```r
plot(thin3)
```

The first plot shows the distribution of records and those selected in the best solution. The cumulative maximum records retained versus number of repetitions shows the number of different records retained as increase the number of replicates. If the plot shows a flat line, this indicates that all replicates yielded the same thinned dataset and that a single repetition would have sufficed. If the plot shows an increasing curve, this means that the each of the replicate datasets have different records and we have little evidence to suggest that any of the replicates is optimal. Whereas, if the plot shows a curve with an asymptote, this suggests that replicates have converge on a solution (though we still cannot say with certainty that it is optimal). 

# Rarefy data
Spatial rarefication is another process for reducing bias. This process works by overlaying a grid over the observation records and randomly selecting a single record in each grid cell. Generally, this method can be used on much larger datasets than spatial thinning.

First, we will turn the `data.frame` to a `SpatialPointsDataFrame`.


```r
Heteromys_anomalus_South_America_sp <- SpatialPointsDataFrame(
	coords=as.matrix(Heteromys_anomalus_South_America[,c("LONG", "LAT")]),
	data=Heteromys_anomalus_South_America,
	proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_def')
)
```

Next, we will reproject it to the PSAD56 / ICN Regional (EPSG:2317) coordinate system. This will allow us to specify the size of the grid cells in metres.


```r
Heteromys_anomalus_South_America_sp <- spTransform(
	Heteromys_anomalus_South_America_sp,
	CRS('+proj=lcc +lat_1=9 +lat_2=3 +lat_0=6
		 +lon_0=-66 +x_0=1000000 +y_0=1000000 
		 +ellps=intl +towgs84=-288,175,-376,0,0,0,0 +units=m +no_defs')
)
```

Now, we can spatially rarefy the dataset. Here, we will generate 100 replicates using 180km grid cells.


```r
# rarefy data
rarefy1 <- spRarefy(
	Heteromys_anomalus_South_America_sp, 
	grid = 180000,
	nrep = 100
)

# show summary for rarefied data
summary(rarefy1)

## plot 1st rarefied replicate 
plot(rarefy1, 1)
```

# Manipulate processed data
Thinned or rarefied datasets using the `[[` method. The thinned or rarefied datasets are stored as `SpatialPoints` or `SpatialPointsDataFrame` objects. Here, we will retrieve the first rarefied replicate and store it in an object.


```r
# access first replicate
firstrep <- rarefy1[[1]]

# show structure
str(firstrep)
```

# Save processed data
Finally, the thinned and rarefied datasets can be saved using the `write` method. Here, we will saved the rarefied datasets to a temporary directory.


```r
# print temporary dir
print(tmpdir())

# write replicate datasets to file
write(
	rarefy1,
	coords=FALSE,
	dir=tempdir()
)
```
