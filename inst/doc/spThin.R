## ----eval=FALSE----------------------------------------------------------
#  ## install package from source
#  # windows systems require Rtool to compile source files:
#  # https://cran.r-project.org/bin/windows/Rtools/
#  if (!require(devtools))
#  	install.packages('devtools')
#  devtools:::install_github('mlammens/spThin')
#  
#  ## load package into workspace
#  library(spThin)

## ----eval=FALSE----------------------------------------------------------
#  # load the dataset
#  data(Heteromys_anomalus_South_America)
#  
#  # inspect first six rows
#  head(Heteromys_anomalus_South_America)

## ----eval=FALSE----------------------------------------------------------
#  # number of records in each region
#  table(Heteromys_anomalus_South_America$REGION)

## ----eval=FALSE----------------------------------------------------------
#  # thin records using lp_solve
#  thin1 <- spThin(
#  	Heteromys_anomalus_South_America,
#  	x.col = "LONG",
#  	y.col = "LAT",
#  	mindist = 100000,
#  	method="lpsolve",
#  	great.circle.distance=TRUE,
#  	timeout=10
#  )
#  
#  # summary of thinned dataset
#  summary(thin1)

## ----eval=FALSE----------------------------------------------------------
#  plot(thin1)

## ----eval=FALSE----------------------------------------------------------
#  # thin records using gurobi
#  thin2 <- spThin(
#  	Heteromys_anomalus_South_America,
#  	x.col = "LONG",
#  	y.col = "LAT",
#  	mindist = 100000,
#  	method="gurobi",
#  	great.circle.distance=TRUE
#  )
#  
#  # summary of thinned dataset
#  summary(thin2)
#  
#  # plot thinned dataset
#  plot(thin2)

## ----eval=FALSE----------------------------------------------------------
#  # thin records using heuristic
#  thin3 <- spThin(
#  	Heteromys_anomalus_South_America,
#  	x.col = "LONG",
#  	y.col = "LAT",
#  	mindist = 100000,
#  	method="heuristic",
#  	nrep=100,
#  	great.circle.distance=TRUE
#  )
#  
#  # summary of thinned dataset
#  summary(thin3)

## ----eval=FALSE----------------------------------------------------------
#  plot(thin3)

## ----eval=FALSE----------------------------------------------------------
#  # load sp package
#  library(sp)
#  
#  # create SpatialPointsDataFrame
#  Heteromys_anomalus_South_America_sp <- SpatialPointsDataFrame(
#  	coords=as.matrix(Heteromys_anomalus_South_America[,c("LONG", "LAT")]),
#  	data=Heteromys_anomalus_South_America,
#  	proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_def')
#  )
#  
#  # show structure for SpatialPointsDataFrame
#  str(Heteromys_anomalus_South_America_sp)

## ----eval=FALSE----------------------------------------------------------
#  Heteromys_anomalus_South_America_sp <- spTransform(
#  	Heteromys_anomalus_South_America_sp,
#  	CRS('+proj=lcc +lat_1=9 +lat_2=3 +lat_0=6
#  		 +lon_0=-66 +x_0=1000000 +y_0=1000000
#  		 +ellps=intl +towgs84=-288,175,-376,0,0,0,0 +units=m +no_defs')
#  )

## ----eval=FALSE----------------------------------------------------------
#  # rarefy data
#  rarefy1 <- spRarefy(
#  	Heteromys_anomalus_South_America_sp,
#  	grid = 180000,
#  	nrep = 100
#  )
#  
#  # show summary for rarefied data
#  summary(rarefy1)
#  
#  ## plot 1st rarefied replicate
#  plot(rarefy1, 1)

## ----eval=FALSE----------------------------------------------------------
#  #### rarefied results
#  # retrieve first rarefied dataset
#  r1 <- rarefy1[[1]]
#  
#  ## show structure for SpatialPointsDataFrame
#  # the @coords slot has the coordinates of the points in a matrix
#  # the @data slot has all the columns in a data.frame
#  str(r1)
#  
#  ## extract information
#  r1.coords <- r1@coords # extract coordinates for solution
#  r1.data <- r1@data # extract data for solution
#  
#  #### thinned results
#  # extract first thinned solution
#  t1 <- thin1[[1]]
#  
#  ## extract properties of thinned data
#  t1.coords <- r1@coords # extract coordinates
#  t1.data <- r1@data # extract data.frame for solution
#  
#  #### make plot comparing both filtered datasets
#  plot(
#  	rbind(r1.coords, t1.coords),
#  	col=c(
#  		rep('red', nrow(r1.coords)),
#  		rep('black', nrow(r1.coords))
#  	),
#  	pch=16
#  )
#  legend(
#  	'topleft',
#  	legend=c('Rarefied records', 'Thinned records'),
#  	col=c('red', 'black')
#  )
#  

## ----eval=FALSE----------------------------------------------------------
#  # print temporary dir
#  print(tempdir())
#  
#  # write thinned datasets to file
#  write.SpThin(
#  	thin2,
#  	coords=FALSE,
#  	dir=tempdir()
#  )
#  
#  # write rarefied datasets to file
#  write.SpRarefy(
#  	rarefy1,
#  	coords=FALSE,
#  	dir=tempdir()
#  )
#  

