
#' Extract samples from \code{SpRarefy} and \code{SpThin} object
#'
#' This function extracts samples from a \code{SpRarefy} or a \code{SpThin} object.
#'
#' @param x \code{SpThin} or \code{SpRarefy} object.
#' @param r \code{numeric} n'th replicate to extract samples from. Zero can be used to extract samples using the best replicate.
#' @param ... not used.
#' @return \code{SpatialPoints} or  \code{SpatialPointsDataFrame} object
#' @seealso \code{\link{SpThin}}.
#' @examples
#' # make simulated dataset
#' testDF <- data.frame(
#'		x=runif(100, -5, -5),
#'		y=runif(100, -5, -5)
#' )
#'
#' # make thinned dataset
#' result1 <- spThin(
#'		testDF,
#'		"x",
#'		"y",
#'		dist=5,
#'		method='heuristic',
#'		nrep=10
#' )
#'
#' # extract first replicate dataset in result1
#' result1[[1]]
#'
#' # make rarefied dataset 
#' result2 <- spRarefy(
#'		testDF,
#'		"x",
#'		"y",
#'		grid=5,
#'		nrep=10
#' )
#'
#' # extract first replicate dataset in result2
#' result2[[1]]
#'
#' @name extract
NULL

#' Number replicates in \code{SpRarefy} and \code{SpThin} object
#'
#' This function returns the number of replicates contained in a \code{SpRarefy} or a \code{SpThin} object.
#'
#' @param x \code{SpThin} object.
#' @param ... not used.
#' @return \code{integer} number of replicates.
#' @seealso \code{\link{SpThin}}.
#' @examples
#' # make simulated dataset
#' testDF <- data.frame(
#'		x=runif(100, -5, -5),
#'		y=runif(100, -5, -5)
#' )
#'
#' # make thinned dataset
#' result1 <- spThin(
#'		testDF,
#'		"x",
#'		"y",
#'		dist=5,
#'		method='heuristic',
#'		nrep=10
#' )
#'
#' # show number replicates in result1
#' nrep(result1)
#'
#' # make rarefied dataset 
#' result2 <- spRarefy(
#'		testDF,
#'		"x",
#'		"y",
#'		grid=5,
#'		nrep=10
#' )
#'
#' # show number replicates in result2
#' nrep(result2)
#'
#' @export
nrep<-function(x, ...) {UseMethod("nrep")}

#' Full dataset contained in a \code{SpThin} and \code{SpRarefy} and  object
#'
#' This function returns the initial dataset subject to spatial processing in a \code{SpRarefy} or a \code{SpThin} object.
#'
#' @param x \code{SpRarefy} or a \code{SpThin} object.
#' @param ... not used.
#' @return \code{SpatialPoints} or \code{SpatialPointsDataFrame} object.
#' @seealso \code{\link{SpThin}}.
#' @examples
#' # make simulated dataset
#' testDF <- data.frame(
#'		x=runif(100, -5, -5),
#'		y=runif(100, -5, -5)
#' )
#'
#' # make thinned dataset
#' result1 <- spThin(
#'		testDF,
#'		"x",
#'		"y",
#'		dist=5,
#'		method='heuristic',
#'		1
#' )
#'
#' # show first rows of full dataset in result1
#' head(fulldata(result1))
#'
#' # make rarefied dataset 
#' result2 <- spRarefy(
#'		testDF,
#'		"x",
#'		"y",
#'		grid=5,
#'		1
#' )
#'
#' # show first rows of full dataset in result2
#' head(fulldata(result2))
#'
#' @export
fulldata<-function(x, ...) {UseMethod("fulldata")}

#' Minimum distance in thinned datasets.
#'
#' This function returns the minimum distances in the thinned dataset contained in a \code{SpThin} object.
#'
#' @param x \code{SpThin} object.
#' @param ... not used.
#' @return \code{numeric} minimum distance (m) between records.
#' @seealso \code{\link{SpThin}}.
#' # make thinned dataset using simulated data
#' result <- spThin(
#'		runif(100, -5, -5),
#'		runif(100, -5, -5),
#'		dist=5,
#'		method='heuristic',
#'		1,
#' )
#'
#' # show minimum distance
#' mindist(result)
#'
#' export
mindist<-function(x, ...) {UseMethod("mindist")}

#' Distance used to thin data.
#'
#' This function returns the distance used to thin datasets contained in a \code{SpThin} object.
#'
#' @param x \code{SpThin} object.
#' @param ... not used.
#' @return \code{numeric} distance (m) used to thin records.
#' @seealso \code{\link{SpThin}}.
#' # make thinned dataset using simulated data
#' result <- spThin(
#'		runif(100, -5, -5),
#'		runif(100, -5, -5),
#'		dist=5,
#'		method='heuristic',
#'		1,
#' )
#'
#' # show distance used to thin data
#' thindist(result)
#'
#' export
thindist<-function(x, ...) {UseMethod("thindist")}

#' Distance metric used to thin data.
#'
#' This function returns the name of the distance metric used to thin datasets contained in a \code{SpThin} object.
#'
#' @param x \code{SpThin} object.
#' @param ... not used.
#' @return \code{character} name of distance metric used to thin records.
#' @seealso \code{\link{SpThin}}.
#' # make thinned dataset using simulated data
#' result <- spThin(
#'		runif(100, -5, -5),
#'		runif(100, -5, -5),
#'		dist=5,
#'		method='heuristic',
#'		1,
#' )
#'
#' # show distance name of metric
#' distname(result)
#'
#' export
distname<-function(x, ...) {UseMethod("distname")}

#' Cell size distance used to rarefy dataset.
#'
#' This function returns the cell size used to rarefy records in a \code{SpRarefy} object.
#'
#' @param x \code{SpRarefy} object.
#' @param ... not used.
#' @return \code{numeric} cell size (m) used to rarefy records.
#' @seealso \code{\link{SpRarefy}}.
#' @examples
#' # make rarefied dataset using simulated data
#' result <- spRarefy(
#'		runif(100, -5, -5),
#'		runif(100, -5, -5),
#'		grid=5,
#'		1,
#' )
#'
#' # show cell size
#' cellsize(result)
#'
#' @export
cellsize<-function(x, ...) {UseMethod("cellsize")}

#' Spatially thin species occurrence data
#' 
#' An optimisation algorithm is used to subset the dataset such that all occurrence 
#' locations are a minimum distance apart.  This process helps reduce the effect
#' of biases in observation records on the predictive performance of ecological niche models.
#' 
#' @param x \code{SpatialPoints}, \code{SpatialPointsDataFrame}, \code{data.frame} or \code{numeric} object with x-coordinates.
#' @param x.col \code{character} name of column or \code{numeric} index of column with x-coordinates.
#' @param y.col \code{character} name of column or \code{numeric} index of column with y-coordinates.
#' @param y \code{numeric} object with y-coordinates if argument x is \code{numeric}.
#' @param dist \code{numeric} minimum distance (m) between records.
#' @param method \code{character} name of method to solve problem: \itemize{
#'	\item{'heuristic': data is thinned using a stingy heuristic algorithm; this involves sequentially removing points until the smallest distance between any point is greater than \code{mindist}.}
#'	\item{'lpsolve': the problem is expressed as an integer programming problem and solved using LpSolve.}
#'	\item{'gurobi': as above, but solved using Gurobi.}
#' }
#' @param nrep \code{numeric} number of replicate thinned data sets to produce when using the \code{heuristic} method. 
#' @param great.circle.distance \code{logical} if \code{TRUE} great circle distances will be used for distance calculations, else euclidean distances will be be used.
#' @param ... additional arguments passed to \code{\link[lpSolveAPI]{lp.control}} if method=='lpsolve' or gurobi::gurobi() if method=='gurobi'.
#' @return \code{SpThin} object. 
#' @details Thinning data is an integer programming problem. The objective is to maximise the number of retained points, whilst ensuring that all retained points
#'			are at least a certain distance apart. LpSolve and Gurobi are guaranteed to identify the optimal solution.
#'			Gurobi can be used to thin bigger datasets faster than LpSolve.
#'			However, Gurobi is a commercial product (www.gurobi.com), and following its installation the gurobi R package will need to be installed.
#'			LpSolve, on the other hand, is freely available (under LGPL 2) and does not need the installation of additional software to use.
#'			For especially large datasets, both Gurobi and LpSolve may need an infeasible amount of time or computational resource to solve the problem.
#'			The heuristic method--while it has no guarantee of identifying the optimal solution--may be useful for large datasets
#'			that Gurobi and LpSolve are unable to solve. Additionally, the heuristic method may be useful for identifying multiple
#'			near-optimal solutions for bootstrapping routines.
#' @seealso \code{\link{SpThin}}
#' @examples
#' # load sp package
#' library(sp)
#'
#' # load data
#' data(Heteromys_anomalus_South_America)
#' 
#' # set coordinate system
#' crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
#' 
#' # thin data using heuristics with data input as numeric vectors
#' result1 <- spThin(
#'		Heteromys_anomalus_South_America$LONG,
#'		Heteromys_anomalus_South_America$LAT,
#'      200000,
#'		method='heuristic',
#'		10
#' )
#'
#' # extract first replicate from result1
#' x <- result1[[1]]
#'
#' # thin data using heuristics with data input as data.frame
#' result2 <- spThin(
#'		Heteromys_anomalus_South_America,
#'		x.col = "LONG", 
#'      y.col = "LAT",
#'      200000,
#'		method='heuristic',
#'		10
#' )
#' 
#' # thin data using heuristics with data input as SpatialPointsDataFrame
#' result3 <- spThin(
#'		SpatialPointsDataFrame(
#'			coords=as.matrix(Heteromys_anomalus_South_America[,c("LONG", "LAT")]),
#'			data=Heteromys_anomalus_South_America,
#'			proj4string=crs
#'		),
#'		200000,
#'		method='heuristic',
#'		10
#' )
#' 
#' # as above, but thin data using lpsolve (with 3 second time limit)
#' result4 <- spThin(
#'		SpatialPointsDataFrame(
#'			coords=as.matrix(Heteromys_anomalus_South_America[,c("LONG", "LAT")]),
#'			data=Heteromys_anomalus_South_America,
#'			proj4string=crs
#'		),
#'		200000,
#'		method='lpsolve',
#'		10,
#'		timeout=3
#' )
#' 
#' \dontrun{
#' # as above, but thin data using gurobi
#' result5 <- spThin(
#'		SpatialPointsDataFrame(
#'			coords=as.matrix(Heteromys_anomalus_South_America[,c("LONG", "LAT")]),
#'			data=Heteromys_anomalus_South_America,
#'			proj4string=crs
#'		),
#'		200000,
#'		method='gurobi',
#'		10
#' )
#' }
#' @export
spThin<-function(x, ...) {UseMethod('spThin')}

#' Spatially rarefy species occurrence data
#' 
#' A grid is overlayed on a set of observation records. A random point inside each grid cell is returned. This procedure can be repeated multiple times to yield multiple rarefied datasets.
#' This process helps reduce the effect of biases in observation records on the predictive performance of ecological niche models.
#' 
#' @param x \code{numeric} with x-coordinates, \code{data.frame}, \code{SpatialPoints}, \code{SpatialPointsDataFrame} or  object.
#' @param y \code{numeric} with y-coordinates.
#' @param x.col \code{character} name of column or \code{numeric} index of column with x-coordinates.
#' @param y.col \code{character} name of column or \code{numeric} index of column with y-coordinates.
#' @param grid either \code{numeric} size of grid cells, or a \code{RasterLayer} object.
#' @param nrep \code{numeric} number of replicate thinned data sets to produce.
#' @param proj4string \code{CRS} object with coordinate system for coordinates (not needed for \code{Spatial} objects).
#' @param ... not used.
#' @return \code{SpRarefy} object.
#' @seealso \code{\link{SpRarefy}}
#' @examples
#' # load sp & raster packages
#' library(sp)
#' library(raster)
#' 
#' # load data
#' data(Heteromys_anomalus_South_America)
#'
#' # set projected coordinate system
#' crs <- CRS(
#'	 	'+proj=lcc +lat_1=9 +lat_2=3 +lat_0=6
#'	 	+lon_0=-66 +x_0=1000000 +y_0=1000000 
#'		+ellps=intl +towgs84=-288,175,-376,0,0,0,0 +units=m +no_defs'
#' )
#' 
#' # project lon/lat data to X/Y
#' sp <- spTransform(
#'		SpatialPoints(
#'			coords=as.matrix(Heteromys_anomalus_South_America[,c("LONG", "LAT")]),
#'			proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_def')
#'		),
#'		crs
#'	)
#' 
#' # create X/Y columns with values	
#' Heteromys_anomalus_South_America$X <- coordinates(sp)[,1]
#' Heteromys_anomalus_South_America$Y <- coordinates(sp)[,2]
#' 
#' # make rarefied dataset using numeric vectors
#' result1 <- spRarefy(
#'		Heteromys_anomalus_South_America$X,
#'		Heteromys_anomalus_South_America$Y,
#'      200000,
#'		10,
#'		proj4string=crs
#' )
#'
#' # make rarefied dataset using data.frame
#' result2 <- spRarefy(
#'		Heteromys_anomalus_South_America,
#'		x.col = "LONG", 
#'      y.col = "LAT",
#'      200000,
#'		10,
#'		proj4string=crs
#' )
#'
#' # make rarefied dataset using SpatialPointsDataFrame
#' result3 <-spThin(
#'		SpatialPointsDataFrame(
#'			coords=as.matrix(Heteromys_anomalus_South_America[,c("X", "Y")]),
#'			data=Heteromys_anomalus_South_America,
#'			proj4string=crs
#'		),
#'		10
#' )
#' 
#' # extract first replicate from result3
#' x <- result3[[1]]
#' @export
spRarefy<-function(x, ...) {UseMethod('spRarefy')}

