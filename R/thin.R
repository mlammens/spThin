#' @export thin
#' @title Spatially thin species occurrence data
#' 
#' @description
#' \code{thin} returns spatially thinned species occurrence data sets.
#' A randomizaiton algorithm is used to create
#' data set in which all occurrence locations are at least \code{thin.par}
#' distance apart. Spatial thinning helps to reduce the effect of uneven,
#' or biased, species occurrence collections on spatial model outcomes.
#' 
#' @param loc.data A data.frame of occurrence locations. It can include several
#'   columns, but must include at minimum a column of latitude and a
#'   column of longitude values
#' @param lat.col Name of column of latitude values. Caps sensitive.
#' @param long.col Name of column of longitude values. Caps sensitive.
#' @param thin.par Thinning parameter - the distance (in kilometres) that you want
#'   records to be separated by.
#' @param reps The number of times to repeat the thinning process. Given the random
#'   process of removing nearest-neighbours there should be 'rep' number of different
#'   sets of coordinates.
#' @param locs.thinned.list.return TRUE/FALSE - If true, the `list` of 
#'   the data.frame of thinned locs resulting from each replication 
#'   is returned (see Returns below).
#' @param write.files TRUE/FALSE - If true, new *.csv files will be 
#'   written with the thinned locs data
#' @param max.files The maximum number of *csv files to be written based on the
#'   thinned data
#' @param out.dir Directory to write new *csv files to
#' @param out.base A file basename to give to the thinned datasets created
#' @param write.log.file TRUE/FALSE create/append log file of thinning run
#' @param log.file Text log file 
#' @param verbose TRUE/FALSE - If true, running details of the function are print at the console.
#' 
#' @return locs.thinned.dfs A list of data.frames, each data.frame
#'   the spatially thinned locations of the algorithm for a 
#'   single replication. This list will have `reps` elements.
#'   
#' @seealso \code{\link{thin.algorithm}}
#'
thin<-function(x, ...) {UseMethod('thin')}

#' @export
thin.data.frame<-function(x, lon.col, lat.col, mindist, nrep, great.circle.distance=TRUE) {
	# check validity of inputs
	if (!lon.col %in% names(x))
		stop('lon.col not column in x')
	if (!lat.col %in% names(x))
		stop('lat.col not column in x')
	if (!is.numeric(mindist))
		stop('mindist is not numeric')
	if (!is.numeric(nrep))
		stop('nrep is not numeric')
	if (!is.logical(great.circle.distance))
		stop('great.circle.distance is not logical')
	# coerce x to Spatial object
	x=SpatialPointsData.frame(
		coords=as.matrix(x[,c(lon.col,lat.cal)])
		data=x
		proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
	)
	# generate samples and return spThin object
	return(
		spThin(
			x,
			rcpp_thin_algorithm(
				x@coords[,1],
				x@coords[,2],
				mindist,
				reps,
				FALSE
			),
			mindist,
			match.call()
		)
	)
}

#' @export
thin.SpatialPoints<-function(x, mindist, nrep, great.circle.distance=x@proj4string@projargs=='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') {
	return(
		spThin(
			x,
			rcpp_thin_algorithm(
				x@coords[,1],
				x@coords[,2],
				mindist,
				reps,
				great.circle.distance
			),
			mindist,
			match.call()
		)
	)
}

