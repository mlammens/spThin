#' @include dependencies.R generics.R RcppExports.R SpThin.R
NULL

#' @rdname thin
#' @inheritParams thin
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
	x=SpatialPointsDataFrame(
		coords=as.matrix(x[,c(lon.col,lat.col)]),
		data=x,
		proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
	)
	currCall=match.call()
	# generate samples and return spThin object
	return(
		SpThin(
			data=x,
			samples=rcpp_thin_algorithm(
				x@coords[,1],
				x@coords[,2],
				mindist,
				nrep,
				great.circle.distance
			),
			mindist=mindist,
			nrep=as.integer(nrep),			
			call=currCall
		)
	)
}

#' @rdname thin
#' @inheritParams thin
#' @export
thin.SpatialPoints<-function(x, mindist, nrep, great.circle.distance=x@proj4string@projargs=='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') {
	currCall=match.call()
	return(
		SpThin(
			data=x,
			samples=rcpp_thin_algorithm(
				x@coords[,1],
				x@coords[,2],
				mindist,
				nrep,
				great.circle.distance
			),
			mindist=mindist,
			nrep=as.integer(nrep),
			call=currCall
		)
	)
}

