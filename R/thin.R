#' @include dependencies.R generics.R RcppExports.R SpThin.R
NULL

#' @rdname spThin
#' @inheritParams spThin
#' @export
spThin.numeric<-function(x, y, mindist, nrep, great.circle.distance=FALSE) {
	# check validity of inputs
	if (!is.numeric(x))
		stop('x is not a numeric vector')
	if (!is.numeric(y))
		stop('y is not a numeric vector')
	if (!identical(length(x),length(y)))
		stop('length(x) is not length(y)')
	if (!is.numeric(dist))
		stop('mindist is not numeric')
	if (!is.numeric(nrep))
		stop('nrep is not numeric')
	if (!is.logical(great.circle.distance))
		stop('great.circle.distance is not logical')
	currCall<-match.call()
	# generate samples and return spThin object
	return(
		SpThin(
			data=SpatialPoints(
				coords=as.matrix(c(x,y, ncol=2)),
				data=x,
				proj4string=NA
			),
			samples=rcpp_thin_algorithm(
				x,
				y,
				mindist,
				nrep,
				great.circle.distance
			),
			mindist=mindist,
			call=currCall
		)
	)
}


#' @rdname spThin
#' @inheritParams spThin
#' @export
spThin.data.frame<-function(x, lon.col, lat.col, mindist, nrep, great.circle.distance=TRUE) {
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
	currCall<-match.call()
	# generate samples and return spThin object
	return(
		SpThin(
			data=SpatialPointsDataFrame(
				coords=as.matrix(x[,c(lon.col,lat.col)]),
				data=x,
				proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
			),
			samples=rcpp_thin_algorithm(
				x[[lon.col]],
				x[[lat.col]],
				mindist,
				nrep,
				great.circle.distance
			),
			mindist=mindist,
			call=currCall
		)
	)
}

#' @rdname spThin
#' @inheritParams spThin
#' @export
spThin.SpatialPoints<-function(x, mindist, nrep, great.circle.distance=x@proj4string@projargs=='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') {
	if (!is.numeric(mindist))
		stop('mindist is not numeric')
	if (!is.numeric(nrep))
		stop('nrep is not numeric')
	if (!is.logical(great.circle.distance))
			stop('great.circle.distance is not logical')
	currCall<-match.call()
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
			call=currCall
		)
	)
}

