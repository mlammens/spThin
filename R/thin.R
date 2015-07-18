#' @include dependencies.R generics.R RcppExports.R SpThin.R
NULL

#' @rdname spThin
#' @inheritParams spThin
#' @export
spThin.numeric<-function(x, y, mindist, nrep, method='heuristic', great.circle.distance=FALSE, ...) {
	# check validity of inputs
	if (!is.numeric(x))
		stop('x is not a numeric vector')
	if (!is.numeric(y))
		stop('y is not a numeric vector')
	if (!identical(length(x),length(y)))
		stop('length(x) is not length(y)')
	match.args(method, c('lpsolve', 'heuristic', 'gurobi'))
	# generate samples 
	x<-spThin(
		x=SpatialPoints(
			coords=as.matrix(c(x,y, ncol=2)),
			proj4string=NA
		),
		mindist,
		nrep,
		great.circle.distance,
		...
	)
	x@call<-match.call()
	return(x)
}


#' @rdname spThin
#' @inheritParams spThin
#' @export
spThin.data.frame<-function(x, lon.col, lat.col, mindist, nrep, method='heuristic', great.circle.distance=TRUE, ...) {
	# check validity of inputs
	if (!lon.col %in% names(x))
		stop('lon.col not column in x')
	if (!lat.col %in% names(x))
		stop('lat.col not column in x')
	# generate samples
	x<-spThin(
		SpatialPointsDataFrame(
			coords=matrix(c(x[[lon.col]],x[[lat.col]]),ncol=2),
			data=x,
			proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
		),
		mindist,
		nrep,
		method,
		great.circle.distance,
		...
	)
	x@call<-match.call()
	return(x)
}

#' @rdname spThin
#' @inheritParams spThin
#' @export
spThin.SpatialPoints<-function(x, mindist, nrep, method='heuristic', great.circle.distance=x@proj4string@projargs=='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs', ...) {
	if (!is.numeric(mindist))
		stop('mindist is not numeric')
	if (!is.numeric(nrep))
		stop('nrep is not numeric')
	if (!is.logical(great.circle.distance))
		stop('great.circle.distance is not logical')
	if (!'gurobi' %in% installed.packages()[,1])
		stop('gurobi R package is not installed, see ?thin for details')
	# generate samples
	if (method=='lpsolve') {
		samples<-thin_lpsolve(
			x@coords[,1],
			x@coords[,2],
			mindist,
			nrep,
			great.circle.distance,
			...
		)
	} else if (method=='gurobi') {
		samples<-thin_gurobi(
			x,
			y,
			mindist,
			nrep,
			great.circle.distance,
			...
		)
	} else {
		samples<-rcpp_thin_algorithm(
			x@coords[,1],
			x@coords[,2],
			mindist,
			nrep,
			great.circle.distance
		)
	}
	# return result
	return(
		SpThin(
			data=x,
			samples=samples,
			mindist=mindist,
			method=method,
			call=match.call()
		)
	)
}

