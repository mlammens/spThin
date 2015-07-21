#' @include dependencies.R generics.R RcppExports.R SpFilter.R
NULL

#' @rdname spFilter
#' @inheritParams spFilter
#' @export
spFilter.numeric<-function(x, y, grid, nrep, proj4string=CRS(), ...) {
	# check validity of inputs
	if (!is.numeric(x))
		stop('x is not a numeric vector')
	if (!is.numeric(y))
		stop('y is not a numeric vector')
	if (!identical(length(x),length(y)))
		stop('length(x) is not length(y)')
	currCall<-match.call()
	# generate samples and return spFilter object	
	x=spFilter(
		SpatialPoints(
			coords=matrix(c(x,y), ncol=2),
			proj4string=proj4string
		),
		grid,
		nrep
	)
	x@call=currCall
	return(x)
}


#' @rdname spFilter
#' @inheritParams spFilter
#' @export
spFilter.data.frame<-function(x, x.col, y.col, grid, nrep, proj4string=CRS(), ...) {
	# check validity of inputs
	if (!x.col %in% names(x))
		stop('x.col not column in x')
	if (!y.col %in% names(x))
		stop('y.col not column in x')
	currCall<-match.call()
	# generate samples and return spFilter object
	x=spFilter(
		SpatialPointsDataFrame(
			coords=as.matrix(x[,c(x.col, y.col)]),
			data=x,
			proj4string=proj4string
		),
		grid,
		nrep		
	)
	x@call=currCall
	return(x)
}

#' @rdname spFilter
#' @inheritParams spFilter
#' @export
spFilter.SpatialPoints<-function(x, grid, nrep, ...) {
	if (!is.numeric(grid) & !inherits(grid, 'RasterLayer') & !inherits(grid, 'SpatialPolygons'))
		stop('grid is not a numeric vector, RasterLayer, or SpatialPolygons object')
	if (!is.numeric(nrep))
		stop('nrep is not numeric')
	if (!is.numeric(nrep))
		stop('nrep is not numeric')
	currCall<-match.call()
	# throw warnings
	if (is.na(x@proj4string@projargs))
		warning('x is not associated with a valid coordinate system')
	if (!is.projected(x) || is.na(is.projected(x))) {
		warning('x does not have a projected coordinate system.\n  Reproject x to a coordinate system suitable for area calculations.')
	}
	# make grid
	if (inherits(grid, c('numeric'))) {
		grid<-make.grid(extent(x), grid, x@proj4string)
	} else if (inherits(grid, 'SpatialPolygons')) {
		ids<-sapply(gIntersects(x,grid,byid=TRUE,returnDense=TRUE), `[[`, 1)
		cellsize<-as.numeric(NA)
	}
	if (inherits(grid, c('RasterLayer'))) {
		ids<-extract(grid, x)
		cellsize<-res(grid)
	}
	# generate samples and return SpFilter object
	return(
		SpFilter(
			data=x,
			samples=rcpp_filter_algorithm(
				split(
					seq_len(nrow(x@coords)),
					ids
				),
				nrep
			),
			cellsize=cellsize,
			call=currCall,
			grid=grid
		)
	)
}

