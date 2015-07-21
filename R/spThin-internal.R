#' @include RcppExports.R dependencies.R
NULL


utils::globalVariables(c("..density.."))

.onUnload <- function (libpath) {
  library.dynam.unload("spThin", libpath)
}

grbiWrapper<-function(x) {
	return(
		list(
			A=sparseMatrix(i=x$i,j=x$j,x=x$x, dims=c(x$nrow, x$ncol)),
			obj=x$obj,
			rhs=x$rhs,
			vtype=x$vtypes,
			sense=x$sense,
			modelsense="max"
		)
	)
}

thin_lpsolve<-function(x, y, mindist, great.circle.distance, ...) {
	# init
	currFilePath=tempfile(fileext = ".lp")
	# generate input file for lpsolve
	rcpp_make_lpsolve_file(x, y, mindist, great.circle.distance, currFilePath)
	# run lpsolve
	lpmodel=read.lp(currFilePath, type='lp')
	lp.control(lpmodel, presolve=c("rows","rowdominate"), ...)
	solve(lpmodel)
	# return samples
	return(
		lapply(
			seq_len(get.solutioncount(lpmodel)),
			function(i) {
				select.solution(lpmodel, i)
				return(which(as.logical(get.variables(lpmodel)[seq_along(x)])))
			}
		)
	)
}

thin_gurobi<-function(x, y, mindist, great.circle.distance, ...) {
	return(
		list(
			which(
				as.logical(
					gurobi::gurobi(
						grbiWrapper(
							rcpp_make_gurobi_object(
								x,
								y,
								mindist,
								great.circle.distance
							)
						),
						params=list(Presolve=2, ...)
					)$x
				)
			)
		)
	)
}


is.installed<-function(x) {
	if (x %in% installed.packages()[,1])
		return(FALSE)
	if (!file.exists(
			file.path(
				find.package(x, quiet=T),
				"libs",
				.Platform$r_arch)
			)
		) {
		warning(paste0("package ",x," is not installed for 'arch = ",.Platform$r_arch,"'"))
		return(FALSE)
	}
	return(TRUE)
}

write.spthin<-function(x, coords, dir, base) {
	if (!coords & inherits(x@data, 'SpatialPointsDataFrame')) {
		z=x@data
	} else {
		z=x@coords
	}
	sapply(seq_along(x@samples), function(i) {
		write.table(
			z[x@samples[[i]],],
			file.path(dir, paste0(base, formatC(i, width=4, flag=0), '.csv')),
			row.names=FALSE,
			sep=','
		)
	})
	return(invisible(TRUE))
}