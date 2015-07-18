#' @include RcppExports.R dependencies.R
NULL

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

thin_lpsolve<-function(x, y, mindist, nrep, great.circle.distance, ...) {
	# init
	currFilePath=tempfile(fileext = ".lp")
	cat('\n', currFilePath, '\n')
	# generate input file for lpsolve
	rcpp_make_lpsolve_file(x, y, mindist, great.circle.distance, currFilePath)
	# run lpsolve
	lpmodel=read.lp(currFilePath, type='lp')
	lp.control(lpmodel, presolve=c("rows","rowdominate"), ...)
	solve(lpmodel)
	# return samples
	nrep=min(nrep, get.solutioncount(lpmodel))
	return(
		lapply(
			seq_len(nrep),
			function(i) {
				select.solution(lpmodel, i)
				return(which(as.logical(get.variables(lpmodel)[seq_along(x)])))
			}
		)
	)
}

thin_gurobi<-function(x, y, mindist, nrep, great.circle.distance, ...) {
	return(
		which(
			as.logical(
				gurobi(
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
}