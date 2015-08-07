#' @include dependencies.R generics.R RcppExports.R
NULL

#' SpPartial: An S4 class to represent spatially rarefied or thinned datasets
#'
#' This class is used to store replicates of spatially rarefied thinned datasets.
#'
#' @slot data \code{SpatialPoints} or \code{SpatialPointsDataFrame} object with all records.
#' @slot samples \code{list} with indices for records in each replicate.
#' @slot call \code{call} used to generate object.
#' @export
#' @seealso \code{\link{call}}, \code{\link{data}}, \code{\link{nrep}}, \code{\link{plot}},\code{\link{summary}}, \code{\link{write}}.
SpPartial <- setClass(
	"SpPartial",
	representation(
		data="SpatialPoints",
		samples="list",
		call="call"
	)
)

#' @export
#' @rdname extract
`[[.SpPartial`<-function(x, r=1, ...) {
	if(r<0 | r>length(x@samples))
		stop('r between 1 and the number of replicates')
	return(x@data[x@samples[[r]],])
}

#' @export
#' @rdname nrep
nrep.SpPartial<-function(x, ...) {
	return(length(x@samples))
}

#' @export
#' @rdname fulldata
fulldata.SpPartial<-function(x, ...) {
	return(x@data)
}

#' Show method
#'
#' This method sets the default show method for the \code{SpRarefy} and \code{SpThin} objects.
#'
#' @param object \code{SpRarefy} or \code{SpThin} object.
#' @export
setMethod("show",
	signature(object="SpPartial"),
	function(object) {
		summary(object)
	}
)


