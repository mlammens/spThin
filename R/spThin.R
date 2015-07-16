#' @include dependencies.R generics.R RcppExports.R SpPartial.R
NULL

#' SpThin: An S4 class to represent spatially thinned datasets
#'
#' This class is used to store replicates of spatially thinned datasets.
#'
#' @slot data \code{SpatialPoints} or \code{SpatialPointsDataFrame} object with all records.
#' @slot samples \code{list} with indices for records in each replicate.
#' @slot mindist \code{numeric} minimum distance (m) between records.
#' @slot call \code{call} used to generate object.
#' @export
#' @seealso \code{\link{call}}, \code{\link{data}}, \code{\link{samples}}, \code{\link{mindist}}, \code{\link{nrep}}, \code{\link{plot}},\code{\link{summary}}, \code{\link{write}}.
SpThin <- setClass(
	"SpThin",
	representation(
		mindist="numeric"
	),
	contains='SpPartial'
)

#' @export
#' @rdname mindist
mindist.SpThin<-function(x) {
	return(x@mindist)
}


