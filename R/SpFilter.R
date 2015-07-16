#' @include dependencies.R generics.R RcppExports.R SpPartial.R
NULL

#' SpFilter: An S4 class to represent spatially filtered datasets
#'
#' This class is used to store replicates of spatially filtered datasets.
#'
#' @slot data \code{SpatialPoints} or \code{SpatialPointsDataFrame} object with all records.
#' @slot samples \code{list} with indices for records in each replicate.
#' @slot cellsize \code{numeric} height and width of cells (m) used to sample records.
#' @slot call \code{call} used to generate object.
#' @slot grid \code{RasterLayer} used to filter data.
#' @export
#' @seealso \code{\link{call}}, \code{\link{data}}, \code{\link{samples}}, \code{\link{cellsize}}, \code{\link{nrep}}, \code{\link{plot}},\code{\link{summary}}, \code{\link{write}}.
SpFilter <- setClass(
	"SpFilter",
	representation(
		cellsize="numeric",
		grid="RasterLayer"
	),
	contains='SpPartial'
)

#' @export
#' @rdname grid
grid.SpFilter <- function(x) {
	return(x@grid)
}

#' @export
#' @rdname cellsize
cellsize.SpFilter <- function(x) {
	return(x@cellsize)
}


