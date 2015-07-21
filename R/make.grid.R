#' Make a \code{RasterLayer} based on \code{extent} and a cell size
#'
#' This function generates a \code{RasterLayer} using an \code{extent} object and a cell size.
#'
#' @param x \code{Extent} object.
#' @param cellsize \code{numeric} cell size for each cell.
#' @param crs \code{CRS} object representing coordinate system. Defaults to NA.
#' @return \code{RasterLayer}.
#' @seealso \code{\link[raster]{raster}}, \code{\link[sp]{CRS}}, \code{\link[raster]{Extent}}.
#' @examples
#' # load sp & raster packages
#' library(sp)
#' library(raster)
#' 
#' # create points dataset
#' pts <- SpatialPoints(coords=matrix(rnorm(100), ncol=2))
#'
#' # make RasterLayer with sqaure cells
#' r1 <- make.grid(extent(pts), 5, CRS())
#'
#' # make RasterLayer with rectangular cells
#' r2 <- make.grid(extent(pts), c(1,5), CRS())
#'
#' @export
make.grid<-function(x, cellsize, crs) {
	stopifnot(inherits(x, "Extent"))
	stopifnot(inherits(cellsize, "numeric"))
	stopifnot(inherits(crs, "CRS"))
	if (length(cellsize)==1)
		cellsize=rep(cellsize, 2)
	if (length(cellsize)!=2)
		stop('length(cellsize) should be either 1 or 2')
	xvals=seq(xmin(x), xmax(x)+cellsize[1], cellsize[1])
	yvals=seq(ymin(x), ymax(x)+cellsize[2], cellsize[2])
	return(
		setValues(
			raster(
				extent(
						min(xvals),
						max(xvals),
						min(yvals),
						max(yvals)
				),
				nrows=length(yvals),
				ncols=length(xvals),
				crs=crs
			),
			seq_len(length(xvals) * length(yvals))
		)
	)
}