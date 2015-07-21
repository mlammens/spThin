#' @import ggplot2
#' @import methods
#' @importFrom hexbin hexbin
#' @importFrom lpSolveAPI solve.lpExtPtr
#' @importFrom lpSolveAPI lp.control
#' @importFrom lpSolveAPI read.lp
#' @importFrom lpSolveAPI get.solutioncount
#' @importFrom lpSolveAPI select.solution
#' @importFrom lpSolveAPI get.variables
#' @importFrom Matrix sparseMatrix
#' @importFrom gridExtra grid.arrange 
#' @importFrom raster raster
#' @importFrom raster extent
#' @importFrom raster extract
#' @importFrom raster xmin
#' @importFrom raster xmax
#' @importFrom raster ymin
#' @importFrom raster ymax
#' @importFrom raster setValues
#' @importFrom raster rasterToPoints
#' @importFrom raster res
#' @importFrom sp CRS
#' @importFrom sp SpatialPoints
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp is.projected
#' @importFrom sp spTransform
#' @importFrom rgeos gIntersects
#' @importFrom rgdal readOGR
NULL

#' @useDynLib spThin
NULL

