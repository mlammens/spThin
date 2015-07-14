
#' Extract samples from SpThin object
#'
#' This function extracts samples from a \code{SpThin} object.
#'
#' @param x \code{SpThin} object.
#' @param r \code{numeric} n'th replicate to extract samples from.
#' @return \code{SpatialPoints} or  \code{SpatialPointsDataFrame} object
#' @seealso \code{\link{SpThin}}.
samples<-function(x, ...) {UseMethod("samples")}

#' Number replicates in SpThin object
#'
#' This function extracts the number of replicates contained in a \code{SpThin} object.
#'
#' @param x \code{SpThin} object.
#' @return \code{integer} number of replicates.
#' @seealso \code{\link{SpThin}}.
nrep<-function(x, ...) {UseMethod("nrep")}

#' Function call used to generate SpThin object
#'
#' This function returns the function call used to generate a \code{SpThin} object.
#'
#' @param x \code{SpThin} object.
#' @return \code{call} object.
#' @seealso \code{\link{SpThin}}.
call<-function(x, ...) {UseMethod("call")}
call.default<-base::call

#' Complete dataset contained in a SpThin object
#'
#' This function returns the complete dataset subject to spatial thinning in a \code{SpThin} object.
#'
#' @param x \code{SpThin} object.
#' @return \code{call} object.
#' @seealso \code{\link{SpThin}}.
data<-function(x, ...) {UseMethod("data")}
data.default<-utils::data

#' Minimum distance used to thin dataset.
#'
#' This function returns the minimum distance used to thin records in a \code{SpThin} object.
#'
#' @param x \code{SpThin} object.
#' @return \code{numeric} distance (m) used to thin records.
#' @seealso \code{\link{SpThin}}.
mindist<-function(x, ...) {UseMethod("mindist")}

#' Write thinned dataset replicates to file.
#'
#' This function writes the spatially thinned replicates in a \code{SpThin} object to files on a computer.
#'
#' @param x \code{SpThin} object.
#' @param coords \code{logical} if \code{TRUE} only coordinates of thinned data will be be saved, otherwise all columns of thinned data will be saved.
#' @param dir \code{character} directory to save output files in.
#' @param base \code{character} base name to save output files in.
#' @seealso \code{\link{SpThin}}.
write<-function(x, ...) {UseMethod("write")}
write.default<-base::write


#' Spatially thin species occurrence data
#' 
#' A heuristic algorithm is used to subset the dataset such that all occurrence 
#' locations are a minimum distance apart. This process helps reduce the effect
#' of biases in observation records on the predictive performance of ecological niche models.
#' 
#' @param x \code{SpatialPoints}, \code{SpatialPointsDataFrame} or \code{data.frame} object.
#' @param lon.col \code{character} name of column or \code{numeric} index of column with latitude values.
#' @param lat.col \code{character} name of column or \code{numeric} index of column with longitude values.
#' @param mindist \code{numeric} minimum distance (m) between records.
#' @param reps \code{numeric} number of replicate thinned data sets to produce.
#' @param great.circle.distance \code{logical} if \code{TRUE} great circle distances will be used for distance calculations, else euclidean distances will be be used.
#' @param fast \code{logical} if \code{TRUE} the point with the most number of close neighbours will be removed at each iteration. This results in a lower run-time with less variation between replicates. Otherwise the points will be randomly sampled for removal based on their number of close neighbours. This results in a longer run-time with more variation between replicates.
#' @export
#' @return \code{SpThin} object. 
#' @seealso \code{\link{SpThin}}
thin<-function(x, ...) {UseMethod('thin')}
