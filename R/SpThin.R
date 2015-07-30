#' @include dependencies.R generics.R spThin-internal.R RcppExports.R SpPartial.R
NULL

#' SpThin: An S4 class to represent spatially thinned datasets
#'
#' This class is used to store replicates of spatially thinned datasets.
#'
#' @slot data \code{SpatialPoints} or \code{SpatialPointsDataFrame} object with all records.
#' @slot samples \code{list} with indices for records in each replicate.
#' @slot mindist \code{numeric} minimum distance (m) between records.
#' @slot method \code{character} name of method used to solve problem.
#' @slot call \code{call} used to generate object.
#' @export
#' @seealso \code{\link{call}}, \code{\link{data}}, \code{\link{mindist}}, \code{\link{nrep}}, \code{\link{plot}},\code{\link{summary}}, \code{\link{write}}.
SpThin <- setClass(
	"SpThin",
	representation(
		mindist="numeric",
		method="character"
	),
	contains='SpPartial'
)

#' @export
#' @rdname mindist
mindist.SpThin<-function(x) {
	return(x@mindist)
}


#' Map and diagnostic plots for thinned datasets
#' 
#' This function produces plots to describe \code{SpThin} datasets:
#' \enumerate{
#' 	\item map showing distribution of occurrence records (hexagons) and records in best solution (red points);
#' 	\item plot of the number of repetitions versus the number of maximum records retained;
#'	\item log-log version of the above plot;
#'	\item histogram with density curve of the maximum records retained.
#' }
#'
#' @param x \code{SpThin} object.
#' @param which "numeric" values to indicate which plots to produce.
#' @param ... not used.
#' @seealso \code{\link{spThin}}.
#' @method plot SpThin
#' @examples
#' # load data
#' data(Heteromys_anomalus_South_America)
#'
#' # make thinned dataset 
#' result <-spThin(
#'		Heteromys_anomalus_South_America,
#'		x.col = "LONG", 
#'      y.col = "LAT",
#'		method='heuristic',		
#'      200000,
#'		10
#'	)
#'
#' # show map + diagnostic plots
#' plot(result)
#'
#' @export
plot.SpThin<-function(x, which=1:4, ...) {
	# init
	lat.long.thin.count <- sapply(x@samples, length)
	cummax.lat.long.thin.count <- cummax(lat.long.thin.count)
	if (x@method=='heuristic') {	
		plotList=lapply(as.character(which), function(i) {
			switch(i,
				'1'={
					ggplot(
						as.data.frame(x@data@coords),
						aes_string(x="coords.x1", y="coords.x2")
					) +
					theme_classic() +					
					suppressWarnings(stat_binhex(bins=5)) +
					geom_point(
						data=as.data.frame(x@data@coords[x@samples[[which.max(sapply(x@samples, length))]],,drop=FALSE]),
						aes_string(x='coords.x1', y='coords.x2'),
						col="#ffffb3"
					) +
					coord_equal() +			
					scale_fill_continuous(name="Frequency" ) +
					theme(
						legend.position='bottom',
						axis.title=element_blank(),
						axis.ticks=element_blank(),
						axis.text=element_blank(),
						axis.line=element_blank()
					) +
					ggtitle("Occurrence records")
				},
				'2'={
					ggplot(
						data=data.frame(x=lat.long.thin.count),
						aes_string(x='x')
					) + 
					geom_histogram(
						aes(y=..density..),
						col='transparent',
						fill='grey20'
					) +
					geom_density(col='transparent', fill='blue', alpha=0.3) +
					xlab('Maximum records retained') +
					ylab('Density') + 
					theme_classic() +
					theme(axis.title.y=element_text(vjust=1)) +
					theme(axis.title.x=element_text(vjust=-0.5))
				},				
				'3'={
					ggplot(
						data.frame(
							x=seq_len(nrep(x)),
							y=cummax.lat.long.thin.count
						),
						aes_string(x='x',y='y')
					) + 
					geom_point() +
					xlab('Number Repetitions') +
					ylab('Cumulative Maximum Records Retained') +
					coord_cartesian(xlim=c(1,nrep(x)))  + 
					theme_classic() +
					theme(axis.title.y=element_text(vjust=1)) +
					theme(axis.title.x=element_text(vjust=-0.5))
				},
				'4'={
					ggplot(
						data.frame(
							x=seq_len(nrep(x)),
							y=cummax.lat.long.thin.count
						),
						aes_string(x='x',y='y')
					) + 
					geom_point() +
					xlab('Log Number Repetitions') +
					ylab('Log Cumulative Maximum Records Retained') +
					coord_trans(ytrans="log", xtrans="log") +
					theme_classic() +
					theme(axis.title.y=element_text(vjust=1)) +
					theme(axis.title.x=element_text(vjust=-0.5))
				}				
			)
		})	
	} else {
		plotList=list(
			ggplot(
				as.data.frame(x@data@coords),
				aes_string(x='coords.x1', y='coords.x2')
			) +
			suppressWarnings(stat_binhex(bins=5)) +
			geom_point(
				data=as.data.frame(x@data@coords[x@samples[[which.max(sapply(x@samples, length))]],,drop=FALSE]),
				aes_string(x='coords.x1', y='coords.x2'),
				col="#ffffb3"
			) +
			theme_classic() +
			coord_equal() +			
			scale_fill_continuous(name="Frequency", guide = guide_legend()) +
			theme(
				legend.position='bottom',
				axis.title=element_blank(),
				axis.ticks=element_blank(),
				axis.text=element_blank(),
				axis.line=element_blank()
			) +
			ggtitle("Occurrence records")
		)
	}
	
	# print plots	
	if (length(plotList)==1) {
		print(plotList[[1]])
	} else if (length(plotList)==2) {
		grid.arrange(plotList[[1]], plotList[[2]], ncol=2)
	} else if (length(plotList)==3) {
		grid.arrange(plotList[[1]], plotList[[2]], plotList[[3]], ncol=2, nrow=2)
	} else {
		grid.arrange(plotList[[1]], plotList[[2]], plotList[[3]], plotList[[4]], ncol=2, nrow=2)
	}
}


#' Summarize thinned datasets
#' 
#' \code{summary} method for \code{SpThin} objects.
#' 
#' @param object \code{SpThin} object.
#' @param ... not used.
#' @seealso \code{\link{spThin}}.
#' @examples
#' # load data
#' data(Heteromys_anomalus_South_America)
#'
#' # make thinned dataset 
#' result <-spThin(
#'		Heteromys_anomalus_South_America,
#'		x.col = "LONG", 
#'      y.col = "LAT",
#'		method='heuristic',		
#'      200000,
#'		10
#'	)
#'
#' # show summary
#' summary(result)
#'
#' @export
summary.SpPartial <- function(object, ...) {
	# init
	cat('SpThin object.\n\n')
	cat('Call: ')
	print(object@call)
	cat('Method: ',object@method,'\n',sep="")
	cat('Minimum distance: ',object@mindist,'\n',sep="")
	cat('Number of replicates: ',length(object@samples),'\n',sep="")
	cat('Initial number of records: ',nrow(object@data@coords),'\n',sep="")
	cat('Best thinned dataset: ',which.max(sapply(object@samples,length)),' (', max(sapply(object@samples,length)),' records)', '\n',sep="")	
}


#' Write thinned dataset replicates to file.
#'
#' This function writes the spatially thinned replicates in a \code{SpThin} object to files on a computer.
#'
#' @param x \code{SpThin} object.
#' @param coords \code{logical} if \code{TRUE} only coordinates of thinned data will be be saved, otherwise all columns of thinned data will be saved.
#' @param dir \code{character} directory to save output files in.
#' @param base \code{character} base name to save output files in.
#' @param ... not used.
#' @seealso \code{\link{SpThin}}.
#' @examples
#' # load data
#' data(Heteromys_anomalus_South_America)
#'
#' # make thinned dataset 
#' result <-spThin(
#'		Heteromys_anomalus_South_America,
#'		x.col = "LONG", 
#'      y.col = "LAT",
#'		method='heuristic',		
#'      200000,
#'		10
#'	)
#'
#' # save data to temporary directory
#' write.SpThin(result, dir=tempdir())
#'
#' # show files in temporary directory
#' dir(tempdir())
#
#' # remove files
#' unlink(dir(tempdir(), '^thin_.*.csv$'))
#'
#' @export
write.SpThin<-function(x, coords=FALSE, dir=getwd(), base='thin_') {
	write.spthin(x, coords=FALSE, dir=getwd(), base='thin_')
}






