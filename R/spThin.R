#' @include dependencies.R generics.R RcppExports.R
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
		data="SpatialPoints",
		samples="list",
		mindist='numeric',
		nrep="integer",
		call="call"
	)
)

#' @export
#' @rdname samples
samples.SpThin<-function(x, r=1) {
	return(x[[r]])
}

#' @export
#' @rdname samples
`[[.SpThin`<-function(x, r=1) {
	if(r<0 | r>length(x@samples))
		stop('r between 1 and the number of replicates')
	return(x@data[x@samples[[r]],])
}

#' @export
#' @rdname nrep
nrep.SpThin<-function(x) {
	return(length(x@samples))
}


#' @export
#' @rdname call
call.SpThin<-function(x) {
	return(x@call)
}

#' @export
#' @rdname data
data.SpThin<-function(x) {
	return(x@data)
}

#' @export
#' @rdname mindist
mindist.SpThin<-function(x) {
	return(x@mindist)
}

#' @export
#' @rdname write
write.SpThin<-function(x, coords=FALSE, dir=getwd(), base='thin_') {
	if (!coords & inherits(x@data, 'SpatialPointsDataFrame')) {
		z=x@data
	} else {
		z=x@coords
	}
	llply(seq_along(x@samples), function(i) {
		write.table(
			z[x@samples[[i]],],
			file.path(dir, paste0(base, formatC(i, width=4, flag=0), '.csv')),
			row.names=FALSE,
			sep=','
		)
	})
}

#' Summarize thinned datasets
#' 
#' \code{summary} method for \code{SpThin} objects.
#' 
#' @param x \code{SpThin} object.
#' @return \code{list} with the (1) maximum number of records, (2) number of data frames
#' with maximum number of records and (3) a table with the number of data frames per number of records.
#' @seealso \code{\link{SpThin}}
#' @export
summary.SpThin <- function(x){
	# init
	reps <- length(x@nrep)
	lat.long.thin.count <- laply(x@samples, length)
	max.lat.long.thin.count <- max(lat.long.thin.count)    
	n.max.data.frame <- sum(lat.long.thin.count==max.lat.long.thin.count)
	n.data.frame.records <- table(lat.long.thin.count)
	n.records <- as.numeric(names(n.data.frame.records))
	Frequency <- as.numeric(n.data.frame.records)
	table2 <- as.data.frame(rbind(n.records, Frequency))
	colnames(table2) <- rep(" ", ncol(table2))
	rownames(table2)[1] <- "Number of records"
 
    cat(
		paste("Maximum number of records after thinning:", 
              max.lat.long.thin.count
		)
	)
    cat(
		paste("\nNumber of data.frames with max records:", 
              n.max.data.frame
		)
	)
	invisible(
		list(
			"Maximum_number_of_records" = max.lat.long.thin.count,
			"Number_of_data_frames_with_maximun_number_of_records" = n.max.data.frame,
			"Number_of_Data_frames_per_number_of_records"=table2
		)
	)
}

#' Diagnostic plots for thinned datasets
#' 
#' This function produces plots to describe the thinned datasets:
#' \begin{enumerate}
#' 	\item plot of the number of repetitions versus the number of maximum records retained;
#'	\item log-log version of the above plot;
#'	\item histogram with density curvie of the maximum records retained.
#' \end{enumerate}
#'
#' @param x \code{SpThin} object.
#' @param which "numeric" values to indicate which plots to produce.
#' @seealso \code{\link{thin}}
#' @export
plot.SpThin <- function(x, which=c(1:3), ...) {
	# init
	lat.long.thin.count <- laply(x@samples, length)
	cummax.lat.long.thin.count <- cummax(lat.long.thin.count)

	# plot 1: number of reps vs the number of maximum 
	#		  records retained at each repetition
	plotList=list()
	if(any(1==which)){
		plotList=append(plotList, list(
			ggplot(
				data.frame(
					x=seq_len(x@nrep),
					y=cummax.lat.long.thin.count
				),
				aes(x,y)
			) + 
			geom_point() +
			xlab('Number Repetitions') +
			ylab('Cumulative Maximum Records Retained') +
			coord_cartesian(xlim=c(1,x@nrep))  + 
			theme_classic() +
			theme(axis.title.y=element_text(vjust=1)) +
			theme(axis.title.x=element_text(vjust=-0.5))
		))
	}
 
	# plot 2: log-log plot of plot 1
	if(any(2==which)) {
		plotList=append(plotList, list(
			ggplot(
				data.frame(
					x=seq_len(x@nrep),
					y=cummax.lat.long.thin.count
				),
				aes(x,y)
			) + 
			geom_point() +
			xlab('Log Number Repetitions') +
			ylab('Log Cumulative Maximum Records Retained') +
			coord_trans(y="log", x="log") +
			theme_classic() +
			theme(axis.title.y=element_text(vjust=1)) +
			theme(axis.title.x=element_text(vjust=-0.5))
		))
	}
	# plot 3: histogram of lat.long.thin.count
	if (any(3==which)) {
		plotList=append(plotList, list(
			ggplot(
				data=data.frame(x=lat.long.thin.count),
				aes(x=x)
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
		))
	}
	
	# print plots	
	if (length(plotList)==1) {
		print(plotList[[1]])
	} else if (length(plotList)==2) {
		grid.arrange(plotList[[1]], plotList[[2]], ncol=2)
	} else {
		grid.arrange(plotList[[1]], plotList[[2]], plotList[[3]], ncol=2, nrow=2)
	}
}

