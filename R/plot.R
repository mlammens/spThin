#' @export plot.spThin
#' @title Plot diagnosis for results of thin function 
#' 
#' @description
#' Three plots (selected by \code{which}) are currently available: 
#' a plot of the number of repetitions versus the number of maximum records retained
#' at each repetition ([1] observed values; [2] log transformed) and 
#' a histogram of the maximum records retained [3].
#' 
#' @param thinned A list of data.frames returned by \code{\link{thin}} function.
#' @param which if a subset of the plots is required, specify a subset of the numbers 1:3.
#' @param ask logical; if \code{TRUE}, the user is asked before each plot, see par(ask=.).
#' @param ... other parameters to be passed through to plotting functions.
#'
#' @seealso \code{\link{thin}}
plot.spThin <- function(
	thinned, 
	which=c(1:3), 
	...) {

	## Repetition number
	reps <- length(thinned)

	## Look at the number of locs kept in each thinned dataset
	## by determining the number of rows in each returned data.frame
	lat.long.thin.count <- unlist(lapply(thinned, nrow ))

	## Create a vector of cummulative maximum records at each 
	## repetition number
	cummax.lat.long.thin.count <- cummax(lat.long.thin.count)

	## Plot the number of repetitions versus the number 
	## of maximum records retained at each repetition
	plotList=list()
	if(any(1==which)){
		print('here1')
		plotList=append(plotList, list(
			ggplot(
				data.frame(
					x=seq_len(reps),
					y=cummax.lat.long.thin.count
				),
				aes(x,y)
			) + 
			geom_point() +
			xlab('Number Repetitions') +
			ylab('Cumulative Maximum Records Retained') +
			coord_cartesian(xlim=c(1,reps))  + 
			theme_classic() +
			theme(axis.title.y=element_text(vjust=1)) +
			theme(axis.title.x=element_text(vjust=-0.5))
		))
	}
 
	## Make a log-log plot of the number of repetitions versus
	## the number of maximum records retained
	if(any(2==which)) {
		print('here2')
		plotList=append(plotList, list(
			ggplot(
				data.frame(
					x=seq_len(reps),
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
	## histogram of lat.long.thin.count
	if (any(3==which)) {
		print('here3')
		plotList=append(plotList, list(
			ggplot(
				data=data.frame(x=lat.long.thin.count),
				aes(x)
			) + 
			geom_histogram(aes(y=..density..), col='transparent', fill='grey20') +
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