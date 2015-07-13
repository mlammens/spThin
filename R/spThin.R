



#' @export
spThin <- setClass(
	slots=c(
		data="SpatialPoints_OR_SpatialPointsDataFrame",
		samples="list",
		mindist='numeric',
		call="call"
	)
)

#' @export
setMethod(
	f="extract",
	signature=signature(x="spThin",r="numeric"),
	definition=function(x, r) {
		if(r<length(x@samples))
			stop('r between 1 and the number of replicates')
		return(x@data[x@samples[[r]],])
	}
)

#' @export
setMethod(
	f="nreps",
	signature=signature(x="spThin"),
	definition=function(x) {
		return(length(x@samples))
	}
)

#' @export
setMethod(
	f="call",
	signature=signature(x="spThin"),
	definition=function(x) {
		return(x@call)
	}
)

#' @export
setMethod(
	f="data",
	signature=signature(x="spThin"),
	definition=function(x) {
		return(x@data)
	}
)


#' @export
setMethod(
	f="mindist",
	signature=signature(x="spThin"),
	definition=function(x) {
		return(x@mindist)
	}
)

#' @export
setMethod(
	f="write",
	signature=signature(x="spThin"),
	definition=function(x, type='data' dir=getwd(), base='thin_') {
		match.arg(type, c('data','coords'))
		if (type=='data' & inherits(x@data, 'SpatialPointsDataFrame')) {
			z=x@data
		} else {
			z=x@coords
		}
		llply(seq_along(x@sample), function(i) {
			write.table(
				z[x@sample[[i]],],
				file.path(dir, paste0(base, formatC(i, width=4, flag=0), '.csv')),
				row.names=FALSE,
				sep=','
			)
		})
	}
)

#' @export summary.spThin
#' @title Summary method for results of thin function
#' 
#' @description
#' Summarize the results of \code{thin} function.
#' 
#' @param thinned A list of data.frames returned by \code{\link{thin}} function.
#' @param show logical; if \code{TRUE},the summary values are printed at the console.
#'  
#' @return Returns a list with the (1) maximun number of records, (2) number of data frames
#' with maximun number of records and (3) a table with the number of data frames per 
#' number of records.
#' 
#' @seealso \code{\link{thin}}
summary.spThin <- function(x){
	# init
	reps <- length(x@nrep)
	lat.long.thin.count <- laply(x@samples, nrow )
	max.lat.long.thin.count <- max(lat.long.thin.count)    
	n.max.data.frame <- sum(lat.long.thin.count==max.lat.long.thin.count)
	n.data.frame.records <- table(lat.long.thin.count)
	n.records <- as.numeric(names(n.data.frame.records))
	Frequency <- as.numeric(n.data.frame.records)
	table2 <- as.data.frame(rbind(n.records, Frequency))
	colnames(table2) <- rep(" ", ncol(table2))
	rownames(table2)[1] <- "Number of records"
 
    cat(paste("Maximum number of records after thinning:", 
              max.lat.long.thin.count))  
    cat(paste("\nNumber of data.frames with max records:", 
              n.max.data.frame))

  
  invisible( list("Maximun_number_of_records" = max.lat.long.thin.count, 
                  "Number_of_data_frames_with_maximun_number_of_records" = n.max.data.frame, 
                  "Number_of_Data_frames_per_number_of_records"=table2))
}

