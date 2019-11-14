#' @export thin.algorithm
#' @title Implements random spatial thinning algorithm
#' 
#' @description \code{thin.algorithm} implements a randomization approach to
#' spatially thinning species occurence data. This function is the algorithm underlying
#' the \code{\link{thin}} function.
#' 
#' @param rec.df.orig A data frame of long/lat points for each presence record. The 
#' data.frame should be a two-column data frame, one column of long and one of 
#' lat
#' @param thin.par Thinning parameter - the distance (in kilometers) that you want
#' records to be separated by.
#' @param reps The number of times to repete the thinning process. Given the random
#' process of removing nearest-neighbors there should be 'rep' number of different
#' sets of coordinates.
#' @return reduced.rec.dfs: A list object of length 'rep'. Each list element is a different
#' data.frame of spatially thinned presence records.
#' 

thin.algorithm <- function(rec.df.orig, thin.par, reps) {
  
  ## Create an empty list object of length `reps` to store thinned occs datasets
  reduced.rec.dfs <- vector("list", reps)
  
  ## Calculate square distance matrix AND identify which elements
  ## are less than the thin parameter
  ## ***
  ## Distances calculated using fields::rdist.earthThis function
  ## which returns distances, correcting for change in distance
  ## of degree of longitude based on distance from equator
  DistMat.save <- rdist.earth(x1=rec.df.orig, miles=FALSE) < thin.par
  
  ## Set the diagonal of the dist matrix to FALSE values
  diag(DistMat.save) <- FALSE
  
  ## Set any NA values in the dist matrix to FALSE
  DistMat.save[is.na(DistMat.save)] <- FALSE
  
  ## Calculate the row sums of the DistMat.save object
  ## This returns the number of elements that are less than
  ## the thin.par for each row
  SumVec.save <- rowSums(DistMat.save)
  
  ## Make a vector of TRUE values of length equal to the number
  ## of rows in the DistMat
  df.keep.save <- rep(TRUE, length(SumVec.save))
  
  for (Rep in seq_len(reps)) {
    ## For each iteration in reps, reset the DistMat and
    ## other indicator variables to original values
    DistMat <- DistMat.save
    SumVec <- SumVec.save
    df.keep <- df.keep.save
    
    ## Perform while loop based on two criteria
    ## 1. The minimum distance between two occurences is less than the 
    ##    thinning parameter 
    ## 2. The number of rows in the resulting data set is greater than 1
    while (any(DistMat) && sum(df.keep) > 1) {
      
      ## Identify the row(s) (occurence) that is within the thin.par distance
      ## to the greatest number of other occurrences. 
      ## If there is more than one row, choose one at random to remove
      RemoveRec <- which(SumVec == max(SumVec))
      if (length(RemoveRec) > 1) {
        RemoveRec <- sample(RemoveRec, 1)
      }
      
      ## Assuming the row chosen above is removed, decrease the 
      ## SumVec object by how many other rows are influenced by its removal
      SumVec <- SumVec - DistMat[, RemoveRec]
      
      ## Set the SumVec value for the row to be removed equal to 0
      SumVec[RemoveRec] <- 0L
      
      ## Set the occ to be ignored in the next iteration of the while loop
      DistMat[RemoveRec, ] <- FALSE
      DistMat[, RemoveRec] <- FALSE
      
      ## Note the occurence for removal from the thinned data set
      df.keep[RemoveRec] <- FALSE
    }
    
    ## Make the new, thinned, data set
    rec.df <- rec.df.orig[df.keep, , drop=FALSE]
    colnames(rec.df) <- c("Longitude", "Latitude")
    reduced.rec.dfs[[Rep]] <- rec.df
  }
  
  ## Order the list object of thinned records by most records
  ## to least
  reduced.rec.order <- unlist(lapply(reduced.rec.dfs, nrow))
  reduced.rec.order <- order(reduced.rec.order, decreasing = TRUE)
  reduced.rec.dfs <- reduced.rec.dfs[reduced.rec.order]
  
  return(reduced.rec.dfs)
}

