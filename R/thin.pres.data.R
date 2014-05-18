#' @title A spatial thinning algorithm
#'
#'   @param rec.df.orig: A data frame of long/lat points for each presence record. The 
#'   data.frame should be a two-column data frame, one column of long and one of 
#'   lat
#'
#'   @param thin.par: Thinning parameter - the distance (in kilometers) that you want
#'   records to be separated by.
#'
#'   @param reps: The number of times to repete the thinning process. Given the random
#'   process of removing nearest-neighbors there should be 'rep' number of different
#'   sets of coordinates.
#'   @return reduced.rec.dfs: A list object of length 'rep'. Each list element is a different
#'   data.frame of spatially thinned presence records.
thin.pres.data <- function( rec.df.orig, thin.par, reps ) {
  ##
  ## General Algorithm:
  ## ******************
  ## While at least one member of the nearest neighbor vector is less than 10 km
  ## remove one of the points associated with nearest neighbor less than 10km randomly
  ##
  ## A few more details:
  ## 1. Define thinning parameter
  ## 2. Find all points with nearest neighbor closer than thinning parameter
  ## 3. Randomly delete one of these points
  ## 4. Continue until no points have nearest neighbor closer than thinning parameter
  ## 5. Repeat many times 
  ##      
  require( fields )
  
  reduced.rec.dfs <- list()
  
  # Value for which neighber is too close - i.e. thinning metric
  print( paste( "Thinning data using thin.par: ", thin.par ) )
  
  for ( Rep in 1:reps ){
    #print( paste( "Repetition: ", Rep ) ) ### Debug Line - uncomment to print repetition number
    rec.df <- rec.df.orig

    # Make distance matrix
    DistMat <- rdist.earth(x1=rec.df,miles=FALSE)
    # print(DistMat) ### DEBUG LINE - warning this could be very large!
    # Replace diagonal (which is all zeroes) with NAs
    diag(DistMat) <- NA
    # For each record, calculate nearest neighbor
    NearNeighbor <- apply( DistMat, MARGIN=2, FUN=min, na.rm=TRUE )
    MinNearNeighbor <- min(NearNeighbor)
    # print(MinNearNeighbor) ### DEBUG LINE
    
    while( MinNearNeighbor<thin.par & nrow(rec.df)>1 ){
      
      # Determine records with a minimum nearest neighbor distance less than thin.par 
      CloseRecs <- which( NearNeighbor < thin.par )
      # print( paste( 'Length of CloseRecs: ',length(CloseRecs)) ) ### DEBUG LINE
      
      # Choose one record to remove randomly
      RemoveRec <- sample(CloseRecs,1)
      # print( paste( 'RemoveRec: ',RemoveRec) ) ### DEBUG LINE
      
      # Remove random record from the complete records data.frame rec.df
      rec.df <-rec.df[ -RemoveRec, ]
      # print( paste( 'Number of remaining records: ', nrow(rec.df) )) ### DEBUG LINE
      
      # Remove random record from the distance matrix DistMat
      DistMat <- DistMat[ -RemoveRec, -RemoveRec ]

      # Calculate NEW nearest neighbors **UNLESS* there is only one location
      # remaining. 
      if ( nrow(rec.df)>1 ){
        NearNeighbor <- apply( DistMat, MARGIN=2, FUN=min, na.rm=TRUE )
        MinNearNeighbor <- min(NearNeighbor)
        # print(paste( 'MinNearNeighbor inside loop: ', MinNearNeighbor) ) DEBUG LINE
      }
    }
    reduced.rec.dfs[[Rep]] <- rec.df
  }
  return( reduced.rec.dfs )
}