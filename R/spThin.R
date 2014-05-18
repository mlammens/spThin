#' @title A 'wrapper' function for the spatial thinning algorithm
#'
#'   @param loc.file: A *.csv file of locations. This file can include several
#'   columnns, but must include at minimum a column of latitude and a
#'   column of longitude values
#'   @param lat.col: Name of column of latitude values. Caps sensitive.
#'   @param long.col: Name of column of longitude values. Caps sensitive.
#'   @param spec.col: Name of column of species name. Caps sensitive.
#'   @param thin.par: Thinning parameter - the distance (in kilometers) that you want
#'   records to be separated by.
#'   @param reps: The number of times to repete the thinning process. Given the random
#'   process of removing nearest-neighbors there should be 'rep' number of different
#'   sets of coordinates.
#'   @param locs.thinned.list.return: TRUE/FALSE - If true, the `list` of 
#'   the data.frame of thinned locs resulting from each replication 
#'   is returned (see Returns below).
#'   @param write.files: TRUE/FALSE - If true, new *.csv files will be 
#'   written with the thinned locs data
#' @param max.files: The maximum number of *csv files to be written based on the
#'   thinned data
#' @param out.dir: Directory to write new *csv files to
#' @param log.file: Text log file 
#' @param thin.pred.data: Location of thin.pres.data.R file.
#' @return locs.thinned.dfs: A list of data.frames, each data.frame
#'   the spatially thinned locations of the algorithm for a 
#'   single replication. This list will have `reps` elements.
#'
spThin <- function( loc.file, lat.col="LAT", long.col="LONG", spec.col="SPEC",
                    thin.par, reps,
                    locs.thinned.list.return=FALSE,
                    write.files=TRUE, max.files=5, out.dir, 
                    log.file='spatial_thin_log.txt' ){ 
  #,
  # thin.pres.data='thin.pres.data.R') {
  
  ## Source thin.pres.data.R function - change this structure in 
  ## the future to allow for all functions to be sourced at once
#   if( !file.exists(thin.pres.data) ){
#     log.thin.pres.data <- paste( "\nERROR: Did not find thin.pres.data.R file:",
#                                  thin.pres.data)
#     write(log.thin.pres.data, file=log.file, append=TRUE)
#     stop(log.thin.pres.data)
#   }
#   #source(thin.pres.data)
  
  log.begin <- paste("**********************************************","\n",
                     "Beginning Spatial Thinning of location data in:",
                     loc.file,"\n",
                     "Script Started at:",
                     date(), sep=" ")
  ## Print information to the console
  cat(log.begin)
  ## Write information to the log.file
  write( log.begin, file=log.file, append = TRUE )
  
  ## Read in the loc.file
  # Check if file exists
  if ( !file.exists( loc.file ) ){
    log.loc.file <- paste( "\nERROR: '", loc.file, 
                           "' not found. Check file name and path.", sep="" )
    write( log.loc.file, file=log.file, append = TRUE )
    stop( log.loc.file )
  }
  locs.df <- read.csv( loc.file )
  
  ## Get the species name used in the `locs.df`
  species <- unique( locs.df[[ which( names(locs.df) == spec.col ) ]] )
  ## Send a warning message if there are more than one species in the df
  if( length( species ) > 1 ){
    log.spec.warn.1 <- "WARNING: There appear to be more than one species name in this *.csv file."
    print(log.spec.warn.1)
    write( log.spec.warn.1, file=log.file, append=TRUE )
    species <- species[1]
    log.spec.warn.2 <- paste( "Only using species name:", species )
    print(log.spec.warn.2)
    write( log.spec.warn.2, file=log.file, append=TRUE )
  }
  
  ## Determine the columns associated with Lat and Long
  lat <- which( names(locs.df)==lat.col)
  long <- which( names(locs.df)==long.col)
  
  ## Make a data.frame that contains only the Long and 
  ## Lat values, in that order (ie df$Long, df$Lat)
  locs.long.lat <- as.data.frame(cbind( locs.df[[long]], locs.df[[lat]] ))

  ## Note in the log file what thinning parameter is being used
  log.thin.par <- paste("\nThinning Parameter Used (in km):", thin.par)
  write( log.thin.par, file=log.file, append = TRUE )
  log.num.reps <- paste("Number of replicates of thinning script:", reps )
  write( log.num.reps, file=log.file, append = TRUE )
  
  ## Execute spatial thinning function `thin.pres.data.R`. This
  ## function returns a `list` of spatially thinned data.frames
  
  # Keep track of how much time it takes to run this algorithm
  thin.time <- system.time( 
    locs.thinned <- thin.pres.data( rec.df.orig=locs.long.lat,
                                    thin.par=thin.par, reps=reps )
  )
  
  ## Record in log file elapsed system time for running the script
  write( "\nElapsed time for thinning completion", file=log.file, append = TRUE )
  write( thin.time, file=log.file, append = TRUE )
    
  ## Look at the number of locs kept in each thinned dataset
  ## by determining the number of rows in each returned data.frame
  lat.long.thin.count <- unlist(lapply(locs.thinned, nrow ))

  ## Create a vector of cummulative maximum records at each 
  ## repetition number
  cummax.lat.long.thin.count <- cummax(lat.long.thin.count)
  ## Plot the number of repetitions versus the number 
  ## of maximum records retained at each repetition
  plot( (1:reps), cummax.lat.long.thin.count,
        xlab='Number Repetitions',
        ylab='Cummulative Maximum Records Retained',
        #ylim=c(0,max(cummax.lat.long.thin.count)),  
        xlim=c(0,reps) )
  ## Make a log-log plot of the number of repetitions versus
  ## the number of maximum records retained
  plot( log(1:reps), log(cummax.lat.long.thin.count),
        xlab='Log Number Repetitions',
        ylab='Log Cummulative Maximum Records Retained',
        #ylim=c(0,log(max(cummax.lat.long.thin.count))), 
        xlim=c(0,log(reps)) )
  
  ## Use `table` to deterine number of dfs for each
  ## locs count
  locs.thinned.tbl <- table(lat.long.thin.count)
  ## Print `locs.thinned.tbl` to console
  print(locs.thinned.tbl)
  ## Print `locs.thinned.tbl` to log file
  write("\nNumber of data.frames per locations retained\nloc.cnt df.freq",
        file=log.file, append=TRUE)
  write(names(locs.thinned.tbl),file=log.file, append=TRUE, 
        ncolumns=length(names(locs.thinned.tbl)),sep="\t")
  write(locs.thinned.tbl, file=log.file, append=TRUE, 
        ncolumns=length(locs.thinned.tbl),sep="\t")
  ## Plot a histogram of lat.long.thin.count
  hist(lat.long.thin.count)
  
  ## Find max number of records
  max.thin.recs <- max( lat.long.thin.count)
  ## Save to log and Print this out for user to see
  log.max.rec <- paste( "\nMaximum number of records after thinning:\n", max.thin.recs)
  print(log.max.rec)
  write(log.max.rec, file=log.file, append=TRUE)
  
  ## Determine which data.frames
  ## have max.no. records
  max.dfs <- which( lat.long.thin.count == max.thin.recs)
  max.dfs.length <- length(max.dfs)
  log.max.df.cnt <- paste( "Number of data.frames with max records:\n", 
                           max.dfs.length)
  print(log.max.df.cnt)
  write(log.max.df.cnt, file=log.file, append=TRUE)
  
  ## Write files if `write.files==TRUE`
  if( write.files ){
    print("Writing new *.csv files")
    write("\n**New *.csv file creation:**", file=log.file, append=TRUE)
    
    # Determine number of files to write - should be the min
    # of the max number requested and the `max.dfs.lenght`
    n.csv <- min( c(max.files, max.dfs.length) )
    
    ## Write the first `n.csv` max data.frames
    # Check that `out.dir` exists. If not, create this directory.
    if ( !file.exists( out.dir ) ) {
      log.dir <- paste('\nWARNING: Created new output directory: ', out.dir, sep="") 
      dir.create( out.dir, recursive=TRUE )
    } else {
      log.dir <- paste('Writing new *.csv files to output directory: ', out.dir, sep="")
    }
    print(log.dir)
    write(log.dir, file=log.file, append=TRUE)
    # Check that `out.dir` terminates in a '/'
    if( !grepl( '/$', out.dir ) ){
      out.dir <- paste( out.dir, '/', sep='' )
    }
    
    ## Make a file base name
    # Use the same base name of the `loc.file`
    csv.base <- basename( loc.file )
    # Remove csv tag
    csv.base <- sub( '.csv$','', csv.base )
    # Make new csv file names
    csv.files <- paste( out.dir, csv.base, "_thin", rep(1:n.csv), 
                        ".csv", sep="")
    
    for ( df in 1:n.csv ){
      # Get long and lat values for thinned locs
      df.temp <- locs.thinned[[ max.dfs[df] ]]
      # Add column of species name
      df.temp <- cbind( rep(as.character(species), max.thin.recs),
                        df.temp )
      # Give columns names
      colnames(df.temp) <- c(spec.col, long.col, lat.col)
      # Change file name in case of possible overwrite 
      # and send a warning to the user
      while( file.exists( csv.files[df] ) ){
        # Change file name
        csv.files[df] <- sub( '.csv$', '_new.csv', csv.files[df] )
        log.csv.overwrite <- paste("\nWARNING: '", csv.files[df], 
                                   "' already exists. Renaming file to avoid overwriting.")
        print(log.csv.overwrite)
        write(log.csv.overwrite, file=log.file, append=TRUE )
      }
      # Write new *.csv file with new name
      write.csv( df.temp, file=csv.files[df], quote=FALSE,
                 row.names=FALSE)
      log.write.file <- paste("Writing file:",csv.files[df])
      print(log.write.file)
      write( log.write.file, file=log.file, append=TRUE )
    }
    
  } else {
    log.write.file <- "\nNo files written for this run."
    print(log.write.file)
    write(log.write.file, file=log.file, append=TRUE)
  }
  
  ## Return `locs.thinned.list` if that setting is TRUE
  if (locs.thinned.list.return){
    return( locs.thinned )
  }
}
