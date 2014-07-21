\name{thin}
\alias{thin}
\title{Spatially thin species occurence data}
\usage{
thin(loc.file, lat.col = "LAT", long.col = "LONG", spec.col = "SPEC",
  thin.par, reps, locs.thinned.list.return = FALSE, write.files = TRUE,
  max.files = 5, out.dir, log.file = "spatial_thin_log.txt")
}
\arguments{
  \item{loc.file:}{A *.csv file of locations. This file can
  include several columnns, but must include at minimum a
  column of latitude and a column of longitude values}

  \item{lat.col:}{Name of column of latitude values. Caps
  sensitive.}

  \item{long.col:}{Name of column of longitude values. Caps
  sensitive.}

  \item{spec.col:}{Name of column of species name. Caps
  sensitive.}

  \item{thin.par:}{Thinning parameter - the distance (in
  kilometers) that you want records to be separated by.}

  \item{reps:}{The number of times to repete the thinning
  process. Given the random process of removing
  nearest-neighbors there should be 'rep' number of
  different sets of coordinates.}

  \item{locs.thinned.list.return:}{TRUE/FALSE - If true,
  the `list` of the data.frame of thinned locs resulting
  from each replication is returned (see Returns below).}

  \item{write.files:}{TRUE/FALSE - If true, new *.csv files
  will be written with the thinned locs data}

  \item{max.files:}{The maximum number of *csv files to be
  written based on the thinned data}

  \item{out.dir:}{Directory to write new *csv files to}

  \item{log.file:}{Text log file}

  \item{thin.pred.data:}{Location of thin.pres.data.R
  file.}
}
\value{
locs.thinned.dfs: A list of data.frames, each data.frame
the spatially thinned locations of the algorithm for a
single replication. This list will have `reps` elements.
}
\description{
\code{thin} returns spatially thinned species occurence
data sets. A randomizaiton algorithm
(\code{\link{thin.algorithm}}) is used to create data set
in which all occurnece locations are at least
\code{thin.par} distance apart. Spatial thinning helps to
reduce the effect of uneven, or biased, species occurence
collections on spatial model outcomes.
}
\seealso{
\code{\link{thin.algorithm}}
}

