\name{thin.pres.data}
\alias{thin.pres.data}
\title{Implements random spatial thinning algorithm}
\usage{
thin.pres.data(rec.df.orig, thin.par, reps)
}
\arguments{
  \item{rec.df.orig:}{A data frame of long/lat points for
  each presence record. The data.frame should be a
  two-column data frame, one column of long and one of lat}

  \item{thin.par:}{Thinning parameter - the distance (in
  kilometers) that you want records to be separated by.}

  \item{reps:}{The number of times to repete the thinning
  process. Given the random process of removing
  nearest-neighbors there should be 'rep' number of
  different sets of coordinates.}
}
\value{
reduced.rec.dfs: A list object of length 'rep'. Each list
element is a different data.frame of spatially thinned
presence records.
}
\description{
\code{thin.pres.data} implements a randomization approach
to spatially thinning species occurence data. It is the
algorithm underlying the \code{\link{spThin}} function.
}

