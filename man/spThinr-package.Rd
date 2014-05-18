\name{spThinr-package}
\alias{spThinr-package}
\alias{spThinr}
\docType{package}
\title{
Spatially thin species occurence data
}
\description{
spThinr is a set of functions to spatially thin
species occurence data. The resulting thinned dataset 
can be used in ecological
modeling, such as ecological niche modeling.
}
\details{
\tabular{ll}{
Package: \tab spThinr\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2014-05-18\cr
License: \tab GPL-3\cr
}
spThinr includes two functions, \code{thin.pres.data} and
\code{spThin}. \code{thin.pres.data} implements a randomization
approach to thinning spatial datasets given a user defined
minimum nearest neighbor distance. \code{spThin} is a wrapper
function for \code{thin.pres.data} that facilitates the writing
of CSV files for resulting datasets and drawing of diagnostic
plots. Additionally, a log file is written to file during
implementation.
}
\author{
Matthew Aiello-Lammens

Maintainer: Matthew Aiello-Lammens <matt.lammens@gmail.com>
}
\references{
Aiello-Lammens, ME, Boria, RA, Radosavljevic, A, and Anderson, RP.
2014. spThinr: An R package for spatial thinning of species occurence
records for use in ecological niche modeling. In Preparation.
}
\keyword{ package }
\seealso{

}
\examples{

}
