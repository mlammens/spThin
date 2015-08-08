# Rscript C:\Users\jhanson\Documents\GitHub\spThin\admin\admin.R

# compile c++ attributes
library(Rcpp)
setwd(file.path(Sys.getenv('HOME'), 'github', 'spThin'))
compileAttributes()

# document code
setwd(file.path(Sys.getenv('HOME'), 'github', 'spThin'))
library(devtools)
library(roxygen2)
document()

# find obvious errors
setwd(file.path(Sys.getenv('HOME'), 'github', 'spThin'))
library(devtools)
library(roxygen2)
load_all() 

# make vignettes
library(devtools)
library(knitr)
setwd(file.path(Sys.getenv('HOME'), 'github', 'spThin'))
build_vignettes()

# formal package tests
setwd(file.path(Sys.getenv('HOME'), 'github', 'spThin'))
library(devtools)
library(roxygen2)
test()

# cran checks
setwd(file.path(Sys.getenv('HOME'), 'github', 'spThin'))
library(devtools)
library(roxygen2)
check()

# local install
library(devtools)
install_local(
	file.path(Sys.getenv('HOME'), 'github', 'spThin')
)

# # install from github
# library(devtools)
# install_github('paleo13/spThin')
