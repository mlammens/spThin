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

# make vignettes
# library(devtools)
# setwd(file.path(Sys.getenv('HOME'), 'github', 'spThin'))
# build_vignettes()

# find obvious errors
setwd(file.path(Sys.getenv('HOME'), 'github', 'spThin'))
library(devtools)
library(roxygen2)
load_all() 

# formal package tests
setwd(file.path(Sys.getenv('HOME'), 'github', 'spThin'))
library(devtools)
library(roxygen2)
test()

# # local install
# library(devtools)
# install_local(
	# file.path(Sys.getenv('HOME'), 'github', 'spThin')
# )

# # install from github
# library(devtools)
# install_github('paleo13/spThin')
