spThin
============

[![Build Status](https://travis-ci.org/paleo13/spThin.svg?branch=master)](https://travis-ci.org/paleo13/spThin)
[![Coverage Status](https://coveralls.io/repos/paleo13/spThin/badge.svg?branch=master&service=github)](https://coveralls.io/github/paleo13/spThin?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/spThin)](http://cran.r-project.org/package=spThin)

#### spThin contains functions to spatially rarefy and thin species occurrence data. These procedures can ameliorate sampling bias, and in turn result in better ecological niche models. The package contains functions to thin datasets using exact-algorithm solvers ([lp_solve](http://lpsolve.sourceforge.net/) and [Gurobi](http://www.gurobi.com/)) and a stingy heuristic. This package also contains functions to rarefy datasets using grids.

Linux and Mac OSX users will first need to install several dependencies. To install them, type the following code in the bash or the terminal:

```
sudo apt-get update
sudo apt-get install libgdal-dev
sudo apt-get install libproj-dev
```

To install the spThin R package, execute the following commands in R:

```
if (!require('devtools'))
	install.packages('devtools', repo='http://cran.rstudio.com', dep=TRUE)
devtools:::install_github('mlammens/spThin')
```

Once this package has been installed, you can explore the functions of this package by reading through the vignette. You can access it in R by running the code below:

```
# open vignette in web browser
vignette('spThin', package='spThin')
```

**If this R package helped you, please cite it.**

M.E. Aiello-Lammens, R.A. Boria, A. Radosavljevic, B. Vilela, R.P. Anderson (2015). spThin: an R package for spatial thinning of species occurrence records for use in ecological niche models. Ecography, **38**: 0–5.
