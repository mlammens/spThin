spThin
============

#### spThin contains functions to spatially rarefy and thin species    occurrence data. These procedures can remove sampling bias, and in turn result in better ecological niche models. The package contains functions to thin datasets using exact-algorithm solvers ([lp_solve](http://lpsolve.sourceforge.net/) and [Gurobi](http://www.gurobi.com/) and a stingy heuristic. This package also contains functions to rarefy datasets using grids.

To install the sp Thin R package, execute the following commands in R:

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