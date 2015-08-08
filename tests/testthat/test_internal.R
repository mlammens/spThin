# test grbiWrapper function
test_that('internal: grbiWrapper function doesn\'t work', {
	result<-spThin:::grbiWrapper(
		spThin:::rcpp_make_gurobi_object(
			seq(0,10,0.5),
			seq(0,10,0.5),
			1,
			FALSE
		)
	)
	expect_identical(
		as.character(dput(result)), 
		c(
			"<S4 object of class \"dgCMatrix\">", 
			"c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)", 
			"c(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)", 
			"B", "c(\">=\", \">=\", \">=\", \">=\", \">=\", \">=\", \">=\", \">=\", \">=\", \">=\", \">=\", \">=\", \">=\", \">=\", \">=\", \">=\", \">=\", \">=\", \">=\", \">=\")", 
			"max"
		)
	)
})

