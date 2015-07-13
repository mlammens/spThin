data(Heteromys_anomalus_South_America)

# test rcpp_thin_algorithm function
test_that('rcpp_thin_algorithm function doesn\'t work', {
	result=spThin:::rcpp_thin_algorithm(
		Heteromys_anomalus_South_America[[3]],
		Heteromys_anomalus_South_America[[2]],
		100,
		1
	)
	dists=fields::rdist.earth(
		Heteromys_anomalus_South_America[result[[1]],3:2],
		miles=FALSE
	)	
	expect_true(
		all(
			dists[lower.tri(dists)] > 100
		)
	)
	
})

# test thin function
test_that('thin function doesn\'t work', {
	result=thin(
		loc.data = Heteromys_anomalus_South_America,
        lat.col = "LAT",
		long.col = "LONG", 
        spec.col = "SPEC", 
        thin.par = 100,
		reps = 1,
        locs.thinned.list.return = TRUE, 
        write.files = FALSE
	)
	dists=fields::rdist.earth(result[[1]], miles=FALSE)
	
	expect_true(
		all(
			dists[lower.tri(dists)] > 100
		)
	)
	
})

# test summary method
test_that('summary method doesn\'t work', {
	result=thin(
		loc.data = Heteromys_anomalus_South_America,
        lat.col = "LAT",
		long.col = "LONG", 
        spec.col = "SPEC", 
        thin.par = 10,
		reps = 2,
        locs.thinned.list.return = TRUE, 
        write.files = FALSE
	)
	summary(result)
})

# test plot method
test_that('plot method doesn\'t work', {
	result=thin(
		loc.data = Heteromys_anomalus_South_America,
        lat.col = "LAT",
		long.col = "LONG", 
        spec.col = "SPEC", 
        thin.par = 100,
		reps = 100,
        locs.thinned.list.return = TRUE, 
        write.files = FALSE
	)
	plot(result)
	# dev.off()
})


