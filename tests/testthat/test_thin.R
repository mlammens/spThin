data(Heteromys_anomalus_South_America)

# test rcpp_thin_algorithm function
test_that('SpThin: rcpp_thin_algorithm function doesn\'t work', {
	result<-spThin:::rcpp_thin_algorithm(
		Heteromys_anomalus_South_America[[3]],
		Heteromys_anomalus_South_America[[2]],
		200000,
		1,
		TRUE
	)
	dists<-fields::rdist.earth(
		Heteromys_anomalus_South_America[result[[1]],3:2],
		miles=FALSE
	)		
	expect_true(
		all(
			dists[lower.tri(dists)] > 200
		)
	)
})

test_that('SpThin: thin_lpsolve function doesn\'t work', {
	result<-spThin:::thin_lpsolve(
		Heteromys_anomalus_South_America[[3]][1:5],
		Heteromys_anomalus_South_America[[2]][1:5],
		200000,
		1,
		TRUE
	)
	dists<-fields::rdist.earth(
		Heteromys_anomalus_South_America[result[[1]],3:2],
		miles=FALSE
	)		
	expect_true(
		all(
			dists[lower.tri(dists)] > 200
		)
	)
})


test_that('SpThin: thin_gurobi function doesn\'t work', {
	if (require(gurobi)) {
		result<-spThin:::thin_gurobi(
			Heteromys_anomalus_South_America[[3]],
			Heteromys_anomalus_South_America[[2]],
			200000,
			1,
			TRUE
		)
		dists<-fields::rdist.earth(
			Heteromys_anomalus_South_America[result[[1]],3:2],
			miles=FALSE
		)		
		expect_true(
			all(
				dists[lower.tri(dists)] > 200
			)
		)
	} else {
		warning('gurobi R package not installed')
	}
})

# test thin function
test_that('SpThin: thin function doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
		method='heuristic',		
        200000,
		10
	)
	dists<-fields::rdist.earth(
		Heteromys_anomalus_South_America[result@samples[[1]],c("LONG", "LAT")], 
		miles=FALSE
	)
	expect_true(
		all(
			dists[lower.tri(dists)] > 200
		)
	)
	
})

# test methods
test_that('SpThin: samples method doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
		method='heuristic',		
        200000,
		10
	)
	x<-samples(result, 1)
})

test_that('SpThin: nrep method doesn\'t work', {
	result=spThin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
		method='heuristic',
        200000,
		10
	)
	x=nrep(result)
})

test_that('SpThin: call method doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
		method='heuristic',		
        200000,
		10
	)
	x<-call(result)

})

test_that('SpThin: data method doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
		method='heuristic',		
        200000,
		10
	)
	x=data(result)
})

test_that('SpThin: mindist method doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
		method='heuristic',		
        200000,
		10
	)
	x=mindist(result)
})

test_that('SpThin: write method doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
		method='heuristic',		
        200000,
		10
	)
	x=write(result, dir=tempdir())
})



# test summary method
test_that('SpThin: summary method doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
		method='heuristic',		
        200000,
		10
	)
	summary(result)
})

# test plot method
test_that('SpThin: plot method doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
		method='heuristic',		
        200000,
		10
	)
	plot(result)
	dev.off()
})


