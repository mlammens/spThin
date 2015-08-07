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
	if (spThin:::is.installed("gurobi")) {
		result<-spThin:::thin_gurobi(
			Heteromys_anomalus_South_America[[3]],
			Heteromys_anomalus_South_America[[2]],
			200000,
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
test_that('SpThin: thin(method="lpsolve") function doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
		method='lpsolve',		
        10000,
		10,
		great.circle.distance=TRUE,
		timeout=10
	)
})

test_that('SpThin: thin(method="heuristic") function doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
		method='heuristic',		
        10000,
		10,
		great.circle.distance=TRUE
	)
	dists<-fields::rdist.earth(
		Heteromys_anomalus_South_America[result@samples[[1]],c("LONG", "LAT")], 
		miles=FALSE
	)
	expect_true(
		all(
			dists[lower.tri(dists)] > 10
		)
	)
	
})

test_that('SpThin: thin(method="gurobi") function doesn\'t work', {
	if (spThin:::is.installed('gurobi')) {
		result<-spThin(
			Heteromys_anomalus_South_America,
			x.col = "LONG", 
			y.col = "LAT",
			method='gurobi',		
			10000,
			10,
			great.circle.distance=TRUE
		)
		dists<-fields::rdist.earth(
			Heteromys_anomalus_South_America[result@samples[[1]],c("LONG", "LAT")], 
			miles=FALSE
		)
		expect_true(
			all(dists[lower.tri(dists)] > 10) & 
			length(result@samples[[1]])==124
		)
	} else {
		warning('gurobi R package is not installed.')
	}
})

# test methods
test_that('SpThin: [[ method doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
		method='heuristic',		
        200000,
		10
	)
	x<-result[[1]]
})

test_that('SpThin: nrep method doesn\'t work', {
	result=spThin(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
		method='heuristic',
        10000,
		10,
		great.circle.distance=TRUE
	)
	x=nrep(result)
})

test_that('SpThin: fulldata method doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
		method='heuristic',		
        10000,
		10,
		great.circle.distance=TRUE
	)
	x=fulldata(result)
})

test_that('SpThin: mindist method doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
		method='heuristic',		
        10000,
		10,
		great.circle.distance=TRUE
	)
	x=mindist(result)
})

test_that('SpThin: write.SpThin method doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
		method='heuristic',		
        10000,
		10,
		great.circle.distance=TRUE
	)
	x=write.SpThin(result, dir=tempdir())
})


# test summary method
test_that('SpThin: summary method doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
		method='heuristic',		
        10000,
		10,
		great.circle.distance=TRUE
	)
	summary(result)
})

# test plot method
test_that('SpThin: plot method doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
		method='heuristic',		
        10000,
		10,
		great.circle.distance=TRUE
	)
	plot(result)
	dev.off()
})


