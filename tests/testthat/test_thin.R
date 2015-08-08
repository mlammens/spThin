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
		cat('gurobi R package is not installed.\n')
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
		Heteromys_anomalus_South_America[["LONG"]],
        Heteromys_anomalus_South_America[["LAT"]],
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
		cat('gurobi R package is not installed.\n')
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
	result
})

# test thindist method
test_that('SpThin: thindist method doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
		method='heuristic',		
        10000,
		10,
		great.circle.distance=TRUE
	)
	thindist(result)
})

# test distname method
test_that('SpThin: distname method doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
		method='heuristic',		
        10000,
		10,
		great.circle.distance=TRUE
	)
	distname(result)
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
	result
})



# test plot method
test_that('SpThin: plot method doesn\'t work', {
	
	# make plots with heuristic
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
	plot(result, which=1)
	plot(result, which=1:2)
	plot(result, which=1:3)
	
	# make plot with lpsolve
	result<-spThin(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
		method='lpsolve',
        10000,
		10,
		great.circle.distance=TRUE
	)
	plot(result)
	
	# kill plot window
	dev.off()
})


