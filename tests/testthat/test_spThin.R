data(Heteromys_anomalus_South_America)

# test rcpp_thin_algorithm function
test_that('rcpp_thin_algorithm function doesn\'t work', {
	result=spThin:::rcpp_thin_algorithm(
		Heteromys_anomalus_South_America[[3]],
		Heteromys_anomalus_South_America[[2]],
		200000,
		100,
		TRUE
	)
	dists=fields::rdist.earth(
		Heteromys_anomalus_South_America[result[[1]],3:2],
		miles=FALSE
	)	
	expect_true(
		all(
			dists[lower.tri(dists)] > 200
		)
	)
	
})

# test thin function
test_that('thin function doesn\'t work', {
	result=thin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
        200000,
		10
	)
	dists=fields::rdist.earth(
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
test_that('samples method doesn\'t work', {
	result=thin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
        200000,
		10
	)
	x=samples(result, 1)
})

test_that('nrep method doesn\'t work', {
	result=thin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
        200000,
		10
	)
	x=nrep(result)
})

test_that('call method doesn\'t work', {
	result=thin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
        200000,
		10
	)
	x=call(result)

})

test_that('data method doesn\'t work', {
	result=thin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
        200000,
		10
	)
	x=data(result)
})

test_that('mindist method doesn\'t work', {
	result=thin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
        200000,
		10
	)
	x=mindist(result)
})

test_that('write method doesn\'t work', {
	result=thin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
        200000,
		10
	)
	x=write(result)
})



# test summary method
test_that('summary method doesn\'t work', {
	result=thin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
        200000,
		10
	)
	summary(result)
})

# test plot method
test_that('plot method doesn\'t work', {
	result=thin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
        200000,
		10
	)
	plot(result)
	dev.off()
})


