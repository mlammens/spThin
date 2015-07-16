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

# # benchmark implementation
# test_that('rcpp_thin_algorithm is slow', {
	# size=1e02
	# x=runif(size, -3, 3)
	# y=runif(size, -3, 3)
	# system.time(
		# spThin:::rcpp_thin_algorithm(
			# x,
			# y,
			# 2,
			# 1,
			# TRUE
		# )
	# )
# })


# test thin function
test_that('SpThin: thin function doesn\'t work', {
	result<-spThin(
		Heteromys_anomalus_South_America,
		lon.col = "LONG", 
        lat.col = "LAT",
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
        200000,
		10
	)
	plot(result)
	dev.off()
})


