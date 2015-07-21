data(Heteromys_anomalus_South_America)

# test rcpp_filter_algorithm function
test_that('SpFilter: rcpp_filter_algorithm function doesn\'t work', {
	result<-spThin:::rcpp_filter_algorithm(
		split(
			seq_len(10),
			sample(rep(seq_len(5),2))
		),
		1
	)
})

# test filter function
test_that('SpFilter: filter function doesn\'t work', {
	result<-spFilter(
		Heteromys_anomalus_South_America[1:5,],
		x.col = "LONG", 
        y.col = "LAT",
        0.1,
		10
	)
})

# test methods
test_that('SpFilter: samples method doesn\'t work', {
	result<-spFilter(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
        0.1,
		10
	)
	x<-samples(result, 1)
})

test_that('SpFilter: nrep method doesn\'t work', {
	result<-spFilter(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
        0.1,
		10
	)
	x=nrep(result)
})


test_that('SpFilter: fulldata method doesn\'t work', {
	result<-spFilter(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
        0.1,
		10
	)
	x=fulldata(result)
})

test_that('SpFilter: cellsize method doesn\'t work', {
	result<-spFilter(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
        0.1,
		10
	)
	x=cellsize(result)
})

test_that('SpFilter: write.SpFilter method doesn\'t work', {
	result<-spFilter(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
        0.1,
		10
	)
	x=write.SpFilter(result, dir=tempdir())
})



# test summary method
test_that('summary method doesn\'t work', {
	result<-spFilter(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
        0.1,
		10
	)
	summary(result)
})

# test plot method
test_that('plot method doesn\'t work', {
	result<-suppressWarnings(spFilter(
		Heteromys_anomalus_South_America,
		x.col = "LONG", 
        y.col = "LAT",
        0.1,
		10
	))
	plot(result)
})


