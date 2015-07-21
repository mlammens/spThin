#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

#include <vector>
#include <algorithm>
#include <limits>
#include <random>
#include <chrono>
#include <string>
#include <iostream>
#include <cmath>

#include "functions.h"
#include "Random.h"


// [[Rcpp::export]]
Rcpp::List rcpp_rarefy_algorithm(Rcpp::List inpLIST, std::size_t nrep) {
	/// init
	// declare objects
	std::size_t nSites=inpLIST.size();
	int seed=std::chrono::high_resolution_clock::now().time_since_epoch().count();
	std::vector<Rcpp::IntegerVector> sites(nSites);
	std::vector<std::vector<int>> expLIST(nrep, std::vector<int>(nSites));
	
	Random rgen(seed);

	/// preliminary processing
	for (std::size_t i=0; i<nSites; ++i) {
		sites[i]=Rcpp::as<Rcpp::IntegerVector>(inpLIST[i]);
	}
	
	/// main processing
	for (std::size_t r=0; r<nrep; ++r) {
		for (std::size_t i=0; i<nSites; ++i) {
			expLIST[r][i]=sites[i][rgen.DrawUniformNumber(sites[i].size()-1)];
			
			if (i % NCHECKUSERINPUT == 0)
				Rcpp::checkUserInterrupt();

		}
	}
	
	/// exports
	return(Rcpp::wrap(expLIST));
}

