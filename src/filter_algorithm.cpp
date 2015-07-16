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


// random integer generator class
class Random {
	public:
		Random(int seed) {
			std::mt19937 tmp(seed); 
			eng=tmp;
		};
		std::size_t DrawDiscreteNumber(std::vector<std::size_t>::const_iterator begin, std::vector<std::size_t>::const_iterator end) {
			d1.param(std::discrete_distribution<std::size_t>::param_type(begin, end));
			return (d1(eng));
		}
		
		std::size_t DrawUniformNumber(std::size_t ub) {
			d2.param(std::uniform_int_distribution<std::size_t>::param_type(0, ub));
			return (d2(eng));
		}
		
	private:
		std::mt19937 eng;
		std::discrete_distribution<std::size_t> d1;
		std::uniform_int_distribution<std::size_t> d2;
};


// [[Rcpp::export]]
Rcpp::List rcpp_filter_algorithm(Rcpp::List inpLIST, std::size_t nrep) {
	/// init
	// declare objects
	int nSites=inpLIST.size();
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
		}
	}
	
	/// exports
	return(Rcpp::wrap(expLIST));
}

