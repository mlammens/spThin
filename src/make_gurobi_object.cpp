#include <Rcpp.h>
#include <RcppEigen.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

#include <vector>
#include <algorithm>
#include <limits>
#include <random>
#include <chrono>
#include <string>
#include <fstream>
#include <sstream>
#include <iostream>
#include <cmath>

#include "functions.h"

// [[Rcpp::export]]
Rcpp::List rcpp_make_gurobi_object(std::vector<double> lon, std::vector<double> lat, double thin_par, bool great_circle_distance) {
	/// init
	// declare objects
	std::size_t nSites=lon.size();
	std::size_t nSiteCombinations=0;
	Eigen::SparseMatrix<bool> greaterThanDist(nSites,nSites);
	std::vector<int> model_obj(nSites, 1);
		
	/// create distance matrix
	// fill upper triangle with distances and use this for computation
	if (great_circle_distance) {
		// use great circle distances
		for (std::size_t i=0; i<(nSites-1); ++i) {
			for (std::size_t j=(i+1); j<nSites; ++j) {
				if (GcDistanceInMeters(lon[i], lat[i], lon[j], lat[j]) < thin_par) {
					greaterThanDist.insert(i,j)=true;
				}
			}
		}
	} else {
		// use euclidean circle distances
		for (std::size_t i=0; i<(nSites-1); ++i) {
			for (std::size_t j=(i+1); j<nSites; ++j) {
				if (EucDistanceInMeters(lon[i], lat[i], lon[j], lat[j]) < thin_par) {
					greaterThanDist.insert(i,j)=true;					
				}
			}
		}
	}
	
	//// preliminary processing
	// define export objects
	nSiteCombinations=greaterThanDist.nonZeros();
	std::vector<int> model_A_i;
	model_A_i.reserve(nSiteCombinations);
	std::vector<int> model_A_j;
	model_A_j.reserve(nSiteCombinations);
	std::vector<int> model_A_x(nSiteCombinations, -1);
	std::vector<std::string> model_sense(nSiteCombinations, ">=");
	std::vector<int> model_rhs(nSiteCombinations, -1);
			
	//// main processing
	/// point combination constraints
	for (std::size_t i=0; i<greaterThanDist.outerSize(); ++i) {
		for (Eigen::SparseMatrix<bool>::InnerIterator it(greaterThanDist, i); it; ++it) {
			model_A_i.push_back(it.row()+1);
			model_A_j.push_back(it.col()+1);
		}
	}

	/// exports
	return(
		Rcpp::List::create(
			Rcpp::Named("i")=model_A_i,
			Rcpp::Named("j")=model_A_j,
			Rcpp::Named("x")=model_A_x,
			Rcpp::Named("nrow")=nSiteCombinations,
			Rcpp::Named("ncol")=nSites,
			Rcpp::Named("obj")=model_obj,
			Rcpp::Named("rhs")=model_rhs,
			Rcpp::Named("vtypes")="B",
			Rcpp::Named("sense")=model_sense
		)
	);
}

