#include <Rcpp.h>
#include <RcppEigen.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

#include <vector>
#include <algorithm>
#include <limits>
#include <cmath>

#include "functions.h"

// [[Rcpp::export]]
Rcpp::NumericVector rcpp_get_mindists(std::vector<double> x, std::vector<double> y, bool great_circle_distance, Rcpp::List solutions) {
	int nSolutions=solutions.size();
	double currDist;
	double nSites;
	double maxDBL=std::numeric_limits<double>::max();
	Rcpp::NumericVector mindists(nSolutions, maxDBL);
	Rcpp::NumericVector tmp;
	
	/// create distance matrix
	// fill upper triangle with distances and use this for computation
	if (great_circle_distance) {
		// use great circle distances
		for (int r=0; r<nSolutions; ++r) {
			tmp=as<Rcpp::NumericVector>(solutions[r]);
			nSites=tmp.size();
			for (int i=0; i<(nSites-1); ++i) {
				for (int j=(i+1); j<nSites; ++j) {
					currDist=GcDistanceInMeters(x[tmp[i]-1], y[tmp[i]-1], x[tmp[j]-1], y[tmp[j]-1]);
					if (currDist < mindists[r])
						mindists[r] = currDist;
				}
			}
		}
	} else {
		// use euclidean circle distances
		for (int r=0; r<nSolutions; ++r) {
			tmp=as<Rcpp::NumericVector>(solutions[r]);
			nSites=tmp.size();
			for (int i=0; i<(nSites-1); ++i) {
				for (int j=(i+1); j<nSites; ++j) {
					currDist=EucDistanceInMeters(x[tmp[i]-1], y[tmp[i]-1], x[tmp[j]-1], y[tmp[j]-1]);					
					if (currDist < mindists[r])
						mindists[r] = currDist;					
				}
			}
		}
	}
	//// exports
	return(mindists);
}