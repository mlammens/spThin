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
#include <iostream>
#include <cmath>

#include "functions.h"
#include "Random.h"

// [[Rcpp::export]]
Rcpp::List rcpp_thin_algorithm(std::vector<double> lon, std::vector<double> lat, double thin_par, int reps, bool great_circle_distance) {
	/// init
	// declare objects
	int currSite;
	int nSites=lon.size();
	int nRemainingSites;
	std::vector<std::size_t> currSiteCounts(nSites);
	std::vector<std::size_t> idMaxCounts(nSites);
	std::vector<std::size_t> idRemainingSites(nSites);
	Eigen::Matrix<bool, Eigen::Dynamic, Eigen::Dynamic> greaterThanDist(nSites,nSites);
	std::vector<std::vector<int> > sites;
	sites.resize(reps);
	
	int seed=std::chrono::high_resolution_clock::now().time_since_epoch().count();
	Random rgen(seed);
	
	/// create distance matrix
	// fill upper triangle with distances and use this for computation
	if (great_circle_distance) {
		// use great circle distances
		for (int i=0; i<(nSites-1); ++i) {
			for (int j=(i+1); j<nSites; ++j) {
				greaterThanDist(i,j)=GcDistanceInMeters(lon[i], lat[i], lon[j], lat[j]) < thin_par;
				greaterThanDist(j,i)=greaterThanDist(i,j);
			}
		}
	} else {
		// use euclidean circle distances
		for (int i=0; i<(nSites-1); ++i) {
			for (int j=(i+1); j<nSites; ++j) {
				greaterThanDist(i,j)=EucDistanceInMeters(lon[i], lat[i], lon[j], lat[j]) < thin_par;
				greaterThanDist(j,i)=greaterThanDist(i,j);
			}
		}
	}
	for (int i=0; i<nSites; ++i) {
		greaterThanDist(i,i)=false;
	}
	
	/// main processing
	for (int r=0; r<reps; ++r) {
		// reset parameters for new rep
		nRemainingSites=nSites;
		std::iota(idRemainingSites.begin(), idRemainingSites.end(), 0);
		while (greaterThanDist.any() & (nRemainingSites > 1)) {				
			// find counts of sites within nearest distances
			for (int i=0; i<nRemainingSites; ++i)
				currSiteCounts[i]=greaterThanDist.col(idRemainingSites[i]).count();
						
			// remove site
			currSite=rgen.DrawDiscreteNumber(currSiteCounts.cbegin(), currSiteCounts.cbegin()+nRemainingSites);
			greaterThanDist.col(idRemainingSites[currSite]).setZero();
			greaterThanDist.row(idRemainingSites[currSite]).setZero();
			--nRemainingSites;
			std::iter_swap(idRemainingSites.begin()+currSite, idRemainingSites.begin()+nRemainingSites);
		}
		
		// store results
		sites[r].reserve(nRemainingSites);
		for (auto i=idRemainingSites.cbegin(); i!=idRemainingSites.cbegin()+nRemainingSites; ++i)
			sites[r].push_back((*i) + 1);
	}
	
	/// exports
	return(Rcpp::wrap(sites));
}

