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
Rcpp::List rcpp_thin_algorithm(std::vector<double> x, std::vector<double> y, double thin_par, int reps, bool great_circle_distance) {
	/// init
	// declare objects
	int currSite;
	int nSites=x.size();
	int nRemainingSites;
	std::vector<std::size_t> currSiteCounts(nSites);
	std::vector<std::size_t> idMaxCounts(nSites);
	std::vector<std::size_t> idRemainingSites(nSites);
	Eigen::Matrix<bool, Eigen::Dynamic, Eigen::Dynamic> greaterThanDist(nSites,nSites);
	Eigen::Matrix<bool, Eigen::Dynamic, Eigen::Dynamic> greaterThanDist_backup(nSites,nSites);
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
				greaterThanDist_backup(i,j)=GcDistanceInMeters(x[i], y[i], x[j], y[j]) < thin_par;
				greaterThanDist_backup(j,i)=greaterThanDist_backup(i,j);
			}
		}
	} else {
		// use euclidean circle distances
		for (int i=0; i<(nSites-1); ++i) {
			for (int j=(i+1); j<nSites; ++j) {
				greaterThanDist_backup(i,j)=EucDistanceInMeters(x[i], y[i], x[j], y[j]) < thin_par;
				greaterThanDist_backup(j,i)=greaterThanDist_backup(i,j);
			}
		}
	}
	for (int i=0; i<nSites; ++i) {
		greaterThanDist_backup(i,i)=false;
	}
	Rcpp::checkUserInterrupt();	
	
	/// main processing
	for (int r=0; r<reps; ++r) {
		// reset parameters for new rep
		nRemainingSites=nSites;
		std::iota(idRemainingSites.begin(), idRemainingSites.end(), 0);
		greaterThanDist=greaterThanDist_backup;
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
			
			// check for user interrupt
			if (nRemainingSites % NCHECKUSERINPUT == 0)
				Rcpp::checkUserInterrupt();
			
		}
		
		// store results
		sites[r].reserve(nRemainingSites);
		for (auto i=idRemainingSites.cbegin(); i!=idRemainingSites.cbegin()+nRemainingSites; ++i)
			sites[r].push_back((*i) + 1);		
	}
	
	/// exports
	return(Rcpp::wrap(sites));
}

