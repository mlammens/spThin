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
int rcpp_make_lpsolve_file(std::vector<double> lon, std::vector<double> lat, double thin_par, bool great_circle_distance, std::string filepath) {
	/// init
	// declare objects
	std::size_t nSites=lon.size();
	Eigen::SparseMatrix<bool> greaterThanDist(nSites,nSites);
	std::string lpmodel;
	std::string constraints;
	std::string vartypes;
	std::vector<std::string> pointNames(nSites);
	
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
	
	// pre-process strings
	for (std::size_t i=0; i<nSites; ++i)
		pointNames[i]="P"+to_string(i);
		
	//// main processing
	/// single points
	// create objective function and variable types
	lpmodel+="/* objective function */\nmax:";
	vartypes+="\n/* variable types */\nbin";
	for (std::size_t i=0; i<nSites; ++i) {
		lpmodel+=" +"+pointNames[i];
		vartypes+=" "+pointNames[i]+",";
	}
	
	/// point combinations
	constraints+=";\n\n/* constraints */\n";
	for (std::size_t i=0; i<greaterThanDist.outerSize(); ++i) {
		for (Eigen::SparseMatrix<bool>::InnerIterator it(greaterThanDist, i); it; ++it) {
			// save constraints				
			constraints+="-1 "+pointNames[it.row()]+
						 " -1 "+pointNames[it.col()]+
						 ">= -1;\n";
		}
	}
	
	// finish procesing vars
	vartypes.erase(vartypes.size()-1); // remove extra comma
	vartypes+=";";
	
	// compile model string	
	lpmodel+=constraints+vartypes+"\n";
	
	/// exports
	// save lpmodel to file
    std::ofstream out(filepath);
    out << lpmodel;

	// return 1
	return(1);
}

