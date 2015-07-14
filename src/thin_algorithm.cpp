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
#include <Rcpp.h>

// The usual PI/180 constant
static const double DEG_TO_RAD = 0.017453292519943295769236907684886;

// Earth's quadratic mean radius for WGS-84
static const double EARTH_RADIUS_IN_METERS = 6372797.560856;

/** Computes the arc, in radian, between two WGS-84 positions.
  *
  * The result is equal to <code>Distance(from,to)/EARTH_RADIUS_IN_METERS
  *  = 2*asin(sqrt(h(d/EARTH_RADIUS_IN_METERS )))
  *
  * where:
  *    - d is the distance in meters between 'from' and 'to' positions.
  *    - h is the haversine function: <code>h(x)=sin²(x/2)
  *
  *
  * The haversine formula gives:
  *    h(d/R) = h(from.lat-to.lat)+h(from.lon-to.lon)+cos(from.lat)*cos(to.lat)
  *
  * http://en.wikipedia.org/wiki/Law_of_haversines
  */
double ArcInRadians(double lon1, double lat1, double lon2, double lat2) {
    double latitudeArc  = (lat1 - lat2) * DEG_TO_RAD;
    double longitudeArc = (lon1 - lon2) * DEG_TO_RAD;
    double latitudeH = sin(latitudeArc * 0.5);
    latitudeH *= latitudeH;
    double longitudeH = sin(longitudeArc * 0.5);
    longitudeH *= longitudeH;
    double tmp = cos(lat1*DEG_TO_RAD) * cos(lat1*DEG_TO_RAD);
    return 2.0 * asin(sqrt(latitudeH + tmp*longitudeH));
}

// great circle distance
double GcDistanceInMeters(double lon1, double lat1, double lon2, double lat2) {
    return EARTH_RADIUS_IN_METERS*ArcInRadians(lon1, lat1, lon2, lat2);
}

// euclidean distance
double EucDistanceInMeters(double x1, double y1, double x2, double y2) {
	double d1=(x1-x2);
	double d2=(y1-y2);
	return std::sqrt((d1*d1) + (d2*d2));
}

// random integer generator class
class Random {
	public:
		Random(int seed) {
			std::mt19937 tmp(seed); 
			eng=tmp;
		};
		int DrawNumber(std::vector<int> &weights) {
			std::discrete_distribution<int> discrete_dist(weights.cbegin(), weights.cend());
			return discrete_dist(eng);
		}
		
	private:
		std::mt19937 eng;
};



// [[Rcpp::export]]
Rcpp::List rcpp_thin_algorithm(std::vector<double> lon, std::vector<double> lat, double thin_par, int reps, bool great_circle_distance) {
	/// init
	// declare objects
	int currSite;
	int nSites=lon.size();
	int nRemainingSites;
	int temp;
	double currMax;
	double Inf=std::numeric_limits<double>::infinity();
	std::vector<int> currSiteCounts(nSites);
	std::vector<int> idMaxCounts(nSites);
	std::vector<int> idRemainingSites(nSites);
	std::vector<std::vector<int> > sites;
	sites.resize(reps);
	Rcpp::NumericMatrix dist(nSites, nSites);
	Rcpp::NumericMatrix currDist(nSites, nSites);
	
	int seed=std::chrono::high_resolution_clock::now().time_since_epoch().count();
	Random rgen(seed);
	
	/// create distance matrix
	// fill upper triangle with distances and use this for computation
	if (great_circle_distance) {
		// use great circle distances
		for (int i=0; i<(nSites-1); ++i) {
			for (int j=(i+1); j<nSites; ++j) {
				dist(i,j)=GcDistanceInMeters(lon[i], lat[i], lon[j], lat[j]);
				dist(j,i)=Inf;
			}
		}
	} else {
		// use euclidean circle distances
		for (int i=0; i<(nSites-1); ++i) {
			for (int j=(i+1); j<nSites; ++j) {
				dist(i,j)=EucDistanceInMeters(lon[i], lat[i], lon[j], lat[j]);
				dist(j,i)=Inf;
			}
		}
	}
	for (int i=0; i<nSites; ++i)
		dist(i,i)=Inf;
	
	/// main processing
	for (int r=0; r<reps; ++r) {
		// reset parameters for new rep
		nRemainingSites=nSites;
		currDist=Rcpp::clone(dist);
		std::iota(idRemainingSites.begin(), idRemainingSites.end(), 0);
				
		while (min(currDist) < thin_par & nRemainingSites > 1) {
			// find counts of sites within nearest distances
			std::fill(currSiteCounts.begin(), currSiteCounts.end(), 0);
			for (int i=0; i<(nSites-1); ++i) {
				for (int j=(i+1); j<nSites; ++j) {
					if (currDist(i,j) < thin_par) {
						++currSiteCounts[i];
					}
				}
			}
		
			// randomly sample a site weighted by frequency of nearest sites
			currSite=rgen.DrawNumber(currSiteCounts);
						
			// remove site 
			--nRemainingSites;
			for (int i=0; i<nSites; ++i) {
				currDist(currSite,i)=Inf;
				currDist(i,currSite)=Inf;
			}
			idRemainingSites[currSite]=-1;
		}
		
		// store results
		sites[r].reserve(nRemainingSites);
		for (int i=0; i<nSites; ++i) {
			if (idRemainingSites[i]>-1) {
				sites[r].push_back(idRemainingSites[i]+1);
			}
		}
		sites[r].shrink_to_fit();
	}
	
	/// exports
	return(Rcpp::wrap(sites));
}

