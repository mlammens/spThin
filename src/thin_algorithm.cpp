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
		int DrawDiscreteNumber(std::vector<int>::const_iterator begin, std::vector<int>::const_iterator end) {
			d1.param(std::discrete_distribution<int>::param_type(begin, end));
			return (d1(eng));
		}
		
		int DrawUniformNumber(int ub) {
			d2.param(std::uniform_int_distribution<int>::param_type(0, ub));
			return (d2(eng));
		}
		
	private:
		std::mt19937 eng;
		std::discrete_distribution<int> d1;
		std::uniform_int_distribution<int> d2;
};

class BoolMatrix {
	public:
		BoolMatrix();
		BoolMatrix(std::size_t nrow, std::size_t ncol) {
			_nrow=nrow;
			_ncol=ncol;
			_data.resize(nrow * ncol);
		};
	
		bool any() {
			for (auto i=_data.cbegin(); i!=_data.cend(); ++i) {
				if (*i) {
					return(true);
				}
			}
			return (false);
		}
		
		std::size_t sumcol(std::size_t x) {
			std::size_t val=0;
			for (auto i=_data.cbegin()+(x*_nrow); i<_data.cbegin()+((x+1)*_nrow); ++i)
				val+=*x
			return (val);
		}
		
		void setcol(std::size_t x, bool v) {
			for (auto i=_data.begin()+(x*_nrow); i<_data.begin()+((x+1)*_nrow); ++i)
				*i=v;
		}
		
		void setrow(std::size_t x, bool v) {
			for (auto i=_data.begin()+x; i<_data.begin()+((_ncol-1)* _nrow)+1; i+=_nrow)
				*i=v;
		}
		
		bool BoolMatrix::operator()(const std::size_t i, const std::size_t j) {
			return(_data[i*_ncol + j]);
		}

		void BoolMatrix::operator()(const std::size_t i, const std::size_t j, bool v) {
			_data[i*_ncol + j]=v;
		}

		void BoolMatrix::operator[](const std::size_t i, bool v) {
			_data[i]=v;
		}

		bool BoolMatrix::operator[](const std::size_t i) {
			return(_data[i]);
		}
	
	private:
		std::size_t _nrow;
		std::size_t _ncol;
		std::vector<bool> _data;
};


// [[Rcpp::export]]
Rcpp::List rcpp_thin_algorithm(std::vector<double> lon, std::vector<double> lat, double thin_par, int reps, bool great_circle_distance) {
	/// init
	// declare objects
	int currSite;
	int nSites=lon.size();
	int nRemainingSites;
	
	int currMaxCount;
	int temp;
	std::vector<std::size_t> currSiteCounts(nSites);
	std::vector<std::size_t> idMaxCounts(nSites);
	std::vector<std::size_t> idRemainingSites(nSites)
	BoolMatrix greaterThanDist(nSites,nSites);		
	std::vector<std::vector<int> > sites;
	sites.resize(reps);
	
	int seed=std::chrono::high_resolution_clock::now().time_since_epoch().count();
	Random rgen(seed);
	
	
	BoolMatrix test(4, 3);
	
	
	/// create distance matrix
	// fill upper triangle with distances and use this for computation
	if (great_circle_distance) {
		// use great circle distances
		for (int i=0; i<(nSites-1); ++i) {
			for (int j=(i+1); j<nSites; ++j) {
				greaterThanDist(i,j)=GcDistanceInMeters(lon[i], lat[i], lon[j], lat[j]) > thin_par;
				greaterThanDist(j,i)=greaterThanDist(i,j);
			}
		}
	} else {
		// use euclidean circle distances
		for (int i=0; i<(nSites-1); ++i) {
			for (int j=(i+1); j<nSites; ++j) {
				greaterThanDist(i,j)=EucDistanceInMeters(lon[i], lat[i], lon[j], lat[j]) > thin_par;
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
		idRemainingSites.resize(nSites);
		std::iota(idRemainingSites.begin(), idRemainingSites.end(), 0);
		while (greaterThanDist.any()) & (nRemainingSites > 1)) {
			// find counts of sites within nearest distances
			for (int i=0; i<nRemainingSites; ++i)
				currSiteCounts[i]=greaterThanDist.sumcol(idRemainingSites[i]);
								
			// remove site
			greaterThanDist.setcol(idRemainingSites[i], false);
			greaterThanDist.setrow(idRemainingSites[i], false);
			currSite=rgen.DrawDiscreteNumber(currSiteCounts.cbegin(), currSiteCounts.cbegin()+nRemainingSites);
			idRemainingSites.erase(idRemainingSites.begin()+currSite);
			--nRemainingSites;
		}
		
		// store results
		sites[r].reserve(nRemainingSites);
		for (auto i=idRemainingSites.cbegin(); i!=idRemainingSites.cend(); ++i)
			sites[r].push_back((*i) + 1);
	}
	
	/// exports
	return(Rcpp::wrap(sites));
}

