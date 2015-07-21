#ifndef FUNCTIONS_H
#define FUNCTIONS_H

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

#define NCHECKUSERINPUT 1000

// #define DEG_TO_RAD 0.017453292519943295769236907684886
// #define EARTH_RADIUS_IN_METERS 6372797.5608566

// The usual PI/180 constant
static const double DEG_TO_RAD = 0.017453292519943295769236907684886;

// Earth's quadratic mean radius for WGS-84
static const double EARTH_RADIUS_IN_METERS = 6372797.560856;

double ArcInRadians(double, double, double, double);

double GcDistanceInMeters(double, double, double, double);

double EucDistanceInMeters(double, double, double, double);

// convert objects to string
template <typename T>
std::string to_string(T value) {
	std::ostringstream os;
	os << value;
	return os.str();
}



#endif