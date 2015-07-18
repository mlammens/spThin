#ifndef RANDOM_H
#define RANDOM_H

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

class Random {
	public:
		Random(int seed) {
			std::mt19937 tmp(seed); 
			eng=tmp;
		};

		inline std::size_t DrawDiscreteNumber(std::vector<std::size_t>::const_iterator begin, std::vector<std::size_t>::const_iterator end) {
			d1.param(std::discrete_distribution<std::size_t>::param_type(begin, end));
			return (d1(eng));
		}

		inline std::size_t DrawUniformNumber(std::size_t ub) {
			d2.param(std::uniform_int_distribution<std::size_t>::param_type(0, ub));
			return (d2(eng));
		}
		
	private:
		std::mt19937 eng;
		std::discrete_distribution<std::size_t> d1;
		std::uniform_int_distribution<std::size_t> d2;
};

#endif