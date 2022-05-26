CXXFLAGS = -std=c++11 -Wno-out-of-line-declaration
OPT = -O3

tc: hypergraph.cpp main_thesy.cpp
	node ../build/cppnator/the-cppnator.js hypergraph.cpp -o inc/hypergraph.h
	g++ $(OPT) -o tc -g $(CXXFLAGS) $+
