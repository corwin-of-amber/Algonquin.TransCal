CXXFLAGS = -std=c++11 -Wno-out-of-line-declaration
OPT = -O3

tc_SRC = hypergraph main_prove
tc_explore_SRC = hypergraph main_thesy

tc: ${foreach m, $(tc_SRC), obj/$m.o}
	g++ -o $@ $+

inc/hypergraph.h: hypergraph.cpp
	node ../build/cppnator/the-cppnator.js $< -o $@

obj/%.o: %.cpp
	@mkdir -p ${@D}
	g++ -c $(OPT) -o $@ -g $(CXXFLAGS) $<

obj/main_prove.o: inc/hypergraph.h