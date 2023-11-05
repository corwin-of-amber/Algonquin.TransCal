CXXFLAGS = -std=c++17 -Wno-out-of-line-declaration -I.
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

clean:
	rm -rf obj/*.o tc

.PHONY: clean

obj/main_prove.o: inc/hypergraph.h