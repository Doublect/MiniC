CXX=clang++ -std=c++20
CFLAGS= -g -O3 `llvm-config --cxxflags --ldflags --system-libs --libs all` \
#-Wno-unused-function -Wno-unknown-warning-option 

mccomp: mccomp.cpp
	$(CXX) mccomp.cpp $(CFLAGS) -o mccomp

clean:
	rm -rf mccomp 
