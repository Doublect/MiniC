CXX=clang++ -std=c++20
CFLAGS= -g -O3 `llvm-config --cxxflags --ldflags --system-libs --libs all` -std=c++17 \
-Wno-unused-function -Wno-unknown-warning-option 
SRCF = src
BUILDF = build
DEPS = $(BUILDF)/ast.o $(BUILDF)/ast_print.o $(BUILDF)/helpers.o $(BUILDF)/lexer.o $(BUILDF)/parser.o

mccomp: $(SRCF)/mccomp.cpp $(DEPS)
	$(CXX) $^ $(CFLAGS) -o mccomp

$(BUILDF)/%.o: $(SRCF)/%.cpp $(SRCF)/%.hpp $(BUILDF)
	$(CXX) $< -c -o $@ -g -O3

$(BUILDF):
	mkdir -p build

clean:
	rm -rf build
	rm -f mccomp