CXX=clang++ -std=c++20 -O3
CompFLAGS= -g `llvm-config --cppflags`
CFLAGS= -g `llvm-config --cppflags --ldflags --system-libs --libs all` \
-Wno-unused-function -Wno-unknown-warning-option \
-Wpessimizing-move -Wredundant-move -ferror-limit=5
SRCF = src
BUILDF = build
DEPS = $(BUILDF)/ast.o $(BUILDF)/ast_print.o $(BUILDF)/code_gen.o $(BUILDF)/code_gen_helpers.o\
 $(BUILDF)/errors.o $(BUILDF)/helpers.o $(BUILDF)/lexer.o $(BUILDF)/parser.o

mccomp: $(SRCF)/mccomp.cpp $(DEPS)
	$(CXX) $^ $(CFLAGS) -o mccomp

$(BUILDF)/%.o: $(SRCF)/%.cpp $(SRCF)/%.hpp
	@mkdir -p build
	$(CXX) $< $(CompFLAGS) -c -o $@

clean:
	rm -rf build
	rm -f mccomp