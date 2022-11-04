CXX=clang++ -std=c++20
CFLAGS= -g -O3 `llvm-config --cppflags --ldflags --system-libs --libs all` \
-Wno-unused-function -Wno-unknown-warning-option -fno-exceptions -fno-rtti \
-Wpessimizing-move -Wredundant-move -ferror-limit=5
SRCF = src
BUILDF = build
DEPS = $(BUILDF)/ast.o $(BUILDF)/ast_print.o $(BUILDF)/code_gen.o $(BUILDF)/helpers.o $(BUILDF)/lexer.o $(BUILDF)/parser.o

mccomp: $(SRCF)/mccomp.cpp $(DEPS)
	$(CXX) $^ $(CFLAGS) -o mccomp

$(BUILDF)/%.o: $(SRCF)/%.cpp $(SRCF)/%.hpp
	@mkdir -p build
	$(CXX) $< -c -o $@ -g -O3

clean:
	rm -rf build
	rm -f mccomp