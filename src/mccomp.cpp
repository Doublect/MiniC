
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/FileSystem.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <cstring>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

#include "ast.hpp"
#include "lexer.hpp"
#include "parser.hpp"

using namespace llvm;

extern FILE *pFile;

extern std::unique_ptr<LLVMContext> TheContext;
extern std::unique_ptr<IRBuilder<>> Builder;
extern std::unique_ptr<Module> TheModule;

extern int lineNo, columnNo;

//===----------------------------------------------------------------------===//
// AST Printer
//===----------------------------------------------------------------------===//

// inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
//                                      const ASTNode &ast) {
//   os << ast.to_string();
//   return os;
// }

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv) {
  if (argc == 2) {
    pFile = fopen(argv[1], "r");
    if (pFile == nullptr) {
      perror("Error opening file");
    }
  } else {
    std::cout << "Usage: ./code InputFile\n";
    return 1;
  }

  // initialize line number and column numbers to zero
  lineNo = 1;
  columnNo = 1;

  //get the first token
  // getNextToken();
  // while (CurTok.type != EOF_TOK) {
  //   fprintf(stderr, "Token: %s with type %d\n", CurTok.lexeme.c_str(),
  //           CurTok.type);
  //   getNextToken();
  // }
  // fprintf(stderr, "Lexer Finished\n");

  // Make the module, which holds all the code.

  std::cout << "Parsing...\n";
  // Run the parser now.
  auto res = parser();
  if(!res.success()) {
    std::cout << "Parser Error" << std::endl;
    std::cout << res.error().msg() << std::endl;

    return 1;
  } else {
    std::cout << res.success() << std::endl;
    std::cout << "Successful Parsing" << std::endl;
    auto AST = std::move(res).unwrap();
    
    AST->to_ast_print()->printAST("", true);

    AST->codegen();
  }

  //********************* Start printing final IR **************************
  // Print out all of the generated code into a file called output.ll
  auto Filename = "output.ll";
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }
  TheModule->print(errs(), nullptr); // print IR to terminal
  TheModule->print(dest, nullptr);
  //********************* End printing final IR ****************************

  fclose(pFile); // close the file that contains the code that was parsed
  return 0;
}
