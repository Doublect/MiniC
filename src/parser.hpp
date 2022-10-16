#ifndef PARSER_H
#define PARSER_H
  #include<functional>
  #include<memory>
  #include<vector>

  #include "ast.hpp"
  #include "lexer.hpp"


  TOKEN getNextToken();

  template<typename T> using ParserFunction = std::function<T()>;

  std::vector<std::unique_ptr<DeclASTNode>> decl_list();
  static std::vector<std::unique_ptr<ExternFunctionDeclASTNode>> extern_list();

  ProgramASTNode parser();
#endif