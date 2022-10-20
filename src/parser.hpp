#ifndef PARSER_H
#define PARSER_H
  #include<functional>
  #include<memory>
  #include<vector>

  #include "ast.hpp"
  #include "lexer.hpp"
  #include "helpers.hpp"

  TOKEN getNextToken();

  template<typename T, bool B> using ParserFunction = std::function<ResultMonad<T, B>()>;

  //ResultMonad<std::vector<std::unique_ptr<DeclASTNode>>, B> decl_list();
  //static ResultMonad<std::vector<std::unique_ptr<ExternFunctionDeclASTNode>>> extern_list();
  auto decl_list();
  auto extern_list();

  //ResultMonad<ProgramASTNode> parser();
  auto parser();
#endif