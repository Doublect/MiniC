#ifndef PARSER_H
#define PARSER_H
  #include<functional>
  #include<memory>
  #include<string>
  #include<vector>

  #include "ast.hpp"
  #include "lexer.hpp"
  #include "helpers.hpp"

  TOKEN getNextToken();

  //template<typename T> using ParserFunction = std::function<ResultMonad<T>()>;
  template<typename T, class... Vars> using ParserFunction = std::function<ResultMonad<T>(Vars...)>;

  //ResultMonad<std::vector<std::unique_ptr<DeclASTNode>>, B> decl_list();
  //static ResultMonad<std::vector<std::unique_ptr<ExternFunctionDeclASTNode>>> extern_list();
  ResultMonad<std::vector<std::unique_ptr<DeclASTNode>>> decl_list();
  static ResultMonad<std::vector<std::unique_ptr<ExternFunctionDeclASTNode>>> extern_list();

  //ResultMonad<ProgramASTNode> parser();
  ResultMonad<ProgramASTNode> parser();
#endif