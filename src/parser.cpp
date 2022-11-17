#include<deque>
#include<memory>
#include<optional>

#include "ast.hpp"
#include "errors.hpp"
#include "helpers.hpp"
#include "lexer.hpp"
#include "parser.hpp"

FILE *pFile;

extern int lineNo, columnNo;

extern std::string IdentifierStr;
extern int IntVal;
extern bool BoolVal;
extern float FloatVal;
extern std::string StringVal;

using namespace std::string_literals;

#define ConsumeAssign(type, variable, result) \
  { ResultMonad<type> res = result(); \
  if(res.success()) { variable = std::move(res).unwrap(); } else { return res; }}
#define Consume(type, variable, result) \
  std::unique_ptr<type> variable; \
  ConsumeAssign(type, variable, result);

#define ConsumeAssignVal(type, variable, result) \
  { ResultMonad<type> res = result(); \
  if(res.success()) { variable = *std::move(res).unwrap_val(); } else { return res; }}
#define ConsumeVal(type, variable, result) \
  type variable; \
  ConsumeAssignVal(type, variable, result);

#define ConsumeAssignNoCall(type, variable, result) \
  { ResultMonad<type> res = result; \
  if(res.success()) { variable = std::move(res).unwrap(); } else { return res; }}
#define ConsumeNoCall(type, variable, result) \
  std::unique_ptr<type> variable; \
  ConsumeAssignNoCall(type, variable, result);

#define LogError(string) \
  return ResultMonad<void>(ErrorT(string));

#define Expect(token_type) \
  { \
    ResultMonad<TOKEN> res = expect(token_type); \
    if(!res.success()) { return res; } \
  }

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static TOKEN CurTok;
static std::deque<TOKEN> tok_buffer;

TOKEN getNextToken() {
  if (tok_buffer.size() == 0)
    tok_buffer.push_back(gettok(pFile));

  TOKEN temp = tok_buffer.front();
  tok_buffer.pop_front();

  return CurTok = temp;
}

static void putBackToken(TOKEN tok) { tok_buffer.push_front(tok); }

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//

static bool isExprFirst() {
  return  CurTok.type == TOKEN_TYPE::IDENT
    || CurTok.type == TOKEN_TYPE::INT_LIT
    || CurTok.type == TOKEN_TYPE::FLOAT_LIT
    || CurTok.type == TOKEN_TYPE::BOOL_LIT
    || CurTok.type == TOKEN_TYPE::SC
    || CurTok.type == TOKEN_TYPE::LPAR
    || CurTok.type == TOKEN_TYPE::MINUS
    || CurTok.type == TOKEN_TYPE::NOT;
}

static bool isVarTypeFirst() {
  return CurTok.type == TOKEN_TYPE::INT_TOK || CurTok.type == TOKEN_TYPE::FLOAT_TOK || CurTok.type == TOKEN_TYPE::BOOL_TOK;
}

static bool isTypeSpecFirst() {
  return 
    CurTok.type == TOKEN_TYPE::VOID_TOK 
    || isVarTypeFirst();
}

static bool isStmtFirst() {
  return 
    isVarTypeFirst() // int, float, bool tokens
    || isExprFirst() // literals, identiier, unary ops, (, ;
    || CurTok.type == TOKEN_TYPE::LBRA 
    || CurTok.type == TOKEN_TYPE::IF
    || CurTok.type == TOKEN_TYPE::WHILE
    || CurTok.type == TOKEN_TYPE::RETURN;
}

///----------------------------------------------------------------------------
/// Parser Errors
///----------------------------------------------------------------------------
/// LogError* - These are little helper functions for error handling.
// static std::unique_ptr<ASTNode> LogError(std::string Str) {
//   fprintf(stderr, "LogError: %s\n", Str.c_str());
//   return nullptr;
// }

ResultMonad<TypeSpecType> type_spec() {
    if(isTypeSpecFirst()) {
      TypeSpecType tst = (TypeSpecType) CurTok.type;
      getNextToken();
      return make_result(std::move(tst));
    }

    return make_result<VariableType>(unique_ptr_cast<ErrorT>(CustomExpectedTokenErrorT("type specifier", CurTok)));
  };

ResultMonad<VariableType> var_type() {
    if(isVarTypeFirst()) {
      VariableType type = (VariableType) CurTok.type;
      getNextToken();
      return make_result(std::move(type));
    }

    return make_result<VariableType>(unique_ptr_cast<ErrorT>(CustomExpectedTokenErrorT("variable type", CurTok)));
  };

static ResultMonad<std::string> ident() {
    if(CurTok.type == TOKEN_TYPE::IDENT) {
      std::string name = CurTok.lexeme;
      getNextToken();
      return make_result(std::move(name));
    }
    std::string name = CurTok.lexeme;
    getNextToken();

    return make_result<std::string>(unique_ptr_cast<ErrorT>(CustomExpectedTokenErrorT("identifier", CurTok)));
  };

static auto token_type() {
  TOKEN_TYPE tp = (TOKEN_TYPE) CurTok.type;
  getNextToken();
  return make_result(std::move(tp));
};

static ResultMonad<TOKEN> expect(TOKEN_TYPE type) {
  if(CurTok.type != type)
    return make_result<TOKEN>(unique_ptr_cast<ErrorT>(ExpectedTokenErrorT(type, CurTok)));

  TOKEN tok = CurTok;
  getNextToken();
  return make_result(std::move(tok));
}

///-----------------------------------------------------------------------------
/// Expression Parsing
///-----------------------------------------------------------------------------
#pragma region Expression Parsing

static ResultMonad<ExprASTNode> literals() {
    if(CurTok.type == TOKEN_TYPE::INT_LIT) {
      int value = IntVal;
      getNextToken();

      return make_result(IntASTNode(CurTok, value));
    } else if(CurTok.type == TOKEN_TYPE::FLOAT_LIT) {
      float value = FloatVal;
      getNextToken();

      return make_result(FloatASTNode(CurTok, value));
    } else if(CurTok.type == TOKEN_TYPE::BOOL_LIT) {
      bool value = BoolVal;
      getNextToken();

      return make_result(BoolASTNode(CurTok, value));
    }

    return make_result<ExprASTNode>(unique_ptr_cast<ErrorT>(CustomExpectedTokenErrorT("literal", CurTok))); 
  };

static ResultMonad<ExprASTNode> expr();

static ResultMonad<std::vector<std::unique_ptr<ExprASTNode>>> args() {
  Expect(TOKEN_TYPE::LPAR);
  std::vector<std::unique_ptr<ExprASTNode>> args;

  if(CurTok.type != TOKEN_TYPE::RPAR) {
    Consume(ExprASTNode, arg, expr);
    args.push_back(std::move(arg));
  }

  while(CurTok.type == TOKEN_TYPE::COMMA) {
    Expect(TOKEN_TYPE::COMMA);

    Consume(ExprASTNode, arg, expr);
    args.push_back(std::move(arg));
  }

  Expect(TOKEN_TYPE::RPAR);

  return make_result(std::move(args));
};

static ResultMonad<ExprASTNode> primary_expr() {
  if(CurTok.type == TOKEN_TYPE::IDENT) {
    ConsumeVal(std::string, varName, ident);

    if(CurTok.type == TOKEN_TYPE::LPAR) {
      Consume(std::vector<std::unique_ptr<ExprASTNode>>, nodes, args);
      return make_result(CallExprAST(CurTok, std::move(varName), std::move(*nodes.release())));
    }

    return make_result(VariableRefASTNode(CurTok, std::move(varName)));
  } else {
    return literals();
  }
};


static ResultMonad<ExprASTNode> parentheses_expr() {
  if(CurTok.type == TOKEN_TYPE::LPAR) {
    Expect(TOKEN_TYPE::LPAR);
    Consume(ExprASTNode, primary, expr);
    Expect(TOKEN_TYPE::RPAR);
    return make_result_ptr(std::move(primary));
  }

  return primary_expr();
};

ResultMonad<ExprASTNode> base_binary(ParserFunction<ExprASTNode> next, ParserFunction<ExprASTNode, std::unique_ptr<ExprASTNode>> prime) {
  Consume(ExprASTNode, lhs, next);
  return prime(std::move(lhs));
};

ResultMonad<ExprASTNode> prime_binary(ParserFunction<ExprASTNode> next, std::vector<TOKEN_TYPE> ops, std::unique_ptr<ExprASTNode> p) {
  std::unique_ptr<ExprASTNode> lhs = std::move(p);

  if(std::find(ops.begin(), ops.end(), CurTok.type) != ops.end()) {
    TOKEN startTok = CurTok;
    ConsumeVal(TOKEN_TYPE, op, token_type);
    // Evaluate lower expression
    Consume(ExprASTNode, rhs, next);

    lhs = unique_ptr_cast<ExprASTNode>(BinaryASTNode(op, std::move(lhs), std::move(rhs), startTok));    

    // Recursion
    return prime_binary(next, ops, std::move(lhs));
  }

  // Epsilon
  return make_result_ptr(std::move(lhs));
};


static ParserFunction<ExprASTNode> unary_expr = 
  []() -> ResultMonad<ExprASTNode> {
    if(CurTok.type == TOKEN_TYPE::MINUS || CurTok.type == TOKEN_TYPE::NOT) {
      TOKEN startTok = CurTok;
      ConsumeVal(TOKEN_TYPE, op, token_type);
      Consume(ExprASTNode, unary, unary_expr);
      
      return make_result(UnaryASTNode(op, std::move(unary), startTok));
    }

    return parentheses_expr();
  };

static auto mul_expr_prime(std::unique_ptr<ExprASTNode> p) {
  return prime_binary(unary_expr, {TOKEN_TYPE::ASTERIX, TOKEN_TYPE::DIV, TOKEN_TYPE::MOD}, std::move(p));
};

static auto mul_expr() {
  return base_binary(unary_expr, mul_expr_prime);
}

static auto add_expr_prime(std::unique_ptr<ExprASTNode> p) {
  return prime_binary(mul_expr, {TOKEN_TYPE::PLUS, TOKEN_TYPE::MINUS}, std::move(p));
};

static auto add_expr() {
  return base_binary(mul_expr, add_expr_prime);
} 

static auto rel_expr_prime(std::unique_ptr<ExprASTNode> p) {
  return prime_binary(add_expr, {TOKEN_TYPE::LT, TOKEN_TYPE::LE, TOKEN_TYPE::GT, TOKEN_TYPE::GE}, std::move(p));
};

static auto rel_expr() {
  return base_binary(add_expr, rel_expr_prime);
} 

static auto eq_expr_prime(std::unique_ptr<ExprASTNode> p) {
  return prime_binary(rel_expr, {TOKEN_TYPE::EQ, TOKEN_TYPE::NE}, std::move(p));
}

static auto eq_expr() {
  return base_binary(rel_expr, eq_expr_prime);
}

static ResultMonad<ExprASTNode> and_expr_prime(std::unique_ptr<ExprASTNode> p) {
  std::unique_ptr<ExprASTNode> lhs = std::move(p);

  if(CurTok.type == TOKEN_TYPE::AND) {
    TOKEN startTok = CurTok;
    ConsumeVal(TOKEN_TYPE, op, token_type);
    // Evaluate lower expression
    Consume(ExprASTNode, rhs, eq_expr);

    lhs = unique_ptr_cast<ExprASTNode>(LazyAndASTNode(std::move(lhs), std::move(rhs), startTok));    

    // Recursion
    return and_expr_prime(std::move(lhs));
  }

  // Epsilon
  return make_result_ptr(std::move(lhs));
}

static auto and_expr() {
  return base_binary(eq_expr, and_expr_prime);
}

static ResultMonad<ExprASTNode> or_prime(std::unique_ptr<ExprASTNode> p) {
  std::unique_ptr<ExprASTNode> lhs = std::move(p);

  if(CurTok.type == TOKEN_TYPE::OR) {
    TOKEN startTok = CurTok;
    ConsumeVal(TOKEN_TYPE, op, token_type);
    // Evaluate lower expression
    Consume(ExprASTNode, rhs, and_expr);

    lhs = unique_ptr_cast<ExprASTNode>(LazyOrASTNode(std::move(lhs), std::move(rhs), startTok));    

    // Recursion
    return or_prime(std::move(lhs));
  }

  // Epsilon
  return make_result_ptr(std::move(lhs));
}

static auto or_expr() {
  return base_binary(and_expr, or_prime);
}

static ResultMonad<ExprASTNode> expr() {
  if(CurTok.type == TOKEN_TYPE::IDENT) {
    TOKEN identTok = CurTok;
    ConsumeVal(std::string, varName, ident);

    if(CurTok.type == TOKEN_TYPE::ASSIGN) {
      // Skip `=` token
      getNextToken();
      Consume(ExprASTNode, node, expr);
      return make_result(AssignmentASTNode(CurTok, std::move(varName), std::move(node)));
    } else {
      putBackToken(CurTok);
      CurTok = identTok;
    }
  }
  return or_expr();
}

#pragma endregion

///-----------------------------------------------------------------------------
/// Statement Parsing
///-----------------------------------------------------------------------------
#pragma region Statement Parsing

static ResultMonad<StatementASTNode> stmt();

static ParserFunction<std::vector<std::unique_ptr<StatementASTNode>>> stmt_list =
  []() -> ResultMonad<std::vector<std::unique_ptr<StatementASTNode>>> {
    std::vector<std::unique_ptr<StatementASTNode>> statements;
    while(isStmtFirst()) {
      Consume(StatementASTNode, node, stmt);
      statements.push_back(std::move(node));
    }
    return make_result(std::move(statements));
  };

static auto local_decl =
  []() -> ResultMonad<VariableDeclASTNode> {
    ConsumeVal(VariableType, type, var_type);
    ConsumeVal(std::string, value, ident);
    Expect(TOKEN_TYPE::SC);

    return make_result(VariableDeclASTNode(CurTok, value, type));
  };

static ParserFunction<std::vector<std::unique_ptr<DeclASTNode>>> local_decl_list =
  []() -> ResultMonad<std::vector<std::unique_ptr<DeclASTNode>>> {
    std::vector<std::unique_ptr<DeclASTNode>> decls;

    while(isVarTypeFirst()) {
      Consume(VariableDeclASTNode, decl, local_decl);
      decls.push_back(std::unique_ptr<DeclASTNode>(std::move(decl)));
    }

    return make_result(std::move(decls));
  };

static ParserFunction<BlockASTNode> block = 
  []() -> ResultMonad<BlockASTNode> {
    TOKEN startTok = CurTok;
    Expect(TOKEN_TYPE::LBRA);

    Consume(std::vector<std::unique_ptr<DeclASTNode>>, decls, local_decl_list);
  
    Consume(std::vector<std::unique_ptr<StatementASTNode>>, statements, stmt_list);

    Expect(TOKEN_TYPE::RBRA);

    return make_result(BlockASTNode(std::move(*decls.release()), std::move(*statements.release()), startTok));
  };

static ParserFunction<IfElseASTNode> if_stmt =
  []() -> ResultMonad<IfElseASTNode> {
    TOKEN startTok = CurTok;
    std::unique_ptr<BlockASTNode> else_branch;

    Expect(TOKEN_TYPE::IF);
    Expect(TOKEN_TYPE::LPAR);
    Consume(ExprASTNode, condition, expr);
    Expect(TOKEN_TYPE::RPAR);
    Consume(BlockASTNode, then_branch, block);
    if(CurTok.type == TOKEN_TYPE::ELSE) {
      getNextToken();
      ConsumeAssign(BlockASTNode, else_branch, block);
    }

    return make_result(IfElseASTNode(std::move(condition), std::move(then_branch), std::move(else_branch), startTok));
  };

static ParserFunction<WhileASTNode> while_stmt =
  []() -> ResultMonad<WhileASTNode> {
    TOKEN startTok = CurTok;
    Expect(TOKEN_TYPE::WHILE);
    Expect(TOKEN_TYPE::LPAR);
    Consume(ExprASTNode, condition, expr);
    Expect(TOKEN_TYPE::RPAR);
    Consume(StatementASTNode, body, stmt);

    return make_result(WhileASTNode(std::move(condition), std::move(body), startTok));
  };

static ResultMonad<ReturnStmtASTNode> return_stmt() {
    std::unique_ptr<ExprASTNode> exp;
    TOKEN startTok = CurTok;

    Expect(TOKEN_TYPE::RETURN);
    if(CurTok.type != TOKEN_TYPE::SC) {
      ConsumeAssign(ExprASTNode, exp, expr);
    }
    Expect(TOKEN_TYPE::SC);

    return make_result(ReturnStmtASTNode(std::move(exp), startTok));
  };

static ResultMonad<StatementASTNode> stmt() {
  if(CurTok.type == TOKEN_TYPE::IF) {
    return if_stmt();
  } else if(CurTok.type == TOKEN_TYPE::WHILE) {
    return while_stmt();
  } else if(CurTok.type == TOKEN_TYPE::RETURN) {
    return return_stmt();
  } else if(CurTok.type == TOKEN_TYPE::LBRA) {
    return block();
  } else if(isExprFirst()) {
    Consume(ExprASTNode, stmt, expr);
    Expect(TOKEN_TYPE::SC);
    return make_result_ptr(std::move(stmt));
  }

  return make_result<StatementASTNode>(ErrorT("One of: 'if', 'while', 'return', 'block' or an expression was expected", CurTok));
}
#pragma endregion


static ResultMonad<FunctionParameterASTNode> param_decl() { 
  ConsumeVal(VariableType, type, var_type);
  ConsumeVal(std::string, value, ident);

  return make_result(FunctionParameterASTNode(CurTok, value, type));
};

static ResultMonad<std::vector<std::unique_ptr<FunctionParameterASTNode>>> func_params() {
  std::vector<std::unique_ptr<FunctionParameterASTNode>> params;

  if(CurTok.type == TOKEN_TYPE::VOID_TOK) {
    getNextToken();
    return make_result(std::move(params));
  }

  if(isVarTypeFirst()) {
    Consume(FunctionParameterASTNode, param, param_decl);
    params.push_back(std::move(param));

    while(CurTok.type == TOKEN_TYPE::COMMA) {
      getNextToken();
      Consume(FunctionParameterASTNode, param, param_decl);
      params.push_back(std::move(param));
    }
  }

  return make_result(std::move(params));
};

static ResultMonad<ExternFunctionDeclASTNode> extern_decl() {
  TOKEN startTok = CurTok;
  Expect(TOKEN_TYPE::EXTERN);
  ConsumeVal(TypeSpecType, type, type_spec);
  ConsumeVal(std::string, name, ident);
  Expect(TOKEN_TYPE::LPAR);
  Consume(std::vector<std::unique_ptr<FunctionParameterASTNode>>, params, func_params);
  Expect(TOKEN_TYPE::RPAR);
  Expect(TOKEN_TYPE::SC); 

  return make_result(ExternFunctionDeclASTNode(name, std::move(*params.release()), type, startTok));
};


static ResultMonad<std::vector<std::unique_ptr<ExternFunctionDeclASTNode>>> extern_list() { 
  std::vector<std::unique_ptr<ExternFunctionDeclASTNode>> func_decls;

  while(CurTok.type == TOKEN_TYPE::EXTERN) {
    Consume(ExternFunctionDeclASTNode, decl, extern_decl);
    func_decls.push_back(std::move(decl));
  }

  return make_result(std::move(func_decls));
}

static ResultMonad<DeclASTNode> decl() {
  std::string name;
  TypeSpecType type;
  TOKEN startTok = CurTok;

  if(CurTok.type == TOKEN_TYPE::VOID_TOK) {
    type = TypeSpecType::VOID;
    getNextToken();
    ConsumeAssignVal(std::string, name, ident);
  } else {
    { 
      ResultMonad<VariableType> res = var_type(); \
      if(res.success()) { type = (TypeSpecType)*std::move(res).unwrap_val(); } else { return res; }
    }

    ConsumeAssignVal(std::string, name, ident);

    if(CurTok.type == TOKEN_TYPE::SC) {
      Expect(TOKEN_TYPE::SC);

      auto a = make_result(VariableDeclASTNode(CurTok, name, (VariableType) type));
      return a;
    }
  }
  Expect(TOKEN_TYPE::LPAR);
  Consume(std::vector<std::unique_ptr<FunctionParameterASTNode>>, params, func_params);
  Expect(TOKEN_TYPE::RPAR);

  Consume(BlockASTNode, node, block);

  // Let the function decl node create the scope for the function
  node->setHasScope(false);
  
  return make_result(FunctionDeclASTNode(name, std::move(*params.release()), std::move(node), type, startTok));
};

ResultMonad<std::vector<std::unique_ptr<DeclASTNode>>> decl_list() {
  std::vector<std::unique_ptr<DeclASTNode>> func_decls;
  
  while(isTypeSpecFirst()) {
    Consume(DeclASTNode, node, decl);

    func_decls.push_back(std::move(node));
  }

  return make_result(std::move(func_decls));
}

// program ::= extern_list decl_list
ResultMonad<ProgramASTNode> parser() {
  // Make sure we are at the beginning of the program
  fseek(pFile, 0, SEEK_SET);

  getNextToken();

  Consume(std::vector<std::unique_ptr<ExternFunctionDeclASTNode>>, extern_func_decls, extern_list);
  Consume(std::vector<std::unique_ptr<DeclASTNode>>, decls, decl_list);

  return make_result(ProgramASTNode(std::move(*extern_func_decls.release()), std::move(*decls.release())));
}
