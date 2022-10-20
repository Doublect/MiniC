#include<deque>
#include <memory>

#include "parser.hpp"

FILE *pFile;

extern int lineNo, columnNo;


//#define Consume(result) result.success() ? result.unwrap() : { return result; } 
//#define Consume(variable, result) auto res = result(); if(res.success()) { auto variable = res.unwrap(); } else { return res; }
#define Consume(type, variable, result) \ 
  ResultMonad variable; \
  { auto res = result(); \
  if(res.success()) { variable = res.unwrap(); } else { return res; }}
#define ConsumeAssign(type, variable, result) \ 
  { ResultMonad<type> res = result(); \
  if(res.success()) { variable = res.unwrap(); } else { return res; }}

#define ConsumeVal(type, variable, result) \ 
  type variable; \
  { ResultMonad<type> res = result(); \
  if(res.success()) { variable = *res.unwrap().release(); } else { return res; }}
#define ConsumeAssignVal(type, variable, result) \ 
  { ResultMonad<type> res = result(); \
  if(res.success()) { variable = *res.unwrap_val(); } else { return res; }}

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

static bool isExternListFirst() {
  return CurTok.type == TOKEN_TYPE::EXTERN;
}

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

static bool isIdent() {
  return CurTok.type == TOKEN_TYPE::IDENT;
}

inline static auto or_expr();

///----------------------------------------------------------------------------
/// Parser Errors
///----------------------------------------------------------------------------
/// LogError* - These are little helper functions for error handling.
static std::unique_ptr<ASTNode> LogError(std::string Str) {
  fprintf(stderr, "LogError: %s\n", Str.c_str());
  return nullptr;
}

//using ParserFunction = std::function<ASTNode()>;

auto type_spec = 
  []() {
    if(isTypeSpecFirst()) {
      TypeSpecType tst = (TypeSpecType) CurTok.type;
      getNextToken();
      return make_result(std::move(tst));
    }

    //TODO: error
    LogError("Expected type specifier");
    getNextToken();
    return make_result(TypeSpecType::VOID);
  };

auto var_type = 
  [](){
    if(isVarTypeFirst()) {
      VariableType type = (VariableType) CurTok.type;
      getNextToken();
      return make_result(std::move(type));
    }

    getNextToken();
    //TODO: Error
    return make_result(VariableType::INT);
  };

auto ident =
  [](){
    if(CurTok.type == TOKEN_TYPE::IDENT) {
      std::string name = CurTok.lexeme;
      getNextToken();
      return make_result(std::move(name));
    }
    std::string name = CurTok.lexeme;
    getNextToken();

    //TODO: Error
    return make_result(std::move(name));
  };

static auto token_type =
  [](){
    TOKEN_TYPE tp = (TOKEN_TYPE) CurTok.type;
    getNextToken();
    return make_result(std::move(tp));
  };

static void expect(TOKEN_TYPE type) {
  if(CurTok.type != type) {
    LogError(std::string("Expected token ") + std::to_string(type) + " but got " + std::to_string(CurTok.type));
    LogError(std::to_string(lineNo) + ":" + std::to_string(columnNo));
    return;
  }
  getNextToken();
}

static std::vector<TOKEN> matchPattern(std::vector<TOKEN_TYPE> &pattern) {
  std::vector<TOKEN> tokens;

  for(auto token_type: pattern) {
    expect(token_type);
    tokens.push_back(CurTok);
    getNextToken();
  }

  return tokens;
}

static VariableType coerceVarType(TOKEN_TYPE type) {
  if(isVarTypeFirst()) {
    return (VariableType) type;
  }
  
  //TODO: Error
  LogError("Invalid variable type");
  return VariableType::INT;
}

static TypeSpecType coerceTypeSpec(TOKEN_TYPE type) {
  if(isTypeSpecFirst()) {
    return (TypeSpecType) type;
  }

  //TODO: Error
  LogError("Invalid type specifier");
  return TypeSpecType::VOID;
}

static void coerceIdent() {
  int type = CurTok.type;
  if(type != TOKEN_TYPE::IDENT) {
    LogError("Expected identifier");
  }
}

///-----------------------------------------------------------------------------
/// Expression Parsing
///-----------------------------------------------------------------------------
#pragma region Expression Parsing

static auto literals =
  []() {
    if(CurTok.type == TOKEN_TYPE::INT_LIT) {
      int value = IntVal;
      getNextToken();

      //return ResultMonad(unique_ptr_cast<ExprASTNode>(IntASTNode(CurTok, value)));
      return make_result(unique_ptr_cast<ExprASTNode>(IntASTNode(CurTok, value)));
    } else if(CurTok.type == TOKEN_TYPE::FLOAT_LIT) {
      float value = FloatVal;
      getNextToken();

      // return ResultMonad(unique_ptr_cast<ExprASTNode>(FloatASTNode(CurTok, value)));
      return make_result(unique_ptr_cast<ExprASTNode>(FloatASTNode(CurTok, value)));
    } else { //if(CurTok.type == TOKEN_TYPE::BOOL_LIT)
      bool value = BoolVal;
      getNextToken();

      return make_result(unique_ptr_cast<ExprASTNode>(BoolASTNode(CurTok, value)));
    }

    //TODO: ERROR
  };

static auto expr = or_expr;

//ParserFunction<std::vector<std::unique_ptr<ExprASTNode>>>
static auto args = 
  []() {
    expect(TOKEN_TYPE::LPAR);
    std::vector<std::unique_ptr<ExprASTNode>> args;

    if(CurTok.type != TOKEN_TYPE::RPAR) {
      Consume(ExprASTNode, arg, expr);
      args.push_back(std::move(arg));
    }

    while(CurTok.type == TOKEN_TYPE::COMMA) {
      getNextToken();
      Consume(ExprASTNode, arg, expr);
      args.push_back(std::move(arg));
    }

    expect(TOKEN_TYPE::RPAR);

    return ResultMonad(std::move(args));
  };

static auto primary_expr = 
  []() {
    if(CurTok.type == TOKEN_TYPE::IDENT) {
      ConsumeVal(std::string, varName, ident);

      if(CurTok.type == TOKEN_TYPE::LPAR) {
        Consume(std::vector<std::unique_ptr<ExprASTNode>>, nodes, args);
        return ResultMonad(unique_ptr_cast<ExprASTNode>(CallExprAST(CurTok, std::move(varName), std::move(*nodes.release()))));
      }

      if(CurTok.type == TOKEN_TYPE::ASSIGN) {
        getNextToken();
        Consume(ExprASTNode, node, expr);
        return unique_ptr_cast<ExprASTNode>(AssignmentASTNode(CurTok, std::move(varName), std::move(node)));
      }

      return unique_ptr_cast<ExprASTNode>(VariableRefASTNode(CurTok, std::move(varName)));
    } else {
      return literals();
    }
  };


static auto parentheses_expr = 
  []() {
    if(CurTok.type == TOKEN_TYPE::LPAR) {
      getNextToken();
      Consume(ExprASTNode, primary, primary_expr);
      expect(TOKEN_TYPE::RPAR);
      return ResultMonad(std::move(primary));
    }

    return primary_expr();
  };

static auto unary_expr = 
  []() -> ResultMonad<ExprASTNode> {
    if(CurTok.type == TOKEN_TYPE::MINUS || CurTok.type == TOKEN_TYPE::NOT) {
      ConsumeVal(TOKEN_TYPE, op, token_type);
      Consume(ExprASTNode, unary, unary_expr);
      
      return ResultMonad(unique_ptr_cast<ExprASTNode>(UnaryASTNode(CurTok, op, std::move(unary))));
    }

    return parentheses_expr();
  };

static auto mul_expr = 
  []() -> ResultMonad<ExprASTNode> {
    Consume(ExprASTNode, lhs, unary_expr);

    if(CurTok.type == TOKEN_TYPE::ASTERIX || CurTok.type == TOKEN_TYPE::DIV || CurTok.type == TOKEN_TYPE::MOD) {
      ConsumeVal(TOKEN_TYPE, op, token_type);
      Consume(ExprASTNode, rhs, mul_expr);

      return ResultMonad(unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, op, std::move(lhs), std::move(rhs))));
    }

    return lhs;
  };

static auto add_expr = 
  []() -> ResultMonad<ExprASTNode> {
    Consume(ExprASTNode, lhs, mul_expr);

    if(CurTok.type == TOKEN_TYPE::PLUS || CurTok.type == TOKEN_TYPE::MINUS) {
      ConsumeVal(TOKEN_TYPE, op, token_type);
      Consume(ExprASTNode, rhs, add_expr);
  
      return ResultMonad(unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, op, std::move(lhs), std::move(rhs))));
    }

    return lhs;
  };


static auto rel_expr = 
  []() -> ResultMonad<ExprASTNode> {
    Consume(ExprASTNode, lhs, add_expr);

    if(CurTok.type == TOKEN_TYPE::LT || CurTok.type == TOKEN_TYPE::LE || CurTok.type == TOKEN_TYPE::GT || CurTok.type == TOKEN_TYPE::GE) {
      ConsumeVal(TOKEN_TYPE, op, token_type);
      Consume(ExprASTNode, rhs, rel_expr);

      return ResultMonad(unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, op, std::move(lhs), std::move(rhs))));
    }

    return lhs;
  };

static auto eq_expr = 
  []() -> ResultMonad<ExprASTNode> {
    Consume(ExprASTNode, lhs, rel_expr);

    if(CurTok.type == TOKEN_TYPE::EQ || CurTok.type == TOKEN_TYPE::NE) {
      Consume(TOKEN_TYPE, op, token_type);
      Consume(ExprASTNode, rhs, eq_expr);

      return ResultMonad(unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, *op.release(), std::move(lhs), std::move(rhs))));
    }

    return lhs;
  };

static auto and_expr = 
  []() -> ResultMonad<ExprASTNode> {
    Consume(ExprASTNode, lhs, eq_expr);

    if(CurTok.type == TOKEN_TYPE::AND) {
      getNextToken();
      Consume(ExprASTNode, rhs, and_expr);

      return ResultMonad(unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, TOKEN_TYPE::AND, std::move(lhs), std::move(rhs))));
    }

    return lhs;
  };

inline static ResultMonad<ExprASTNode> or_expr()
  {
    Consume(ExprASTNode, lhs, and_expr);

    if(CurTok.type == TOKEN_TYPE::OR) {
      getNextToken();
      Consume(ExprASTNode, rhs, or_expr);

      return ResultMonad(unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, TOKEN_TYPE::OR, std::move(lhs), std::move(rhs))));
    }

    return lhs;
  }

#pragma endregion

///-----------------------------------------------------------------------------
/// Statement Parsing
///-----------------------------------------------------------------------------
#pragma region Statement Parsing

inline static ResultMonad<StatementASTNode> stmt();

static ParserFunction<std::vector<std::unique_ptr<StatementASTNode>>> stmt_list =
  []() -> ResultMonad<std::vector<std::unique_ptr<StatementASTNode>>> {
    std::vector<std::unique_ptr<StatementASTNode>> statements;

    while(isStmtFirst()) {
      Consume(StatementASTNode, node, stmt);
      statements.push_back(std::move(node));
    }

    return ResultMonad(std::move(statements));
  };

static ParserFunction<std::unique_ptr<VariableDeclASTNode>> local_decl =
  []() -> ResultMonad<std::unique_ptr<VariableDeclASTNode>> {
    ConsumeVal(VariableType, type, var_type);
    ConsumeVal(std::string, value, ident);
    expect(TOKEN_TYPE::SC);

    return ResultMonad(VariableDeclASTNode(CurTok, value, type));
  };

static ParserFunction<std::vector<std::unique_ptr<DeclASTNode>>> local_decl_list =
  []() -> ResultMonad<std::vector<std::unique_ptr<DeclASTNode>>> {
    std::vector<std::unique_ptr<DeclASTNode>> decls;

    while(isVarTypeFirst()) {
      Consume(DeclASTNode, decl, local_decl);
      decls.push_back(std::move(decl));
    }

    return ResultMonad(std::move(decls));
  };

static ParserFunction<std::unique_ptr<BlockASTNode>> block = 
  []() -> ResultMonad<std::unique_ptr<BlockASTNode>> {
    expect(TOKEN_TYPE::LBRA);

    Consume(std::vector<std::unique_ptr<DeclASTNode>>, decls, local_decl_list);
    // std::vector<std::unique_ptr<DeclASTNode>> decls;
    // { 
    //   ResultMonad<std::vector<std::unique_ptr<DeclASTNode>>> res = local_decl_list();
    //   if(res.success()) { 
    //     decls = *res.unwrap_val();
    //   } else { 
    //     return res;
    //   }
    // }
    
    Consume(std::vector<std::unique_ptr<StatementASTNode>>, statements, stmt_list);

    expect(TOKEN_TYPE::RBRA);

    return ResultMonad(BlockASTNode(std::move(*decls.release()), std::move(*statements.release())));
  };

static ParserFunction<IfElseASTNode> if_stmt =
  []() -> ResultMonad<IfElseASTNode> {
    std::unique_ptr<BlockASTNode> else_branch;

    expect(TOKEN_TYPE::IF);
    expect(TOKEN_TYPE::LPAR);
    Consume(ExprASTNode, condition, expr);
    expect(TOKEN_TYPE::RPAR);
    Consume(BlockASTNode, then_branch, block);
    if(CurTok.type == TOKEN_TYPE::ELSE) {
      getNextToken();
      ConsumeAssign(BlockASTNode, else_branch, block);
    }

    return ResultMonad(IfElseASTNode(std::move(condition), std::move(then_branch), std::move(else_branch)));
  };

static ParserFunction<WhileASTNode> while_stmt =
  []() -> ResultMonad<WhileASTNode> {
    expect(TOKEN_TYPE::WHILE);
    expect(TOKEN_TYPE::LPAR);
    Consume(ExprASTNode, condition, expr);
    expect(TOKEN_TYPE::RPAR);
    Consume(BlockASTNode, body, block);

    return WhileASTNode(std::move(condition), std::move(body));
  };

static ParserFunction<ReturnStmtASTNode> return_stmt =
  []() -> ResultMonad<ReturnStmtASTNode> {
    std::unique_ptr<ExprASTNode> exp;

    expect(TOKEN_TYPE::RETURN);
    if(CurTok.type != TOKEN_TYPE::SC) {
      ConsumeAssign(ExprASTNode, exp, expr);
    }
    expect(TOKEN_TYPE::SC);

    return ResultMonad(ReturnStmtASTNode(std::move(exp)));
  };

static ParserFunction<AssignmentStmtASTNode> assign_stmt =
  []() -> ResultMonad<AssignmentStmtASTNode> {
    ConsumeVal(std::string, name, ident);
    expect(TOKEN_TYPE::ASSIGN);
    Consume(ExprASTNode, exp, expr);
    expect(TOKEN_TYPE::SC);

    return ResultMonad(AssignmentStmtASTNode(CurTok, name, std::move(exp)));
  };

inline static ResultMonad<StatementASTNode> stmt() {
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
      expect(TOKEN_TYPE::SC);
      return ResultMonad(std::move(stmt));
    }

  getNextToken();
  //TODO: error;
  return ResultMonad(EmptyStatementASTNode());
}
#pragma endregion


static ParserFunction<FunctionParameterASTNode> param_decl =
  []() -> ResultMonad<FunctionParameterASTNode> { 
    ConsumeVal(VariableType, type, var_type);
    ConsumeVal(std::string, value, ident);

    return ResultMonad(FunctionParameterASTNode(CurTok, value, type));
  };

static ParserFunction<std::vector<std::unique_ptr<FunctionParameterASTNode>>> func_params =
  []() -> ResultMonad<std::vector<std::unique_ptr<FunctionParameterASTNode>>> {
    std::vector<std::unique_ptr<FunctionParameterASTNode>> params;

    if(CurTok.type == TOKEN_TYPE::VOID_TOK) {
      getNextToken();
      return ResultMonad(std::move(params));
    }

    while(isVarTypeFirst()) {
      Consume(FunctionParameterASTNode, param, param_decl);
      params.push_back(std::move(param));
    }

    return params;
  };

static ParserFunction<ExternFunctionDeclASTNode> extern_decl = 
  []() -> ResultMonad<ExternFunctionDeclASTNode> {
    getNextToken();
    ConsumeVal(TypeSpecType, type, type_spec);
    ConsumeVal(std::string, name, ident);
    expect(TOKEN_TYPE::LPAR);
    Consume(std::vector<std::unique_ptr<FunctionParameterASTNode>>, params, func_params);
    expect(TOKEN_TYPE::RPAR);
    expect(TOKEN_TYPE::SC); 

    return ResultMonad(ExternFunctionDeclASTNode(name, std::move(*params.release()), type));
  };


static ResultMonad<std::vector<std::unique_ptr<ExternFunctionDeclASTNode>>> extern_list() { 
  std::vector<std::unique_ptr<ExternFunctionDeclASTNode>> func_decls;

  while(CurTok.type == TOKEN_TYPE::EXTERN) {
    Consume(ExternFunctionDeclASTNode, decl, extern_decl);
    func_decls.push_back(std::move(decl));
  }

  return ResultMonad(std::move(func_decls));
}

static ParserFunction<DeclASTNode> decl =
 []() -> ResultMonad<DeclASTNode> {
    std::string name;

    TypeSpecType type;
    if(CurTok.type == TOKEN_TYPE::VOID_TOK) {
      type = TypeSpecType::VOID;
      getNextToken();
      ConsumeAssignVal(std::string, name, ident);
    } else {
      ConsumeAssignVal(TypeSpecType, type, (ResultMonad<TypeSpecType>) var_type);
      ConsumeAssignVal(std::string, name, ident);
      if(CurTok.type == TOKEN_TYPE::SC) {
        //return unique_ptr_cast<DeclASTNode, VariableDeclASTNode>(VariableDeclASTNode(CurTok, name, (VariableType) type));
        return ResultMonad(VariableDeclASTNode(CurTok, name, (VariableType) type));
      }
    }
    expect(TOKEN_TYPE::LPAR);
    Consume(std::vector<std::unique_ptr<FunctionParameterASTNode>>, params, func_params);
    expect(TOKEN_TYPE::RPAR);

    Consume(BlockASTNode, node, block);

    // return std::unique_ptr<DeclASTNode>(
    //   std::make_unique<FunctionDeclASTNode>(FunctionDeclASTNode(name, std::move(params), node, type))
    // );
    return ResultMonad(FunctionDeclASTNode(name, std::move(*params.release()), std::move(node), type));
  };

ResultMonad<std::vector<std::unique_ptr<DeclASTNode>>> decl_list() {
  std::vector<std::unique_ptr<DeclASTNode>> func_decls;
  
  while(isTypeSpecFirst()) {
    //Consume(DeclASTNode, node, decl);
    std::unique_ptr<std::unique_ptr<DeclASTNode>> node;
    { ResultMonad<std::unique_ptr<DeclASTNode>> res = decl();
    if(res.success()) { node = res.unwrap(); } else { return res; }}
    func_decls.push_back(std::move(*node.release()));
  }

  return ResultMonad(std::move(func_decls));
}

// program ::= extern_list decl_list
ResultMonad<ProgramASTNode> parser() {
  // Make sure we are at the beginning of the program
  fseek(pFile, 0, SEEK_SET);

  getNextToken();

  Consume(std::vector<std::unique_ptr<ExternFunctionDeclASTNode>>, extern_func_decls, extern_list);
  Consume(std::vector<std::unique_ptr<DeclASTNode>>, decls, decl_list);

  return ResultMonad(ProgramASTNode(std::move(*extern_func_decls.release()), std::move(*decls.release())));
}
