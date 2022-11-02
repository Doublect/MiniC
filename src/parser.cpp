#include<deque>
#include <memory>

#include "helpers.hpp"
#include "parser.hpp"

#define BOOST_STACKTRACE_USE_ADDR2LINE
#include <boost/stacktrace.hpp>

FILE *pFile;

extern int lineNo, columnNo;

using namespace std::string_literals;

//#define Consume(result) result.success() ? result.unwrap() : { return result; } 
//#define Consume(variable, result) auto res = result(); if(res.success()) { auto variable = res.unwrap(); } else { return res; }
  //std::cout << boost::stacktrace::stacktrace() << std::endl; 
#define Consume(type, variable, result) \ 
  std::unique_ptr<type> variable; \
  { ResultMonad<type> res = result(); \
  if(res.success()) { variable = std::move(res).unwrap(); } else { return res; }}
#define ConsumeAssign(type, variable, result) \ 
  { ResultMonad<type> res = result(); \
  if(res.success()) { variable = std::move(res).unwrap(); } else { return res; }}

#define ConsumeVal(type, variable, result) \ 
  type variable; \
  { ResultMonad<type> res = result(); \
  if(res.success()) { variable = *std::move(res).unwrap().release(); } else { return res; }}
#define ConsumeAssignVal(type, variable, result) \ 
  { ResultMonad<type> res = result(); \
  if(res.success()) { variable = *std::move(res).unwrap_val(); } else { return res; }}

#define LogError(string) \
  return ResultMonad<void>(ErrorT(string));
#define LogErrorType(string, type) \
  return ResultMonad<type>(ErrorT(string));

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

inline static ResultMonad<ExprASTNode> or_expr();

///----------------------------------------------------------------------------
/// Parser Errors
///----------------------------------------------------------------------------
/// LogError* - These are little helper functions for error handling.
// static std::unique_ptr<ASTNode> LogError(std::string Str) {
//   fprintf(stderr, "LogError: %s\n", Str.c_str());
//   return nullptr;
// }

//using ParserFunction = std::function<ASTNode()>;

auto type_spec = 
  []() -> ResultMonad<TypeSpecType> {
    if(isTypeSpecFirst()) {
      TypeSpecType tst = (TypeSpecType) CurTok.type;
      getNextToken();
      return make_result(std::move(tst));
    }

    return make_result(ErrorT("Expected type specifier, got: "s + std::to_string(CurTok.type)));
  };

auto var_type = 
  []() -> ResultMonad<VariableType>{
    if(isVarTypeFirst()) {
      VariableType type = (VariableType) CurTok.type;
      getNextToken();
      return make_result(std::move(type));
    }

    return make_result(ErrorT("Expected variable type, got: "s + std::to_string(CurTok.type)));
  };

auto ident =
  []() -> ResultMonad<std::string> {
    if(CurTok.type == TOKEN_TYPE::IDENT) {
      std::string name = CurTok.lexeme;
      getNextToken();
      return make_result(std::move(name));
    }
    std::string name = CurTok.lexeme;
    getNextToken();

    return make_result(ErrorT("Expected identifier, got: "s + std::to_string(CurTok.type), CurTok));
  };

static auto token_type =
  [](){
    TOKEN_TYPE tp = (TOKEN_TYPE) CurTok.type;
    getNextToken();
    return make_result(std::move(tp));
  };

static ResultMonad<TOKEN> expect(TOKEN_TYPE type) {
  if(CurTok.type != type) {
    auto str = std::string(std::string("Expected token ") + std::to_string(type) + std::string(" but got ") + std::to_string(CurTok.type));
    std::cout << str << std::endl;
    return ResultMonad<TOKEN>(ErrorT(str, lineNo, columnNo));
    //LogError(std::string(std::string("Expected token ") + std::to_string(type) + std::string(" but got ") + std::to_string(CurTok.type)));
    //LogError(std::to_string(lineNo) + ":" + std::to_string(columnNo));
  }
  //std::cout << "Read token: " << CurTok.type << std::endl;
  TOKEN tok = CurTok;
  auto a = make_result(std::move(tok));
  getNextToken();
  return std::move(a);
}

///-----------------------------------------------------------------------------
/// Expression Parsing
///-----------------------------------------------------------------------------
#pragma region Expression Parsing

static auto literals =
  []() -> ResultMonad<ExprASTNode> {
    if(CurTok.type == TOKEN_TYPE::INT_LIT) {
      int value = IntVal;
      getNextToken();

      //return ResultMonad(unique_ptr_cast<ExprASTNode>(IntASTNode(CurTok, value)));
      return make_result_ptr(unique_ptr_cast<ExprASTNode>(IntASTNode(CurTok, value)));
    } else if(CurTok.type == TOKEN_TYPE::FLOAT_LIT) {
      float value = FloatVal;
      getNextToken();

      // return ResultMonad(unique_ptr_cast<ExprASTNode>(FloatASTNode(CurTok, value)));
      return make_result_ptr(unique_ptr_cast<ExprASTNode>(FloatASTNode(CurTok, value)));
    } else { //if(CurTok.type == TOKEN_TYPE::BOOL_LIT)
      bool value = BoolVal;
      getNextToken();

      return make_result_ptr(unique_ptr_cast<ExprASTNode>(BoolASTNode(CurTok, value)));
    }

    return make_result(ErrorT("Expected literal, got: "s + std::to_string(CurTok.type), CurTok));
  };

static ParserFunction<ExprASTNode> expr = or_expr;

static ParserFunction<std::vector<std::unique_ptr<ExprASTNode>>> args = 
  []() -> ResultMonad<std::vector<std::unique_ptr<ExprASTNode>>> {
    Expect(TOKEN_TYPE::LPAR);
    std::vector<std::unique_ptr<ExprASTNode>> args;

    if(CurTok.type != TOKEN_TYPE::RPAR) {
      //Consume(ExprASTNode, arg, expr);

      std::unique_ptr<ExprASTNode> arg;
    { ResultMonad<ExprASTNode> res = expr();
      if(res.success()) { arg = std::move(res).unwrap(); } else { return ResultMonad<std::vector<std::unique_ptr<ExprASTNode>>>(res.error()); }}
      args.push_back(std::move(arg));
    }

    while(CurTok.type == TOKEN_TYPE::COMMA) {
      getNextToken();
      Consume(ExprASTNode, arg, expr);
      args.push_back(std::move(arg));
    }

    Expect(TOKEN_TYPE::RPAR);

    return make_result(std::move(args));
  };

static ParserFunction<ExprASTNode> primary_expr = 
  []() -> ResultMonad<ExprASTNode> {
    if(CurTok.type == TOKEN_TYPE::IDENT) {
      ConsumeVal(std::string, varName, ident);

      if(CurTok.type == TOKEN_TYPE::LPAR) {
        Consume(std::vector<std::unique_ptr<ExprASTNode>>, nodes, args);
        return make_result_ptr(unique_ptr_cast<ExprASTNode>(CallExprAST(CurTok, std::move(varName), std::move(*nodes.release()))));
      }

      if(CurTok.type == TOKEN_TYPE::ASSIGN) {
        getNextToken();
        Consume(ExprASTNode, node, expr);
        return make_result_ptr(unique_ptr_cast<ExprASTNode>(AssignmentASTNode(CurTok, std::move(varName), std::move(node))));
      }

      return make_result_ptr(unique_ptr_cast<ExprASTNode>(VariableRefASTNode(CurTok, std::move(varName))));
    } else {
      return literals();
    }
  };


static ParserFunction<ExprASTNode> parentheses_expr = 
  []() -> ResultMonad<ExprASTNode> {
    if(CurTok.type == TOKEN_TYPE::LPAR) {
      getNextToken();
      Consume(ExprASTNode, primary, primary_expr);
      Expect(TOKEN_TYPE::RPAR);
      return make_result_ptr(std::move(primary));
    }

    return primary_expr();
  };

static ParserFunction<ExprASTNode> unary_expr = 
  []() -> ResultMonad<ExprASTNode> {
    if(CurTok.type == TOKEN_TYPE::MINUS || CurTok.type == TOKEN_TYPE::NOT) {
      ConsumeVal(TOKEN_TYPE, op, token_type);
      Consume(ExprASTNode, unary, unary_expr);
      
      return make_result_ptr(unique_ptr_cast<ExprASTNode>(UnaryASTNode(CurTok, op, std::move(unary))));
    }

    return parentheses_expr();
  };

static ParserFunction<ExprASTNode> mul_expr = 
  []() -> ResultMonad<ExprASTNode> {
    Consume(ExprASTNode, lhs, unary_expr);

    if(CurTok.type == TOKEN_TYPE::ASTERIX || CurTok.type == TOKEN_TYPE::DIV || CurTok.type == TOKEN_TYPE::MOD) {
      ConsumeVal(TOKEN_TYPE, op, token_type);
      Consume(ExprASTNode, rhs, mul_expr);

      return make_result_ptr(unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, op, std::move(lhs), std::move(rhs))));
    }

    return lhs;
  };

static ParserFunction<ExprASTNode> add_expr = 
  []() -> ResultMonad<ExprASTNode> {
    Consume(ExprASTNode, lhs, mul_expr);

    if(CurTok.type == TOKEN_TYPE::PLUS || CurTok.type == TOKEN_TYPE::MINUS) {
      ConsumeVal(TOKEN_TYPE, op, token_type);
      Consume(ExprASTNode, rhs, add_expr);
  
      return make_result_ptr(unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, op, std::move(lhs), std::move(rhs))));
    }

    return lhs;
  };


static ParserFunction<ExprASTNode> rel_expr = 
  []() -> ResultMonad<ExprASTNode> {
    Consume(ExprASTNode, lhs, add_expr);

    if(CurTok.type == TOKEN_TYPE::LT || CurTok.type == TOKEN_TYPE::LE || CurTok.type == TOKEN_TYPE::GT || CurTok.type == TOKEN_TYPE::GE) {
      ConsumeVal(TOKEN_TYPE, op, token_type);
      Consume(ExprASTNode, rhs, rel_expr);

      return make_result_ptr(unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, op, std::move(lhs), std::move(rhs))));
    }

    return lhs;
  };

static ParserFunction<ExprASTNode> eq_expr = 
  []() -> ResultMonad<ExprASTNode> {
    Consume(ExprASTNode, lhs, rel_expr);

    if(CurTok.type == TOKEN_TYPE::EQ || CurTok.type == TOKEN_TYPE::NE) {
      Consume(TOKEN_TYPE, op, token_type);
      Consume(ExprASTNode, rhs, eq_expr);

      return make_result_ptr(unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, *op.release(), std::move(lhs), std::move(rhs))));
    }

    return lhs;
  };

static ParserFunction<ExprASTNode> and_expr = 
  []() -> ResultMonad<ExprASTNode> {
    Consume(ExprASTNode, lhs, eq_expr);

    if(CurTok.type == TOKEN_TYPE::AND) {
      getNextToken();
      Consume(ExprASTNode, rhs, and_expr);

      return make_result_ptr(unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, TOKEN_TYPE::AND, std::move(lhs), std::move(rhs))));
    }

    return lhs;
  };

inline static ResultMonad<ExprASTNode> or_expr()
  {
    Consume(ExprASTNode, lhs, and_expr);

    if(CurTok.type == TOKEN_TYPE::OR) {
      getNextToken();
      Consume(ExprASTNode, rhs, or_expr);

      return make_result_ptr(unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, TOKEN_TYPE::OR, std::move(lhs), std::move(rhs))));
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
      decls.push_back(std::move(std::unique_ptr<DeclASTNode>(std::move(decl))));
    }

    return make_result(std::move(decls));
  };

static ParserFunction<BlockASTNode> block = 
  []() -> ResultMonad<BlockASTNode> {
    Expect(TOKEN_TYPE::LBRA);

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

    Expect(TOKEN_TYPE::RBRA);

    return make_result(BlockASTNode(std::move(*decls.release()), std::move(*statements.release())));
  };

static ParserFunction<IfElseASTNode> if_stmt =
  []() -> ResultMonad<IfElseASTNode> {
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

    return make_result(IfElseASTNode(std::move(condition), std::move(then_branch), std::move(else_branch)));
  };

static ParserFunction<WhileASTNode> while_stmt =
  []() -> ResultMonad<WhileASTNode> {
    Expect(TOKEN_TYPE::WHILE);
    Expect(TOKEN_TYPE::LPAR);
    Consume(ExprASTNode, condition, expr);
    Expect(TOKEN_TYPE::RPAR);
    Consume(BlockASTNode, body, block);

    return make_result(WhileASTNode(std::move(condition), std::move(body)));
  };

static ParserFunction<ReturnStmtASTNode> return_stmt =
  []() -> ResultMonad<ReturnStmtASTNode> {
    std::unique_ptr<ExprASTNode> exp;

    Expect(TOKEN_TYPE::RETURN);
    if(CurTok.type != TOKEN_TYPE::SC) {
      ConsumeAssign(ExprASTNode, exp, expr);
    }
    Expect(TOKEN_TYPE::SC);

    return make_result(ReturnStmtASTNode(std::move(exp)));
  };

static ParserFunction<AssignmentStmtASTNode> assign_stmt =
  []() -> ResultMonad<AssignmentStmtASTNode> {
    ConsumeVal(std::string, name, ident);
    Expect(TOKEN_TYPE::ASSIGN);
    Consume(ExprASTNode, exp, expr);
    Expect(TOKEN_TYPE::SC);

    return make_result(AssignmentStmtASTNode(CurTok, name, std::move(exp)));
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
      Expect(TOKEN_TYPE::SC);
      return make_result_ptr(std::move(stmt));
    }

  return make_result(ErrorT("One of: 'if', 'while', 'return', 'block' or an expression was expected", CurTok.lineNo, CurTok.columnNo));
}
#pragma endregion


static ParserFunction<FunctionParameterASTNode> param_decl =
  []() -> ResultMonad<FunctionParameterASTNode> { 
    ConsumeVal(VariableType, type, var_type);
    ConsumeVal(std::string, value, ident);

    return make_result(FunctionParameterASTNode(CurTok, value, type));
  };

static ParserFunction<std::vector<std::unique_ptr<FunctionParameterASTNode>>> func_params =
  []() -> ResultMonad<std::vector<std::unique_ptr<FunctionParameterASTNode>>> {
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

static ParserFunction<ExternFunctionDeclASTNode> extern_decl = 
  []() -> ResultMonad<ExternFunctionDeclASTNode> {

    Expect(TOKEN_TYPE::EXTERN);
    ConsumeVal(TypeSpecType, type, type_spec);
    ConsumeVal(std::string, name, ident);
    Expect(TOKEN_TYPE::LPAR);
    Consume(std::vector<std::unique_ptr<FunctionParameterASTNode>>, params, func_params);
    Expect(TOKEN_TYPE::RPAR);
    Expect(TOKEN_TYPE::SC); 

    return make_result(ExternFunctionDeclASTNode(name, std::move(*params.release()), type));
  };


static ResultMonad<std::vector<std::unique_ptr<ExternFunctionDeclASTNode>>> extern_list() { 
  std::vector<std::unique_ptr<ExternFunctionDeclASTNode>> func_decls;

  while(CurTok.type == TOKEN_TYPE::EXTERN) {
    Consume(ExternFunctionDeclASTNode, decl, extern_decl);
    func_decls.push_back(std::move(decl));
  }

  return make_result(std::move(func_decls));
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
      //ConsumeAssignVal(VariableType, type, var_type);
      { 
        ResultMonad<VariableType> res = var_type(); \
        if(res.success()) { type = (TypeSpecType)*std::move(res).unwrap_val(); } else { return res; }
      }
      ConsumeAssignVal(std::string, name, ident);
      if(CurTok.type == TOKEN_TYPE::SC) {
        //return unique_ptr_cast<DeclASTNode, VariableDeclASTNode>(VariableDeclASTNode(CurTok, name, (VariableType) type));
        return make_result(VariableDeclASTNode(CurTok, name, (VariableType) type));
      }
    }
    Expect(TOKEN_TYPE::LPAR);
    Consume(std::vector<std::unique_ptr<FunctionParameterASTNode>>, params, func_params);
    Expect(TOKEN_TYPE::RPAR);

    Consume(BlockASTNode, node, block);

    // return std::unique_ptr<DeclASTNode>(
    //   std::make_unique<FunctionDeclASTNode>(FunctionDeclASTNode(name, std::move(params), node, type))
    // );
    return make_result(FunctionDeclASTNode(name, std::move(*params.release()), std::move(node), type));
  };

ResultMonad<std::vector<std::unique_ptr<DeclASTNode>>> decl_list() {
  std::vector<std::unique_ptr<DeclASTNode>> func_decls;
  
  while(isTypeSpecFirst()) {
    Consume(DeclASTNode, node, decl);
    // std::unique_ptr<std::unique_ptr<DeclASTNode>> node;
    // { ResultMonad<std::unique_ptr<DeclASTNode>> res = decl();
    // if(res.success()) { node = res.unwrap(); } else { return res; }}
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
