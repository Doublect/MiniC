#include<deque>

#include "parser.hpp"

#include "helpers.hpp"

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

inline static std::unique_ptr<ExprASTNode> or_expr();

///----------------------------------------------------------------------------
/// Parser Errors
///----------------------------------------------------------------------------
/// LogError* - These are little helper functions for error handling.
static std::unique_ptr<ASTNode> LogError(std::string Str) {
  fprintf(stderr, "LogError: %s\n", Str.c_str());
  return nullptr;
}

//using ParserFunction = std::function<ASTNode()>;

ParserFunction<TypeSpecType> type_spec = 
  [](){
    if(isTypeSpecFirst()) {
      return (TypeSpecType) CurTok.type;
    }

    //TODO: error
    return TypeSpecType::VOID;
  };

ParserFunction<VariableType> var_type = 
  [](){
    if(isVarTypeFirst()) {
      return (VariableType) CurTok.type;
    }

    //TODO: Error
    return VariableType::INT;
  };

static ParserFunction<std::string> ident =
  [](){
    if(CurTok.type == TOKEN_TYPE::IDENT) {
      return CurTok.lexeme;
    }

    //TODO: Error
    return CurTok.lexeme;
  };

static void expect(TOKEN_TYPE type) {
  if(CurTok.type != type) {
    LogError(std::string("Expected token") + std::to_string(type) + " but got " + std::to_string(CurTok.type));
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

static ParserFunction<std::unique_ptr<ExprASTNode>> literals =
  []() -> std::unique_ptr<ExprASTNode> {
    if(CurTok.type == TOKEN_TYPE::INT_LIT) {
      int value = IntVal;
      getNextToken();

      return unique_ptr_cast<ExprASTNode>(IntASTNode(CurTok, value));
    } else if(CurTok.type == TOKEN_TYPE::FLOAT_LIT) {
      float value = FloatVal;
      getNextToken();

      return unique_ptr_cast<ExprASTNode>(FloatASTNode(CurTok, value));
    } else { //if(CurTok.type == TOKEN_TYPE::BOOL_LIT)
      bool value = BoolVal;
      getNextToken();

      return unique_ptr_cast<ExprASTNode>(BoolASTNode(CurTok, value));
    }

    //TODO: ERROR
  };

static ParserFunction<std::unique_ptr<ExprASTNode>> expr = or_expr;

static ParserFunction<std::vector<std::unique_ptr<ExprASTNode>>> args = 
  [](){
    expect(TOKEN_TYPE::LPAR);
    std::vector<std::unique_ptr<ExprASTNode>> args;

    if(CurTok.type != TOKEN_TYPE::RPAR) {
      args.push_back(expr());
    }

    while(CurTok.type == TOKEN_TYPE::COMMA) {
      getNextToken();
      args.push_back(expr());
    }

    expect(TOKEN_TYPE::RPAR);

    return args;
  };

static ParserFunction<std::unique_ptr<ExprASTNode>> primary_expr = 
  []() -> std::unique_ptr<ExprASTNode> {
    if(CurTok.type == TOKEN_TYPE::IDENT) {
      std::string varName = ident();
      getNextToken();

      if(CurTok.type == TOKEN_TYPE::LPAR) {
        return unique_ptr_cast<ExprASTNode>(CallExprAST(CurTok, std::move(varName), args()));
      }

      if(CurTok.type == TOKEN_TYPE::ASSIGN) {
        getNextToken();
        return unique_ptr_cast<ExprASTNode>(AssignmentASTNode(CurTok, std::move(varName), expr()));
      }

      return unique_ptr_cast<ExprASTNode>(VariableRefASTNode(CurTok, std::move(varName)));
    } else {
      return literals();
    }
  };


static ParserFunction<std::unique_ptr<ExprASTNode>> parentheses_expr = 
  [](){
    if(CurTok.type == TOKEN_TYPE::LPAR) {
      getNextToken();
      std::unique_ptr<ExprASTNode> expr = primary_expr();
      expect(TOKEN_TYPE::RPAR);
      return expr;
    }

    return primary_expr();
  };

static ParserFunction<std::unique_ptr<ExprASTNode>> unary_expr = 
  []() -> std::unique_ptr<ExprASTNode> {
    if(CurTok.type == TOKEN_TYPE::MINUS || CurTok.type == TOKEN_TYPE::NOT) {
      TOKEN_TYPE op = (TOKEN_TYPE) CurTok.type;
      getNextToken();
      return unique_ptr_cast<ExprASTNode>(UnaryASTNode(CurTok, op, std::move(unary_expr())));
    }

    return parentheses_expr();
  };

static ParserFunction<std::unique_ptr<ExprASTNode>> mul_expr = 
  []() -> std::unique_ptr<ExprASTNode> {
    std::unique_ptr<ExprASTNode> lhs = unary_expr();

    if(CurTok.type == TOKEN_TYPE::ASTERIX || CurTok.type == TOKEN_TYPE::DIV || CurTok.type == TOKEN_TYPE::MOD) {
      TOKEN_TYPE op = (TOKEN_TYPE) CurTok.type;
      getNextToken();
      auto rhs = mul_expr();

      return unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, op, std::move(lhs), std::move(rhs)));
    }

    return lhs;
  };

static ParserFunction<std::unique_ptr<ExprASTNode>> add_expr = 
  []() -> std::unique_ptr<ExprASTNode> {
    std::unique_ptr<ExprASTNode> lhs = mul_expr();

    if(CurTok.type == TOKEN_TYPE::PLUS || CurTok.type == TOKEN_TYPE::MINUS) {
      TOKEN_TYPE op = (TOKEN_TYPE) CurTok.type;
      getNextToken();
      auto rhs = add_expr();

      return unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, op, std::move(lhs), std::move(rhs)));
    }

    return lhs;
  };


static ParserFunction<std::unique_ptr<ExprASTNode>> rel_expr = 
  []() -> std::unique_ptr<ExprASTNode> {
    std::unique_ptr<ExprASTNode> lhs = add_expr();

    if(CurTok.type == TOKEN_TYPE::LT || CurTok.type == TOKEN_TYPE::LE || CurTok.type == TOKEN_TYPE::GT || CurTok.type == TOKEN_TYPE::GE) {
      TOKEN_TYPE op = (TOKEN_TYPE) CurTok.type;
      getNextToken();
      auto rhs = rel_expr();

      return unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, op, std::move(lhs), std::move(rhs)));
    }

    return lhs;
  };

static ParserFunction<std::unique_ptr<ExprASTNode>> eq_expr = 
  []() -> std::unique_ptr<ExprASTNode> {
    std::unique_ptr<ExprASTNode> lhs = rel_expr();

    if(CurTok.type == TOKEN_TYPE::EQ || CurTok.type == TOKEN_TYPE::NE) {
      TOKEN_TYPE op = (TOKEN_TYPE) CurTok.type;
      getNextToken();
      auto rhs = eq_expr();

      return unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, op, std::move(lhs), std::move(rhs)));
    }

    return lhs;
  };

static ParserFunction<std::unique_ptr<ExprASTNode>> and_expr = 
  []() -> std::unique_ptr<ExprASTNode> {
    std::unique_ptr<ExprASTNode> lhs = eq_expr();

    if(CurTok.type == TOKEN_TYPE::AND) {
      getNextToken();
      auto rhs = and_expr();

      return unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, TOKEN_TYPE::AND, std::move(lhs), std::move(rhs)));
    }

    return lhs;
  };

inline static std::unique_ptr<ExprASTNode> or_expr()
  {
    std::unique_ptr<ExprASTNode> lhs = and_expr();

    if(CurTok.type == TOKEN_TYPE::OR) {
      getNextToken();
      auto rhs = or_expr();

      return unique_ptr_cast<ExprASTNode>(BinaryASTNode(CurTok, TOKEN_TYPE::OR, std::move(lhs), std::move(rhs)));
    }

    return lhs;
  }

#pragma endregion

///-----------------------------------------------------------------------------
/// Statement Parsing
///-----------------------------------------------------------------------------
#pragma region Statement Parsing

inline static std::unique_ptr<StatementASTNode> stmt();

static ParserFunction<std::vector<std::unique_ptr<StatementASTNode>>> stmt_list =
  [](){
    std::vector<std::unique_ptr<StatementASTNode>> statements;

    while(isStmtFirst()) {
      statements.push_back(stmt());
    }

    return statements;
  };

static ParserFunction<std::unique_ptr<VariableDeclASTNode>> local_decl =
  [](){
    VariableType type = var_type();
    getNextToken();
    const std::string value = ident();
    getNextToken();
    expect(TOKEN_TYPE::SC);

    return std::make_unique<VariableDeclASTNode>(VariableDeclASTNode(CurTok, value, type));
  };

static ParserFunction<std::vector<std::unique_ptr<DeclASTNode>>> local_decl_list =
  [](){
    std::vector<std::unique_ptr<DeclASTNode>> decls;

    while(isVarTypeFirst()) {
      decls.push_back(local_decl());
    }

    return decls;
  };

static ParserFunction<std::unique_ptr<BlockASTNode>> block = 
  [](){
    std::vector<std::unique_ptr<DeclASTNode>> decls;
    std::vector<std::unique_ptr<StatementASTNode>> statements;

    expect(TOKEN_TYPE::LBRA);

    decls = local_decl_list();
    statements = stmt_list();

    expect(TOKEN_TYPE::RBRA);

    return std::make_unique<BlockASTNode>(BlockASTNode(std::move(statements)));
  };

static ParserFunction<IfElseASTNode> if_stmt =
  [](){
    std::unique_ptr<ExprASTNode> condition;
    std::unique_ptr<BlockASTNode> then_branch;
    std::unique_ptr<BlockASTNode> else_branch;

    expect(TOKEN_TYPE::IF);
    expect(TOKEN_TYPE::LPAR);
    condition = expr();
    expect(TOKEN_TYPE::RPAR);
    then_branch = block();
    if(CurTok.type == TOKEN_TYPE::ELSE) {
      getNextToken();
      else_branch = block();
    }

    return IfElseASTNode(std::move(condition), std::move(then_branch), std::move(else_branch));
  };

static ParserFunction<WhileASTNode> while_stmt =
  [](){
    std::unique_ptr<ExprASTNode> condition;
    std::unique_ptr<BlockASTNode> body;

    expect(TOKEN_TYPE::WHILE);
    expect(TOKEN_TYPE::LPAR);
    condition = expr();
    expect(TOKEN_TYPE::RPAR);
    body = block();

    return WhileASTNode(std::move(condition), std::move(body));
  };

static ParserFunction<ReturnStmtASTNode> return_stmt =
  [](){
    std::unique_ptr<ExprASTNode> exp;

    expect(TOKEN_TYPE::RETURN);
    if(CurTok.type != TOKEN_TYPE::SC) {
      exp = expr();
    }
    expect(TOKEN_TYPE::SC);

    return ReturnStmtASTNode(std::move(exp));
  };

static ParserFunction<AssignmentStmtASTNode> assign_stmt =
  [](){
    std::string name;
    std::unique_ptr<ExprASTNode> exp;

    name = ident();
    expect(TOKEN_TYPE::ASSIGN);
    exp = expr();
    expect(TOKEN_TYPE::SC);

    return AssignmentStmtASTNode(CurTok, name, std::move(exp));
  };

inline static std::unique_ptr<StatementASTNode> stmt() {
    if(CurTok.type == TOKEN_TYPE::IF) {
      return unique_ptr_cast<StatementASTNode>(if_stmt());
    } else if(CurTok.type == TOKEN_TYPE::WHILE) {
      return unique_ptr_cast<StatementASTNode>(while_stmt());
    } else if(CurTok.type == TOKEN_TYPE::RETURN) {
      return unique_ptr_cast<StatementASTNode>(return_stmt());
    } else if(CurTok.type == TOKEN_TYPE::LBRA) {
      return std::unique_ptr<StatementASTNode>(block());
    } else if(isExprFirst()) {
      std::unique_ptr<ExprASTNode> stmt = expr();
      expect(TOKEN_TYPE::SC);
      return std::unique_ptr<StatementASTNode>(std::move(stmt));
    }

  getNextToken();
  //TODO: error;
  return unique_ptr_cast<StatementASTNode>(EmptyStatementASTNode());
}
#pragma endregion


static ParserFunction<FunctionParameterASTNode> param_decl =
  [](){
    VariableType type = var_type();
    getNextToken();
    const std::string value = ident(); 

    return FunctionParameterASTNode(CurTok, value, type);
  };

static ParserFunction<std::vector<std::unique_ptr<FunctionParameterASTNode>>> func_params =
  [](){
    std::vector<std::unique_ptr<FunctionParameterASTNode>> params;

    if(CurTok.type == TOKEN_TYPE::VOID_TOK) {
      getNextToken();
      return params;
    }

    while(isVarTypeFirst()) {
      params.push_back(std::make_unique<FunctionParameterASTNode>(param_decl()));
      getNextToken();
    }

    return params;
  };

static ParserFunction<ExternFunctionDeclASTNode> extern_decl = 
  [](){
    TypeSpecType type;
    std::string name;
    std::vector<std::unique_ptr<FunctionParameterASTNode>> params;
    
    getNextToken();
    type = type_spec();
    getNextToken();
    name = ident();
    getNextToken();
    expect(TOKEN_TYPE::LPAR);
    params = func_params();
    expect(TOKEN_TYPE::RPAR);
    expect(TOKEN_TYPE::SC); 

    return ExternFunctionDeclASTNode(name, std::move(params), type);
  };


static std::vector<std::unique_ptr<ExternFunctionDeclASTNode>> extern_list() { 
  std::vector<std::unique_ptr<ExternFunctionDeclASTNode>> func_decls;

  while(CurTok.type == TOKEN_TYPE::EXTERN) {
    func_decls.push_back(std::make_unique<ExternFunctionDeclASTNode>(extern_decl()));
  }

  return func_decls;
}

static ParserFunction<std::unique_ptr<DeclASTNode>> decl =
 []() -> std::unique_ptr<DeclASTNode> {
    std::string name;
    std::vector<std::unique_ptr<FunctionParameterASTNode>> params;

    TypeSpecType type;
    if(CurTok.type == TOKEN_TYPE::VOID_TOK) {
      type = TypeSpecType::VOID;
      getNextToken();
      name = ident();
      getNextToken();
    } else {
      type = (TypeSpecType) var_type();
      getNextToken();
      name = ident();
      getNextToken();
      if(CurTok.type == TOKEN_TYPE::SC) {
        return unique_ptr_cast<DeclASTNode, VariableDeclASTNode>(VariableDeclASTNode(CurTok, name, (VariableType) type));
      }
    }
    expect(TOKEN_TYPE::LPAR);
    params = func_params();
    expect(TOKEN_TYPE::RPAR);

    return std::unique_ptr<DeclASTNode>(
      std::make_unique<FunctionDeclASTNode>(FunctionDeclASTNode(name, std::move(params), block(), type))
    );
  };

std::vector<std::unique_ptr<DeclASTNode>> decl_list() {
  std::vector<std::unique_ptr<DeclASTNode>> func_decls;
  
  while(isTypeSpecFirst()) {
    func_decls.push_back(decl());
  }

  return func_decls;
}

// program ::= extern_list decl_list
ProgramASTNode parser() {
  // Make sure we are at the beginning of the program
  fseek(pFile, 0, SEEK_SET);

  getNextToken();

  std::vector<std::unique_ptr<ExternFunctionDeclASTNode>> extern_func_decls = extern_list();
  std::vector<std::unique_ptr<DeclASTNode>> decls = decl_list();

  return ProgramASTNode(std::move(extern_func_decls), std::move(decls));
}
