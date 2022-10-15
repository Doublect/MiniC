#include<deque>
#include<memory>
#include<vector>

#include "ast.cpp"
#include "lexer.cpp"

static FILE *pFile;

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static TOKEN CurTok;
static std::deque<TOKEN> tok_buffer;

static inline TOKEN getNextToken() {

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
    CurTok.type == TOKEN_TYPE::LPAR 
    || CurTok.type == TOKEN_TYPE::LBRA 
    || CurTok.type == TOKEN_TYPE::MINUS 
    || CurTok.type == TOKEN_TYPE::NOT 
    || CurTok.type == TOKEN_TYPE::SC 
    || CurTok.type == TOKEN_TYPE::IDENT 
    || CurTok.type == TOKEN_TYPE::IF
    || CurTok.type == TOKEN_TYPE::WHILE
    || CurTok.type == TOKEN_TYPE::INT_LIT
    || CurTok.type == TOKEN_TYPE::FLOAT_LIT
    || CurTok.type == TOKEN_TYPE::BOOL_LIT
    || CurTok.type == TOKEN_TYPE::INT_TOK
    || CurTok.type == TOKEN_TYPE::FLOAT_TOK
    || CurTok.type == TOKEN_TYPE::BOOL_TOK
    || CurTok.type == TOKEN_TYPE::IDENT
    || CurTok.type == TOKEN_TYPE::RETURN;
}

static bool isIdent() {
  return CurTok.type == TOKEN_TYPE::IDENT;
}

template<typename T> using ParserFunction = std::function<T()>;

inline static ExprASTNode or_expr();

///----------------------------------------------------------------------------
/// Parser Errors
///----------------------------------------------------------------------------
/// LogError* - These are little helper functions for error handling.
static std::unique_ptr<ASTNode> LogError(std::string Str) {
  fprintf(stderr, "LogError: %s\n", Str.c_str());
  return nullptr;
}

//using ParserFunction = std::function<ASTNode()>;

static ParserFunction<TypeSpecType> type_spec = 
  [](){
    if(isTypeSpecFirst()) {
      return (TypeSpecType) CurTok.type;
    }

    //TODO: error
    return TypeSpecType::VOID;
  };

static ParserFunction<VariableType> var_type = 
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

static ParserFunction<ExprASTNode> literals =
  [](){
    if(CurTok.type == TOKEN_TYPE::INT_LIT) {
      int value = IntVal;
      getNextToken();

      return static_cast<ExprASTNode>(IntASTNode(CurTok, value));
    } else if(CurTok.type == TOKEN_TYPE::FLOAT_LIT) {
      float value = FloatVal;
      getNextToken();

      return static_cast<ExprASTNode>(FloatASTNode(CurTok, value));
    } else { //if(CurTok.type == TOKEN_TYPE::BOOL_LIT)
      bool value = BoolVal;
      getNextToken();

      return static_cast<ExprASTNode>(BoolASTNode(CurTok, value));
    }

    //TODO: ERROR
  };

static ParserFunction<ExprASTNode> expr = or_expr;

static ParserFunction<std::vector<std::unique_ptr<ExprASTNode>>> args = 
  [](){
    expect(TOKEN_TYPE::LPAR);
    std::vector<std::unique_ptr<ExprASTNode>> args;

    if(CurTok.type != TOKEN_TYPE::RPAR) {
      args.push_back(std::make_unique<ExprASTNode>(expr()));
    }

    while(CurTok.type == TOKEN_TYPE::COMMA) {
      getNextToken();
      args.push_back(std::make_unique<ExprASTNode>(expr()));
    }

    expect(TOKEN_TYPE::RPAR);

    return args;
  };

static ParserFunction<ExprASTNode> primary_expr = 
  [](){
    if(CurTok.type == TOKEN_TYPE::IDENT) {
      std::string varName = ident();
      getNextToken();

      if(CurTok.type == TOKEN_TYPE::LPAR) {
        return static_cast<ExprASTNode>(CallExprAST(CurTok, std::move(varName), args()));
      }

      if(CurTok.type == TOKEN_TYPE::ASSIGN) {
        getNextToken();
        return static_cast<ExprASTNode>(AssignmentASTNode(CurTok, std::move(varName), std::make_unique<ExprASTNode>(expr())));
      }

      return static_cast<ExprASTNode>(VariableRefASTNode(CurTok, std::move(varName)));
    } else {
      return literals();
    }
  };


static ParserFunction<ExprASTNode> parentheses_expr = 
  [](){
    if(CurTok.type == TOKEN_TYPE::LPAR) {
      getNextToken();
      ExprASTNode expr = primary_expr();
      expect(TOKEN_TYPE::RPAR);
      return expr;
    }

    return primary_expr();
  };

static ParserFunction<ExprASTNode> unary_expr = 
  [](){
    if(CurTok.type == TOKEN_TYPE::MINUS || CurTok.type == TOKEN_TYPE::NOT) {
      TOKEN_TYPE op = (TOKEN_TYPE) CurTok.type;
      getNextToken();
      return static_cast<ExprASTNode>(UnaryASTNode(CurTok, op, std::make_unique<ExprASTNode>(unary_expr())));
    }

    return parentheses_expr();
  };

static ParserFunction<ExprASTNode> mul_expr = 
  [](){
    ExprASTNode lhs = unary_expr();

    if(CurTok.type == TOKEN_TYPE::ASTERIX || CurTok.type == TOKEN_TYPE::DIV) {
      TOKEN_TYPE op = (TOKEN_TYPE) CurTok.type;
      getNextToken();
      auto rhs = mul_expr();

      return static_cast<ExprASTNode>(BinaryASTNode(CurTok, op, std::make_unique<ExprASTNode>(lhs), std::make_unique<ExprASTNode>(rhs)));
    }

    return lhs;
  };

static ParserFunction<ExprASTNode> add_expr = 
  [](){
    ExprASTNode lhs = mul_expr();

    if(CurTok.type == TOKEN_TYPE::PLUS || CurTok.type == TOKEN_TYPE::MINUS) {
      TOKEN_TYPE op = (TOKEN_TYPE) CurTok.type;
      getNextToken();
      auto rhs = add_expr();

      return static_cast<ExprASTNode>(BinaryASTNode(CurTok, op, std::make_unique<ExprASTNode>(lhs), std::make_unique<ExprASTNode>(rhs)));
    }

    return lhs;
  };


static ParserFunction<ExprASTNode> rel_expr = 
  [](){
    ExprASTNode lhs = add_expr();

    if(CurTok.type == TOKEN_TYPE::LT || CurTok.type == TOKEN_TYPE::LE || CurTok.type == TOKEN_TYPE::GT || CurTok.type == TOKEN_TYPE::GE) {
      TOKEN_TYPE op = (TOKEN_TYPE) CurTok.type;
      getNextToken();
      auto rhs = rel_expr();

      return static_cast<ExprASTNode>(BinaryASTNode(CurTok, op, std::make_unique<ExprASTNode>(lhs), std::make_unique<ExprASTNode>(rhs)));
    }

    return lhs;
  };

static ParserFunction<ExprASTNode> eq_expr = 
  [](){
    ExprASTNode lhs = rel_expr();

    if(CurTok.type == TOKEN_TYPE::EQ || CurTok.type == TOKEN_TYPE::NE) {
      TOKEN_TYPE op = (TOKEN_TYPE) CurTok.type;
      getNextToken();
      auto rhs = eq_expr();

      return static_cast<ExprASTNode>(BinaryASTNode(CurTok, op, std::make_unique<ExprASTNode>(lhs), std::make_unique<ExprASTNode>(rhs)));
    }

    return lhs;
  };

static ParserFunction<ExprASTNode> and_expr = 
  [](){
    ExprASTNode lhs = eq_expr();

    if(CurTok.type == TOKEN_TYPE::AND) {
      getNextToken();
      auto rhs = and_expr();

      return static_cast<ExprASTNode>(BinaryASTNode(CurTok, TOKEN_TYPE::AND, std::make_unique<ExprASTNode>(lhs), std::make_unique<ExprASTNode>(rhs)));
    }

    return lhs;
  };

inline static ExprASTNode or_expr()
  {
    ExprASTNode lhs = and_expr();

    if(CurTok.type == TOKEN_TYPE::OR) {
      getNextToken();
      auto rhs = or_expr();

      return static_cast<ExprASTNode>(BinaryASTNode(CurTok, TOKEN_TYPE::OR, std::make_unique<ExprASTNode>(lhs), std::make_unique<ExprASTNode>(rhs)));
    }

    return lhs;
  }

#pragma endregion

///-----------------------------------------------------------------------------
/// Statement Parsing
///-----------------------------------------------------------------------------
#pragma region Statement Parsing
inline static StatementASTNode stmt();

static ParserFunction<std::vector<std::unique_ptr<StatementASTNode>>> stmt_list =
  [](){
    std::vector<std::unique_ptr<StatementASTNode>> statements;

    while(isStmtFirst()) {
      statements.push_back(std::make_unique<StatementASTNode>(stmt()));
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

static ParserFunction<BlockASTNode> block = 
  [](){
    std::vector<std::unique_ptr<DeclASTNode>> decls;
    std::vector<std::unique_ptr<StatementASTNode>> statements;

    expect(TOKEN_TYPE::LBRA);

    decls = local_decl_list();
    statements = stmt_list();

    expect(TOKEN_TYPE::RBRA);

    return BlockASTNode(std::move(statements));
  };

static ParserFunction<IfElseASTNode> if_stmt =
  [](){
    std::unique_ptr<ExprASTNode> condition;
    std::unique_ptr<BlockASTNode> then_branch;
    std::unique_ptr<BlockASTNode> else_branch;

    expect(TOKEN_TYPE::IF);
    expect(TOKEN_TYPE::LPAR);
    condition = std::make_unique<ExprASTNode>(expr());
    expect(TOKEN_TYPE::RPAR);
    then_branch = std::make_unique<BlockASTNode>(block());
    if(CurTok.type == TOKEN_TYPE::ELSE) {
      getNextToken();
      else_branch = std::make_unique<BlockASTNode>(block());
    }

    return IfElseASTNode(std::move(condition), std::move(then_branch), std::move(else_branch));
  };

static ParserFunction<WhileASTNode> while_stmt =
  [](){
    std::unique_ptr<ExprASTNode> condition;
    std::unique_ptr<BlockASTNode> body;

    expect(TOKEN_TYPE::WHILE);
    expect(TOKEN_TYPE::LPAR);
    condition = std::make_unique<ExprASTNode>(expr());
    expect(TOKEN_TYPE::RPAR);
    body = std::make_unique<BlockASTNode>(block());

    return WhileASTNode(std::move(condition), std::move(body));
  };

static ParserFunction<ReturnStmtASTNode> return_stmt =
  [](){
    std::unique_ptr<ExprASTNode> exp;

    expect(TOKEN_TYPE::RETURN);
    if(CurTok.type != TOKEN_TYPE::SC) {
      exp = std::make_unique<ExprASTNode>(expr());
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
    exp = std::make_unique<ExprASTNode>(expr());
    expect(TOKEN_TYPE::SC);

    return AssignmentStmtASTNode(CurTok, name, std::move(exp));
  };

inline static StatementASTNode stmt() {
    if(CurTok.type == TOKEN_TYPE::IF) {
      return static_cast<StatementASTNode>(if_stmt());
    } else if(CurTok.type == TOKEN_TYPE::WHILE) {
      return static_cast<StatementASTNode>(while_stmt());
    } else if(CurTok.type == TOKEN_TYPE::RETURN) {
      return static_cast<StatementASTNode>(return_stmt());
    } else if(CurTok.type == TOKEN_TYPE::LBRA) {
      return static_cast<StatementASTNode>(block());
    } else if(isExprFirst()) {
      StatementASTNode stmt = static_cast<StatementASTNode>(expr());
      expect(TOKEN_TYPE::SC);
      return stmt;
    }

  getNextToken();
  //TODO: error;
  return EmptyStatementASTNode();
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


static ParserFunction<std::vector<std::unique_ptr<ExternFunctionDeclASTNode>>> extern_list = 
  [](){
    std::vector<std::unique_ptr<ExternFunctionDeclASTNode>> func_decls;

    while(CurTok.type == TOKEN_TYPE::EXTERN) {
      func_decls.push_back(std::make_unique<ExternFunctionDeclASTNode>(extern_decl()));
    }

    return func_decls;
  };

static ParserFunction<DeclASTNode> decl =
 [](){
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
        return static_cast<DeclASTNode>(VariableDeclASTNode(CurTok, name, (VariableType) type));
      }
    }
    expect(TOKEN_TYPE::LPAR);
    params = func_params();
    expect(TOKEN_TYPE::RPAR);

    return static_cast<DeclASTNode>(FunctionDeclASTNode(name, std::move(params), std::make_unique<BlockASTNode>(block()), type));
  };

static ParserFunction<std::vector<std::unique_ptr<DeclASTNode>>> decl_list =
 []() {
  std::vector<std::unique_ptr<DeclASTNode>> func_decls;
  
  while(isTypeSpecFirst()) {
    func_decls.push_back(std::make_unique<DeclASTNode>(decl()));
  }

  return func_decls;
 };
