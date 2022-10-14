// #include "llvm/ADT/APFloat.h"
// #include "llvm/ADT/Optional.h"
// #include "llvm/ADT/STLExtras.h"
// #include "llvm/IR/BasicBlock.h"
// #include "llvm/IR/Constants.h"
// #include "llvm/IR/DerivedTypes.h"
// #include "llvm/IR/Function.h"
// #include "llvm/IR/IRBuilder.h"
// #include "llvm/IR/Instructions.h"
// #include "llvm/IR/LLVMContext.h"
// #include "llvm/IR/LegacyPassManager.h"
// #include "llvm/IR/Module.h"
// #include "llvm/IR/Type.h"
// #include "llvm/IR/Verifier.h"
#include "llvm/IR/Value.h"
// #include "llvm/Support/FileSystem.h"
// #include "llvm/Support/Host.h"
// //#include "llvm/Support/TargetRegistry.h"
// #include "llvm/Support/TargetSelect.h"
// #include "llvm/Support/raw_ostream.h"
// #include "llvm/Target/TargetMachine.h"
// #include "llvm/Target/TargetOptions.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <string.h>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

#include "lexer.cpp"

using namespace llvm;
//using namespace llvm::sys;

static FILE *pFile;

enum class VariableType {
  INT = -2,
  FLOAT = -4,
  BOOL = -5,
};

namespace tst {
  enum TypeSpecType {
    INT = -2,
    VOID = -3,
    FLOAT = -4,
    BOOL = -5,
  };
}

using namespace tst;

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
// AST nodes
//===----------------------------------------------------------------------===//
#pragma region AST
/// ASTNode - Base class for all AST nodes.
class ASTNode {
public:
  virtual ~ASTNode() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string() const { return "ASTNode"; };
};

class ExprASTNode : public ASTNode {
public:
  ExprASTNode() {}
  virtual Value *codegen() {return nullptr;};
};

/// AST representation of an integer number
class IntASTNode : public ExprASTNode {
  int Val;
  TOKEN Tok;
  std::string Name;

public:
  IntASTNode(TOKEN tok, int val) : Val(val), Tok(tok) {}
    virtual Value *codegen() {return nullptr;};
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

/// AST representation of a boolean value
class BoolASTNode : public ExprASTNode {
  bool Val;
  TOKEN Tok;
  std::string Name;

public:
  BoolASTNode(TOKEN tok, bool val) : Val(val), Tok(tok) {}
    virtual Value *codegen() {return nullptr;};

};  

/// AST representation of a float literal
class FloatASTNode : public ExprASTNode {
  float Val;
  TOKEN Tok;
  std::string Name;

public:
  FloatASTNode(TOKEN tok, float val) : Val(val), Tok(tok) {}
  virtual Value *codegen() {return nullptr;};
};

///----------------------------------------------------------------------------
/// Expressions
///----------------------------------------------------------------------------

class UnaryASTNode : public ExprASTNode {
  TOKEN_TYPE Op;
  std::unique_ptr<ASTNode> Operand;

  TOKEN Tok;

public:
  UnaryASTNode(TOKEN tok, TOKEN_TYPE op, std::unique_ptr<ASTNode> operand)
      : Op(op), Operand(std::move(operand)), Tok(tok) {}
  virtual Value *codegen() {return nullptr;};
};

/// AST representation of a binary expression
class BinaryASTNode : public ExprASTNode {
  TOKEN_TYPE Op;
  std::unique_ptr<ASTNode> LHS, RHS;
  
  TOKEN Tok;
  std::string Name;

public: 
  BinaryASTNode(TOKEN tok, TOKEN_TYPE op, std::unique_ptr<ASTNode> LHS,
                std::unique_ptr<ASTNode> RHS)
      : Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
  virtual Value *codegen() {return nullptr;};
};

/// AST representation of a variable reference
class VariableRefASTNode : public ExprASTNode {
  std::string Name;
  TOKEN Tok;

public: 
  VariableRefASTNode(TOKEN tok, const std::string &Name) : Name(Name) {}
  virtual Value *codegen() {return nullptr;};
};

// AST representation of a function call
class CallExprAST : public ExprASTNode {
  std::string FunctionName;
  std::vector<std::unique_ptr<ExprASTNode>> Args;

  TOKEN tok;

public:
  CallExprAST(TOKEN tok, const std::string &funcName,
              std::vector<std::unique_ptr<ExprASTNode>> Args)
    : tok(tok), FunctionName(funcName), Args(std::move(Args)) {}
  virtual Value *codegen() {return nullptr;};
};

class AssignmentASTNode : public ExprASTNode {
  std::string Name;
  std::unique_ptr<ASTNode> RHS;
  TOKEN Tok;

public:
  AssignmentASTNode(TOKEN tok, const std::string& Name,
                    std::unique_ptr<ASTNode> RHS)
      : Name(Name), RHS(std::move(RHS)) {}
  virtual Value *codegen() {return nullptr;};
};

///----------------------------------------------------------------------------
/// Statements
///----------------------------------------------------------------------------
#pragma region Statements
class StatementASTNode : public ASTNode {
public:
  StatementASTNode() {}
  virtual Value *codegen() {return nullptr;};
};

class ExprStatementASTNode: public StatementASTNode {
  std::unique_ptr<ExprASTNode> Expr;

public:
  ExprStatementASTNode(std::unique_ptr<ExprASTNode> expr) : Expr(std::move(expr)) {}
  virtual Value *codegen() {return nullptr;};
};

class VariableDeclASTNode;

class BlockAstNode: public StatementASTNode {
  std::vector<std::unique_ptr<VariableDeclASTNode>> Declarations;
  std::vector<std::unique_ptr<StatementASTNode>> Statements;
public:
  BlockAstNode(std::vector<std::unique_ptr<StatementASTNode>> statements) : Statements(std::move(statements)) {}
  virtual Value *codegen() {return nullptr;};
};

class IfElseASTNode : public StatementASTNode {
  std::unique_ptr<ExprASTNode> Cond;
  std::unique_ptr<BlockAstNode> Then;
  std::unique_ptr<BlockAstNode> Else;

public:
  IfElseASTNode(std::unique_ptr<ExprASTNode> Cond, std::unique_ptr<BlockAstNode> Then,
                std::unique_ptr<BlockAstNode> Else)
      : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
  virtual Value *codegen() {return nullptr;};
};

class WhileASTNode: public StatementASTNode {
  std::unique_ptr<ExprASTNode> Cond;
  std::unique_ptr<ASTNode> Body;

public:
  WhileASTNode(std::unique_ptr<ExprASTNode> Cond, std::unique_ptr<BlockAstNode> Body)
      : Cond(std::move(Cond)), Body(std::move(Body)) {}
  virtual Value *codegen() {return nullptr;};
};

class ReturnStmtASTNode: public StatementASTNode {
  std::unique_ptr<ExprASTNode> Expr;

public:
  ReturnStmtASTNode(std::unique_ptr<ExprASTNode> expr) : Expr(std::move(expr)) {}
  virtual Value *codegen() {return nullptr;};
};

class AssignmentStmtASTNode : public StatementASTNode {
  std::string Name;
  std::unique_ptr<ASTNode> RHS;
  TOKEN Tok;

public:
  AssignmentStmtASTNode(TOKEN tok, const std::string& Name,
                    std::unique_ptr<ASTNode> RHS)
      : Name(Name), RHS(std::move(RHS)) {}
  virtual Value *codegen() {return nullptr;};
};
class EmptyStatementASTNode: public StatementASTNode {

  public:
    EmptyStatementASTNode() {}
    virtual Value *codegen() {return nullptr;};
};
#pragma endregion
///----------------------------------------------------------------------------
/// Declarations
///----------------------------------------------------------------------------
#pragma region Declarations
class DeclASTNode : public ASTNode {

public:
  DeclASTNode() {}
  virtual Value *codegen() {return nullptr;};
};

class VariableDeclASTNode : public DeclASTNode {
  std::string Name;
  TOKEN Tok;
  VariableType Type;

public:
  VariableDeclASTNode(TOKEN tok, const std::string &Name, VariableType type) : Name(std::move(Name)), Type(type) {}
  virtual Value *codegen() {return nullptr;};
};

class FunctionParameterASTNode : public VariableDeclASTNode {
  // Inherit constructor
  using VariableDeclASTNode::VariableDeclASTNode;

public:
  virtual Value *codegen() {return nullptr;};
};

class FunctionDeclASTNode : public DeclASTNode {
  std::string Name;
  std::vector<std::unique_ptr<FunctionParameterASTNode>> Args;
  std::unique_ptr<BlockAstNode> Body;
  TypeSpecType ReturnType;

public:
  FunctionDeclASTNode(std::string Name,
                      std::vector<std::unique_ptr<FunctionParameterASTNode>> Args,
                      std::unique_ptr<BlockAstNode> Body, TypeSpecType returnType)
      : Name(Name), Args(std::move(Args)), Body(std::move(Body)), ReturnType(returnType) {}
  virtual Value *codegen() {return nullptr;};
};

class ExternFunctionDeclASTNode : public ASTNode {
  std::string Name;
  std::vector<std::unique_ptr<FunctionParameterASTNode>> Args;
  TypeSpecType ReturnType;

public:
  ExternFunctionDeclASTNode(std::string Name,
                            std::vector<std::unique_ptr<FunctionParameterASTNode>> Args,
                            TypeSpecType returnType)
      : Name(Name), Args(std::move(Args)), ReturnType(returnType) {}
  virtual Value *codegen() {return nullptr;};
};
#pragma endregion
#pragma endregion
//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//

static bool isExternListFirst() {
  return CurTok.type == TOKEN_TYPE::EXTERN;
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
static std::unique_ptr<ASTNode> LogError(const char *Str) {
  fprintf(stderr, "LogError: %s\n", Str);
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
    LogError("Expected token");
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

static ParserFunction<ExprASTNode> expr = 
  [](){
    if(CurTok.type == TOKEN_TYPE::IDENT) {
      std::string varName = ident();
      getNextToken();
      expect(TOKEN_TYPE::EQ);
      return static_cast<ExprASTNode>(AssignmentASTNode(CurTok, std::move(varName), std::move(std::make_unique<ExprASTNode>(expr()))));
    } 

    return static_cast<ExprASTNode>(or_expr());
  };

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
    expect(TOKEN_TYPE::LPAR);
    ExprASTNode expr = primary_expr();
    expect(TOKEN_TYPE::RPAR);

    return expr;
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

static ParserFunction<BlockAstNode> block = 
  [](){
    std::vector<std::unique_ptr<StatementASTNode>> statements;

    expect(TOKEN_TYPE::LBRA);

    while(CurTok.type != TOKEN_TYPE::RBRA) {
       statements = stmt_list();
    }

    expect(TOKEN_TYPE::RBRA);

    return BlockAstNode(std::move(statements));
  };

static ParserFunction<IfElseASTNode> if_stmt =
  [](){
    std::unique_ptr<ExprASTNode> condition;
    std::unique_ptr<BlockAstNode> then_branch;
    std::unique_ptr<BlockAstNode> else_branch;

    expect(TOKEN_TYPE::IF);
    expect(TOKEN_TYPE::LPAR);
    condition = std::make_unique<ExprASTNode>(expr());
    expect(TOKEN_TYPE::RPAR);
    then_branch = std::make_unique<BlockAstNode>(block());
    if(CurTok.type == TOKEN_TYPE::ELSE) {
      getNextToken();
      else_branch = std::make_unique<BlockAstNode>(block());
    }

    return IfElseASTNode(std::move(condition), std::move(then_branch), std::move(else_branch));
  };

static ParserFunction<WhileASTNode> while_stmt =
  [](){
    std::unique_ptr<ExprASTNode> condition;
    std::unique_ptr<BlockAstNode> body;

    expect(TOKEN_TYPE::WHILE);
    expect(TOKEN_TYPE::LPAR);
    condition = std::make_unique<ExprASTNode>(expr());
    expect(TOKEN_TYPE::RPAR);
    body = std::make_unique<BlockAstNode>(block());

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
    } else if(CurTok.type == TOKEN_TYPE::IDENT) {
      return static_cast<StatementASTNode>(assign_stmt());
    } else if(CurTok.type == TOKEN_TYPE::SC) {
      return static_cast<StatementASTNode>(EmptyStatementASTNode());
    }

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

static ParserFunction<std::vector<std::unique_ptr<FunctionDeclASTNode>>> decl_list =
 []() {
  std::vector<std::unique_ptr<FunctionDeclASTNode>> func_decls;
  //TODO: hello;
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

    return static_cast<DeclASTNode>(FunctionDeclASTNode(name, std::move(params), std::make_unique<BlockAstNode>(block()), type));
  };

static ParserFunction<std::unique_ptr<VariableDeclASTNode>> local_decl =
  [](){
    VariableType type = var_type();
    getNextToken();
    const std::string value = ident(); 
    expect(TOKEN_TYPE::SC);

    return std::make_unique<VariableDeclASTNode>(VariableDeclASTNode(CurTok, value, type));
  };

// program ::= extern_list decl_list
static void parser() {
  // Make sure we are at the beginning of the program
  fseek(pFile, 0, SEEK_SET);

  getNextToken();

  std::vector<std::unique_ptr<ExternFunctionDeclASTNode>> extern_func_decls;

  extern_func_decls = extern_list();

  if(isTypeSpecFirst()) {
    //decl_list();
  }
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

// static LLVMContext TheContext;
// static IRBuilder<> Builder(TheContext);
// static std::unique_ptr<Module> TheModule;

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
    if (pFile == NULL)
      perror("Error opening file");
  } else {
    std::cout << "Usage: ./code InputFile\n";
    return 1;
  }

  // initialize line number and column numbers to zero
  //lineNo = 1;
  //columnNo = 1;

  // get the first token
  getNextToken();
  while (CurTok.type != EOF_TOK) {
    fprintf(stderr, "Token: %s with type %d\n", CurTok.lexeme.c_str(),
            CurTok.type);
    getNextToken();
  }
  fprintf(stderr, "Lexer Finished\n");

  // Make the module, which holds all the code.
  //TheModule = std::make_unique<Module>("mini-c", TheContext);

  // Run the parser now.
  parser();
  fprintf(stderr, "Parsing Finished\n");

  //********************* Start printing final IR **************************
  // Print out all of the generated code into a file called output.ll
  // auto Filename = "output.ll";
  // std::error_code EC;
  // raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

  // if (EC) {
  //   errs() << "Could not open file: " << EC.message();
  //   return 1;
  // }
  // TheModule->print(errs(), nullptr); // print IR to terminal
  //TheModule->print(dest, nullptr);
  //********************* End printing final IR ****************************

  fclose(pFile); // close the file that contains the code that was parsed
  return 0;
}
