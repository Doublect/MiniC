#include "llvm/IR/Value.h"
#include<string>
#include<memory>
#include<vector>

#include "ast.hpp"
#include "lexer.hpp"


using namespace llvm;

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