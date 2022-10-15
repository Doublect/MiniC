#include "llvm/IR/Value.h"

#include <iostream>
#include <string>
#include <memory>
#include <vector>

#include "ast.hpp"
#include "lexer.hpp"

using namespace llvm;

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//
#pragma region AST
/// ASTNode - Base class for all AST nodes.
class ASTNode
{
public:
  virtual ~ASTNode() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string() const { return "ASTNode"; };

  void print(int depth, std::string str = "")
  {

    std::string name = std::string(typeid(*this).name());
    name.erase(std::remove_if(name.begin(), name.end(), [](char c) { return std::isdigit(c); }), name.end());

    std::cout << std::string(depth * 2, ' ') << name << std::endl;
    if(str != "")
      std::cout << " " << str;

    std::cout << std::endl;    
  }

  void print_string(int depth, std::string str)
  {
    std::cout << std::string(depth, ' ') << str << std::endl;
  }
};

class StatementASTNode : public ASTNode
{
public:
  StatementASTNode() {}
  virtual Value *codegen() { return nullptr; };
};

class ExprASTNode : public StatementASTNode
{
public:
  ExprASTNode() {}
  virtual Value *codegen() { return nullptr; };
};

/// AST representation of an integer number
class IntASTNode : public ExprASTNode
{
  int Val;
  TOKEN Tok;
  std::string Name;

public:
  IntASTNode(TOKEN tok, int val) : Val(val), Tok(tok) {}
  virtual Value *codegen() { return nullptr; };
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

/// AST representation of a boolean value
class BoolASTNode : public ExprASTNode
{
  bool Val;
  TOKEN Tok;
  std::string Name;

public:
  BoolASTNode(TOKEN tok, bool val) : Val(val), Tok(tok) {}
  virtual Value *codegen() { return nullptr; };
};

/// AST representation of a float literal
class FloatASTNode : public ExprASTNode
{
  float Val;
  TOKEN Tok;
  std::string Name;

public:
  FloatASTNode(TOKEN tok, float val) : Val(val), Tok(tok) {}
  virtual Value *codegen() { return nullptr; };
};

///----------------------------------------------------------------------------
/// Expressions
///----------------------------------------------------------------------------

class UnaryASTNode : public ExprASTNode
{
  TOKEN_TYPE Op;
  std::unique_ptr<ASTNode> Operand;

  TOKEN Tok;

public:
  UnaryASTNode(TOKEN tok, TOKEN_TYPE op, std::unique_ptr<ASTNode> operand)
      : Op(op), Operand(std::move(operand)), Tok(tok) {}
  virtual Value *codegen() { return nullptr; };

  void print(int depth)
  {
    ASTNode::print(depth);
    std::cout << "Op: " << Tok.lexeme << std::endl;
    Operand->print(depth + 1);
  }
};

/// AST representation of a binary expression
class BinaryASTNode : public ExprASTNode
{
  TOKEN_TYPE Op;
  std::unique_ptr<ASTNode> LHS, RHS;

  TOKEN Tok;
  std::string Name;

public:
  BinaryASTNode(TOKEN tok, TOKEN_TYPE op, std::unique_ptr<ASTNode> LHS,
                std::unique_ptr<ASTNode> RHS)
      : Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
  virtual Value *codegen() { return nullptr; };
  void print(int depth)
  {
    ASTNode::print(depth);
    std::cout << "Op: " << Tok.lexeme << std::endl;
    std::cout << "LHS: " << std::endl;
    LHS->print(depth + 1);
    std::cout << "RHS: " << std::endl;
    RHS->print(depth + 1);
  }
};

/// AST representation of a variable reference
class VariableRefASTNode : public ExprASTNode
{
  std::string Name;
  TOKEN Tok;

public:
  VariableRefASTNode(TOKEN tok, const std::string &Name) : Name(Name) {}
  virtual Value *codegen() { return nullptr; };
  void print(int depth)
  {
    ASTNode::print(depth);
    std::cout << Name << std::endl;
  }
};

// AST representation of a function call
class CallExprAST : public ExprASTNode
{
  std::string FunctionName;
  std::vector<std::unique_ptr<ExprASTNode>> Args;

  TOKEN tok;

public:
  CallExprAST(TOKEN tok, const std::string &funcName,
              std::vector<std::unique_ptr<ExprASTNode>> Args)
      : tok(tok), FunctionName(funcName), Args(std::move(Args)) {}
  virtual Value *codegen() { return nullptr; };
  void print(int depth)
  {
    ASTNode::print(depth);
    std::cout << "FunctionName: " << FunctionName << std::endl;
    std::cout << "Args: " << std::endl;
    for (auto &arg : Args)
    {
      arg->print(depth + 1);
    }
  }
};

class AssignmentASTNode : public ExprASTNode
{
  std::string Name;
  std::unique_ptr<ASTNode> RHS;
  TOKEN Tok;

public:
  AssignmentASTNode(TOKEN tok, const std::string &Name,
                    std::unique_ptr<ASTNode> RHS)
      : Name(Name), RHS(std::move(RHS)) {}
  virtual Value *codegen() { return nullptr; };
  void print(int depth)
  {
    ASTNode::print(depth);
    std::cout << "Name: " << Name << std::endl;
    std::cout << "RHS: " << std::endl;
    RHS->print(depth + 1);
  }
};

///----------------------------------------------------------------------------
/// Statements
///----------------------------------------------------------------------------
#pragma region Statements
class ExprStatementASTNode : public StatementASTNode
{
  std::unique_ptr<ExprASTNode> Expr;

public:
  ExprStatementASTNode(std::unique_ptr<ExprASTNode> expr) : Expr(std::move(expr)) {}
  virtual Value *codegen() { return nullptr; };
  void print(int depth)
  {
    ASTNode::print(depth);
    Expr->print(depth + 1);
  }
};

class VariableDeclASTNode;

class BlockAstNode : public StatementASTNode
{
  std::vector<std::unique_ptr<VariableDeclASTNode>> Declarations;
  std::vector<std::unique_ptr<StatementASTNode>> Statements;

public:
  BlockAstNode(std::vector<std::unique_ptr<StatementASTNode>> statements) : Statements(std::move(statements)) {}
  virtual Value *codegen() { return nullptr; };
  void print(int depth);
};

class IfElseASTNode : public StatementASTNode
{
  std::unique_ptr<ExprASTNode> Cond;
  std::unique_ptr<BlockAstNode> Then;
  std::unique_ptr<BlockAstNode> Else;

public:
  IfElseASTNode(std::unique_ptr<ExprASTNode> Cond, std::unique_ptr<BlockAstNode> Then,
                std::unique_ptr<BlockAstNode> Else)
      : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
  virtual Value *codegen() { return nullptr; };
  void print(int depth)
  {
    ASTNode::print(depth);
    std::cout << "Cond: " << std::endl;
    Cond->print(depth + 1);
    std::cout << "Then: " << std::endl;
    Then->print(depth + 1);
    std::cout << "Else: " << std::endl;
    Else->print(depth + 1);
  }
};

class WhileASTNode : public StatementASTNode
{
  std::unique_ptr<ExprASTNode> Cond;
  std::unique_ptr<ASTNode> Body;

public:
  WhileASTNode(std::unique_ptr<ExprASTNode> Cond, std::unique_ptr<BlockAstNode> Body)
      : Cond(std::move(Cond)), Body(std::move(Body)) {}
  virtual Value *codegen() { return nullptr; };
  void print(int depth)
  {
    ASTNode::print(depth);
    std::cout << "Cond: " << std::endl;
    Cond->print(depth + 1);
    std::cout << "Body: " << std::endl;
    Body->print(depth + 1);
  }
};

class ReturnStmtASTNode : public StatementASTNode
{
  std::unique_ptr<ExprASTNode> Expr;

public:
  ReturnStmtASTNode(std::unique_ptr<ExprASTNode> expr) : Expr(std::move(expr)) {}
  virtual Value *codegen() { return nullptr; };
  void print(int depth)
  {
    ASTNode::print(depth);
    Expr->print(depth + 1);
  }
};

class AssignmentStmtASTNode : public StatementASTNode
{
  std::string Name;
  std::unique_ptr<ASTNode> RHS;
  TOKEN Tok;

public:
  AssignmentStmtASTNode(TOKEN tok, const std::string &Name,
                        std::unique_ptr<ASTNode> RHS)
      : Name(Name), RHS(std::move(RHS)) {}
  virtual Value *codegen() { return nullptr; };
  void print(int depth)
  {
    ASTNode::print(depth, "Name: " + Name);
    ASTNode::print_string(depth, "RHS: ");
    RHS->print(depth + 1);
  }
};
class EmptyStatementASTNode : public StatementASTNode
{

public:
  EmptyStatementASTNode() {}
  virtual Value *codegen() { return nullptr; };
};
#pragma endregion
///----------------------------------------------------------------------------
/// Declarations
///----------------------------------------------------------------------------
#pragma region Declarations
class DeclASTNode : public ASTNode
{

public:
  DeclASTNode() {}
  virtual Value *codegen() { return nullptr; };
};

class VariableDeclASTNode : public DeclASTNode
{
  std::string Name;
  TOKEN Tok;
  VariableType Type;

public:
  VariableDeclASTNode(TOKEN tok, const std::string &Name, VariableType type) : Name(std::move(Name)), Type(type) {}
  virtual Value *codegen() { return nullptr; };
  void print(int depth)
  {
    ASTNode::print(depth, "Name: " + Name);
    //std::cout << "Type: " << Type. << std::endl;
  }
};

void BlockAstNode::print(int depth)
  {
    ASTNode::print(depth);
    ASTNode::print_string(depth, "Declarations: ");
    for (auto &decl : Declarations)
    {
      decl->print(depth + 1);
    }
    ASTNode::print_string(depth, "Statements: ");
    for (auto &stmt : Statements)
    {
      stmt->print(depth + 1);
    }
  }

class FunctionParameterASTNode : public VariableDeclASTNode
{
  // Inherit constructor
  using VariableDeclASTNode::VariableDeclASTNode;

public:
  virtual Value *codegen() { return nullptr; };
};

class FunctionDeclASTNode : public DeclASTNode
{
  std::string Name;
  std::vector<std::unique_ptr<FunctionParameterASTNode>> Args;
  std::unique_ptr<BlockAstNode> Body;
  TypeSpecType ReturnType;

public:
  FunctionDeclASTNode(std::string Name,
                      std::vector<std::unique_ptr<FunctionParameterASTNode>> Args,
                      std::unique_ptr<BlockAstNode> Body, TypeSpecType returnType)
      : Name(Name), Args(std::move(Args)), Body(std::move(Body)), ReturnType(returnType) {}
  virtual Value *codegen() { return nullptr; };
  void print(int depth)
  {
    ASTNode::print(depth, "Name: " + Name);
    ASTNode::print_string(depth, "Args: ");
    for (auto &arg : Args)
    {
      arg->print(depth + 1);
    }
    ASTNode::print_string(depth, "Body: ");
    Body->print(depth + 1);
    ASTNode::print_string(depth, "ReturnType: " + std::to_string(ReturnType));
  }
};

class ExternFunctionDeclASTNode : public ASTNode
{
  std::string Name;
  std::vector<std::unique_ptr<FunctionParameterASTNode>> Args;
  TypeSpecType ReturnType;

public:
  ExternFunctionDeclASTNode(std::string Name,
                            std::vector<std::unique_ptr<FunctionParameterASTNode>> Args,
                            TypeSpecType returnType)
      : Name(Name), Args(std::move(Args)), ReturnType(returnType) {}
  virtual Value *codegen() { return nullptr; };
  void print(int depth)
  {
    ASTNode::print(depth, "Name: " + Name);
    ASTNode::print_string(depth, "Args: ");
    for (auto &arg : Args)
    {
      arg->print(depth + 1);
    }
    ASTNode::print_string(depth, "ReturnType: " + std::to_string(ReturnType));
  }
};

class ProgramASTNode : public ASTNode
{
  std::vector<std::unique_ptr<ExternFunctionDeclASTNode>> ExternDeclarations;
  std::vector<std::unique_ptr<DeclASTNode>> Declarations;

public:
  ProgramASTNode(std::vector<std::unique_ptr<ExternFunctionDeclASTNode>> ExternDeclarations,
                 std::vector<std::unique_ptr<DeclASTNode>> Declarations)
      : ExternDeclarations(std::move(ExternDeclarations)), Declarations(std::move(Declarations)) {}
  virtual Value *codegen() { return nullptr; };
  void print(int depth)
  {
    ASTNode::print(depth);
    ASTNode::print_string(depth, "ExternDeclarations: ");
    for (auto &decl : ExternDeclarations)
    {
      decl->print(depth + 1);
    }
    ASTNode::print_string(depth, "Declarations: ");
    for (auto &func : Declarations)
    {
      func->print(depth + 1);
    }
  }
};
#pragma endregion
#pragma endregion