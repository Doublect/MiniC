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

class ASTPrintEntry {
  std::string name;
  std::string var;
  std::vector<ASTPrintEntry> children;

  void print(std::string indent, bool last = false) {
    std::cout << indent << (last ? "└─" : "├─") << name << " " << var << std::endl;
  }
};

class ASTPrint {
  std::string name;
  std::vector<std::string> vars;
  std::vector<ASTPrint> children;

public :
  ASTPrint(
    std::string &&name,
    std::vector<std::string> &&vars, 
    std::vector<ASTPrint> &&children = std::vector<ASTPrint>()) {
    this->name = name;
    this->vars = vars;
    this->children = children;
  }

  void printAST(std::string indent, bool last = false) {
    std::cout << indent << (last ? "└──" : "├── ") << name << " " << vars[0] << std::endl;

    indent = indent + (last ? "  " : "│ ");

    //TODO: vars
    for (int i = 0; i < this->children.size() - 1; i++) {
      this->children[i].printAST(indent, false);
    }
    this->children.back().printAST(indent, true);
  }
};

static std::pair<std::string, ASTPrint> make_pair(std::string name, ASTPrint ast) {
  return std::make_pair(name, ast);
};

static std::string indent(int level, int begin) {
  std::string s;
  for (int i = 0; i < level; i++) {
    s += "│ ";
  }
  for (int i = 0; i < begin; i++) {
    s += "└─";
  }

  return s;
}

static std::string indent(std::string &s) {
  s.append("│ ");
}

static std::string deindent(std::string &s) {
  s.erase(s.length() - 2);
}

static std::string indentWrapper(std::string s, std::function<void()> fn) {
  indent(s);
  fn();
  deindent(s);
}

/// ASTNode - Base class for all AST nodes.
class ASTNode
{
public:
  virtual ~ASTNode() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string() const { return "ASTNode"; };


  virtual ASTPrint to_ast_print() {
    return ASTPrint(std::vector<std::string>(), std::vector<std::pair<std::string, ASTPrint>>());
  }

  virtual void print(std::string indent, std::string str = "", bool last = false)
  {

    std::string name = std::string(typeid(*this).name());
    name.erase(std::remove_if(name.begin(), name.end(), [](char c) { return std::isdigit(c); }), name.end());

    std::cout << indent << (last ? "└─" : "├─") << name;
    if(str != "")
      std::cout << " " << str;

    std::cout << std::endl;    
  }

  virtual void print_string(std::string indent, std::string str, bool last = false)
  {
    std::cout << indent << (last ? "└─" : "├─") << str << std::endl;
  }
};

class StatementASTNode : public ASTNode
{
public:
  StatementASTNode() {}
  virtual Value *codegen() { return nullptr; };
  virtual void print(std::string indent) { ASTNode::print(indent, ""); };
};

class ExprASTNode : public StatementASTNode
{
public:
  ExprASTNode() {}
  virtual Value *codegen() { return nullptr; };
  virtual void print(std::string indent) { ASTNode::print(indent, ""); };
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
  virtual ASTPrint to_ast_print() { 
    return ASTPrint(
      std::vector<std::string>({std::to_string(Val)}), std::vector<std::pair<std::string, ASTPrint>>()
    );
  };
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
  virtual ASTPrint to_ast_print() { 
    return ASTPrint(std::vector<std::string>({std::to_string(Val)}), std::vector<std::pair<std::string, ASTPrint>>());
  };
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
  virtual ASTPrint to_ast_print() { 
    return ASTPrint(std::vector<std::string>({std::to_string(Val)}), std::vector<std::pair<std::string, ASTPrint>>());
  };
};

///----------------------------------------------------------------------------
/// Expressions
///----------------------------------------------------------------------------

class UnaryASTNode : public ExprASTNode
{
  TOKEN_TYPE Op;
  std::unique_ptr<ExprASTNode> Operand;

  TOKEN Tok;

public:
  UnaryASTNode(TOKEN tok, TOKEN_TYPE op, std::unique_ptr<ExprASTNode> operand)
      : Op(op), Operand(std::move(operand)), Tok(tok) {}
  virtual Value *codegen() { return nullptr; };

  virtual ASTPrint to_ast_print() { 
    return ASTPrint(std::vector<std::string>({"Op: " + Tok.lexeme}), std::vector<std::pair<std::string, ASTPrint>>({make_pair("", Operand->to_ast_print())}));
  };
};

/// AST representation of a binary expression
class BinaryASTNode : public ExprASTNode
{
  TOKEN_TYPE Op;
  std::unique_ptr<ExprASTNode> LHS, RHS;

  TOKEN Tok;
  std::string Name;

public:
  BinaryASTNode(TOKEN tok, TOKEN_TYPE op, std::unique_ptr<ExprASTNode> LHS,
                std::unique_ptr<ExprASTNode> RHS)
      : Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
  virtual Value *codegen() { return nullptr; };

  virtual ASTPrint to_ast_print() { 
    return ASTPrint(std::vector<std::string>({"Op: " + Tok.lexeme}), std::vector<std::pair<std::string, ASTPrint>>({make_pair("LHS: ", LHS->to_ast_print()), make_pair("RHS:", RHS->to_ast_print())}));
  };
};

/// AST representation of a variable reference
class VariableRefASTNode : public ExprASTNode
{
  std::string Name;
  TOKEN Tok;

public:
  VariableRefASTNode(TOKEN tok, const std::string &Name) : Name(Name) {}
  virtual Value *codegen() { return nullptr; };
  virtual void print(int depth)
  {
    ASTNode::print(depth, Name);
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
  virtual void print(int depth)
  {
    ASTNode::print(depth, "FunctionName: " + FunctionName);
    ASTNode::print_string(depth + 1, "Args: ");
    for (auto &arg : Args)
    {
      arg->print(depth + 1);
    }
  }
};

class AssignmentASTNode : public ExprASTNode
{
  std::string Name;
  std::unique_ptr<ExprASTNode> RHS;
  TOKEN Tok;

public:
  AssignmentASTNode(TOKEN tok, const std::string &Name,
                    std::unique_ptr<ExprASTNode> RHS)
      : Name(Name), RHS(std::move(RHS)) {}
  virtual Value *codegen() { return nullptr; };
  virtual void print(int depth)
  {
    ASTNode::print(depth, "Name: " + Name);
    ASTNode::print_string(depth + 1, "RHS: ");    
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
  virtual void print(int depth)
  {
    ASTNode::print(depth);
    Expr->print(depth + 1);
  }
};

class VariableDeclASTNode;

class BlockASTNode : public StatementASTNode
{
  std::vector<std::unique_ptr<VariableDeclASTNode>> Declarations;
  std::vector<std::unique_ptr<StatementASTNode>> Statements;

public:
  BlockASTNode(std::vector<std::unique_ptr<StatementASTNode>> statements) : Statements(std::move(statements)) {}
  virtual Value *codegen() { return nullptr; };
  virtual void print(int depth);
  // virtual void print(int depth)
  // {
  //   ASTNode::print(depth);
  //   ASTNode::print_string(depth, "Declarations: ");
  //   for (auto &decl : Declarations)
  //   {
  //     decl->print(depth + 1);
  //   }
  //   ASTNode::print_string(depth, "Statements: ");
  //   for (auto &stmt : Statements)
  //   {
  //     stmt->print(depth + 1);
  //   }
  // }
};

class IfElseASTNode : public StatementASTNode
{
  std::unique_ptr<ExprASTNode> Cond;
  std::unique_ptr<BlockASTNode> Then;
  std::unique_ptr<BlockASTNode> Else;

public:
  IfElseASTNode(std::unique_ptr<ExprASTNode> Cond, std::unique_ptr<BlockASTNode> Then,
                std::unique_ptr<BlockASTNode> Else)
      : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
  virtual Value *codegen() { return nullptr; };
  virtual void print(int depth)
  {
    ASTNode::print(depth);
    ASTNode::print_string(depth, "Cond: ");
    Cond->print(depth + 1);
    ASTNode::print_string(depth, "Then: ");
    Then->print(depth + 1);
    ASTNode::print_string(depth, "Else: ");
    Else->print(depth + 1);
  }
};

class WhileASTNode : public StatementASTNode
{
  std::unique_ptr<ExprASTNode> Cond;
  std::unique_ptr<BlockASTNode> Body;

public:
  WhileASTNode(std::unique_ptr<ExprASTNode> Cond, std::unique_ptr<BlockASTNode> Body)
      : Cond(std::move(Cond)), Body(std::move(Body)) {}
  virtual Value *codegen() { return nullptr; };
  virtual void print(int depth)
  {
    ASTNode::print(depth);
    ASTNode::print_string(depth, "Cond: ");
    Cond->print(depth + 1);
    ASTNode::print_string(depth, "Body: ");
    Body->print(depth + 1);
  }
};

class ReturnStmtASTNode : public StatementASTNode
{
  std::unique_ptr<ExprASTNode> Expr;

public:
  ReturnStmtASTNode(std::unique_ptr<ExprASTNode> expr) : Expr(std::move(expr)) {}
  virtual Value *codegen() { return nullptr; };
  virtual void print(int depth)
  {
    ASTNode::print(depth);
    Expr->print(depth + 1);
  }
};

class AssignmentStmtASTNode : public StatementASTNode
{
  std::string Name;
  std::unique_ptr<ExprASTNode> RHS;
  TOKEN Tok;

public:
  AssignmentStmtASTNode(TOKEN tok, const std::string &Name,
                        std::unique_ptr<ExprASTNode> RHS)
      : Name(Name), RHS(std::move(RHS)) {}
  virtual Value *codegen() { return nullptr; };
  virtual void print(int depth)
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
  virtual void print(int depth)
  {
    ASTNode::print(depth);
  }
};

class VariableDeclASTNode : public DeclASTNode
{
  std::string Name;
  TOKEN Tok;
  VariableType Type;

public:
  VariableDeclASTNode(TOKEN tok, const std::string &Name, VariableType type) : Name(std::move(Name)), Type(type) {}
  virtual Value *codegen() { return nullptr; };
  virtual void print(int depth)
  {
    ASTNode::print(depth, "Name: " + Name);
    //std::cout << "Type: " << Type. << std::endl;
  }
};

void BlockASTNode::print(int depth)
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
  std::unique_ptr<BlockASTNode> Body;
  TypeSpecType ReturnType;

public:
  FunctionDeclASTNode(std::string Name,
                      std::vector<std::unique_ptr<FunctionParameterASTNode>> Args,
                      std::unique_ptr<BlockASTNode> Body, TypeSpecType returnType)
      : Name(Name), Args(std::move(Args)), Body(std::move(Body)), ReturnType(returnType) {}
  virtual Value *codegen() { return nullptr; };
  virtual void print(int depth)
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
  virtual void print(int depth)
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
  virtual void print(int depth)
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