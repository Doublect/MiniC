#ifndef AST_H
#define AST_H
    

#include "llvm/IR/Value.h"

#include <iostream>
#include <string>
#include <vector>

#include "ast_print.hpp"
#include "helpers.hpp"
#include "lexer.hpp"

using namespace llvm;

enum class VariableType {
    INT = -2,
    FLOAT = -4,
    BOOL = -5,
};

enum TypeSpecType {
    INT = -2,
    VOID = -3,
    FLOAT = -4,
    BOOL = -5,
};

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//
#pragma region AST

// /// ASTNode - Base class for all AST nodes.
// class ASTNode
// {
// public:
//   virtual ~ASTNode();
//   virtual auto codegen();
//   virtual std::string to_string();

//   virtual std::unique_ptr<ASTPrint> to_ast_print();

//   virtual void print(std::string indent, std::string str = "", bool last = false);

//   virtual void print_string(std::string indent, std::string str, bool last = false);
// };

// template <typename T>
// static std::vector<std::unique_ptr<ASTPrint>> map_printer(std::vector<std::unique_ptr<T>> &nodes);

// class StatementASTNode : public ASTNode
// {
// public:
//     StatementASTNode();
//     virtual auto codegen();
//     virtual void print(std::string indent);
//     };

// class ExprASTNode : public StatementASTNode
// {
// public:
//     ExprASTNode();
//     virtual auto codegen();
//     virtual void print(std::string indent);
// };

// /// AST representation of an integer number
// class IntASTNode : public ExprASTNode
// {
//     int Val;
//     TOKEN Tok;
//     std::string Name;

// public:
//     IntASTNode(TOKEN tok, int val);
//     virtual auto codegen();
//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

//     /// AST representation of a boolean value
// class BoolASTNode : public ExprASTNode
// {
//     bool Val;
//     TOKEN Tok;
//     std::string Name;

// public:
//     BoolASTNode(TOKEN tok, bool val);
//     virtual auto codegen();
//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// /// AST representation of a float literal
// class FloatASTNode : public ExprASTNode
// {
//     float Val;
//     TOKEN Tok;
//     std::string Name;

//     public:
//     FloatASTNode(TOKEN tok, float val);
//     virtual auto codegen();
//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// ///----------------------------------------------------------------------------
// /// Expressions
// ///----------------------------------------------------------------------------

// class UnaryASTNode : public ExprASTNode
// {
//     TOKEN_TYPE Op;
//     std::unique_ptr<ExprASTNode> Operand;

//     TOKEN Tok;

// public:
//     UnaryASTNode(TOKEN tok, TOKEN_TYPE op, std::unique_ptr<ExprASTNode> operand);
//     virtual auto codegen();

//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// /// AST representation of a binary expression
// class BinaryASTNode : public ExprASTNode
// {
//     TOKEN_TYPE Op;
//     std::unique_ptr<ExprASTNode> LHS, RHS;

//     TOKEN Tok;
//     std::string Name;

// public:
//     BinaryASTNode(TOKEN tok, TOKEN_TYPE op, std::unique_ptr<ExprASTNode> LHS,
//                     std::unique_ptr<ExprASTNode> RHS);
//     virtual auto codegen();

//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// /// AST representation of a variable reference
// class VariableRefASTNode : public ExprASTNode
// {
//     std::string Name;
//     TOKEN Tok;

// public:
//     VariableRefASTNode(TOKEN tok, const std::string &Name);
//     virtual auto codegen();
//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// // AST representation of a function call
// class CallExprAST : public ExprASTNode
// {
//     std::string FunctionName;
//     std::vector<std::unique_ptr<ExprASTNode>> Args;

//     TOKEN tok;

// public:
//     CallExprAST(TOKEN tok, const std::string &funcName,
//                 std::vector<std::unique_ptr<ExprASTNode>> Args);
//     virtual auto codegen();

//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class AssignmentASTNode : public ExprASTNode
// {
//     std::string Name;
//     std::unique_ptr<ExprASTNode> RHS;
//     TOKEN Tok;

// public:
//     AssignmentASTNode(TOKEN tok, const std::string &Name,
//                         std::unique_ptr<ExprASTNode> RHS);
//     virtual auto codegen();
//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// ///----------------------------------------------------------------------------
// /// Statements
// ///----------------------------------------------------------------------------
// #pragma region Statements
// class ExprStatementASTNode : public StatementASTNode
// {
//     std::unique_ptr<ExprASTNode> Expr;

// public:
//     ExprStatementASTNode(std::unique_ptr<ExprASTNode> expr);
//     virtual auto codegen();

//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class VariableDeclASTNode;

// class BlockASTNode : public StatementASTNode
// {
//     std::vector<std::unique_ptr<VariableDeclASTNode>> Declarations;
//     std::vector<std::unique_ptr<StatementASTNode>> Statements;

// public:
//     BlockASTNode(std::vector<std::unique_ptr<StatementASTNode>> statements);
//     virtual auto codegen();
//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class IfElseASTNode : public StatementASTNode
// {
//     std::unique_ptr<ExprASTNode> Cond;
//     std::unique_ptr<BlockASTNode> Then;
//     std::unique_ptr<BlockASTNode> Else;

// public:
//     IfElseASTNode(std::unique_ptr<ExprASTNode> Cond, std::unique_ptr<BlockASTNode> Then,
//                     std::unique_ptr<BlockASTNode> Else);
//     virtual auto codegen();
//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class WhileASTNode : public StatementASTNode
// {
//     std::unique_ptr<ExprASTNode> Cond;
//     std::unique_ptr<BlockASTNode> Body;

// public:
//     WhileASTNode(std::unique_ptr<ExprASTNode> Cond, std::unique_ptr<BlockASTNode> Body);
//     virtual auto codegen();

//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class ReturnStmtASTNode : public StatementASTNode
// {
//     std::unique_ptr<ExprASTNode> Expr;

// public:
//     ReturnStmtASTNode(std::unique_ptr<ExprASTNode> expr);
//     virtual auto codegen();

//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class AssignmentStmtASTNode : public StatementASTNode
// {
//     std::string Name;
//     std::unique_ptr<ExprASTNode> RHS;
//     TOKEN Tok;

// public:
//     AssignmentStmtASTNode(TOKEN tok, const std::string &Name,
//                             std::unique_ptr<ExprASTNode> RHS);
//     virtual auto codegen();
//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class EmptyStatementASTNode : public StatementASTNode
// {

// public:
//     EmptyStatementASTNode();
//     virtual auto codegen();
//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };
// #pragma endregion
// ///----------------------------------------------------------------------------
// /// Declarations
// ///----------------------------------------------------------------------------
// #pragma region Declarations
// class DeclASTNode : public ASTNode
// {

// public:
//     DeclASTNode();
//     virtual auto codegen();
//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class VariableDeclASTNode : public DeclASTNode
// {
//     std::string Name;
//     TOKEN Tok;
//     VariableType Type;

// public:
//     VariableDeclASTNode(TOKEN tok, const std::string &Name, VariableType type);
//     virtual auto codegen();

//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class FunctionParameterASTNode : public VariableDeclASTNode
// {
//     // Inherit constructor
//     using VariableDeclASTNode::VariableDeclASTNode;

// public:
//     virtual auto codegen();
// };

// class FunctionDeclASTNode : public DeclASTNode
// {
//     std::string Name;
//     std::vector<std::unique_ptr<FunctionParameterASTNode>> Args;
//     std::unique_ptr<BlockASTNode> Body;
//     TypeSpecType ReturnType;

// public:
//     FunctionDeclASTNode(std::string Name,
//                         std::vector<std::unique_ptr<FunctionParameterASTNode>> Args,
//                         std::unique_ptr<BlockASTNode> Body, TypeSpecType returnType);
//     virtual auto codegen();

//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class ExternFunctionDeclASTNode : public ASTNode
// {
//     std::string Name;
//     std::vector<std::unique_ptr<FunctionParameterASTNode>> Args;
//     TypeSpecType ReturnType;

// public:
//     ExternFunctionDeclASTNode(std::string Name,
//                                 std::vector<std::unique_ptr<FunctionParameterASTNode>> Args,
//                                 TypeSpecType returnType);
//     virtual auto codegen();

//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class ProgramASTNode : public ASTNode
// {
//     std::vector<std::unique_ptr<ExternFunctionDeclASTNode>> ExternDeclarations;
//     std::vector<std::unique_ptr<DeclASTNode>> Declarations;

// public:
//     ProgramASTNode(std::vector<std::unique_ptr<ExternFunctionDeclASTNode>> ExternDeclarations,
//                     std::vector<std::unique_ptr<DeclASTNode>> Declarations)
//         : ExternDeclarations(std::move(ExternDeclarations)), Declarations(std::move(Declarations)) {}
//     virtual auto codegen();

//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };
// #pragma endregion
#pragma endregion

#pragma endregion
#pragma endregion

class ASTNode;
class StatementASTNode;
class IntASTNode;
class BoolASTNode;
class FloatASTNode;

class ExprASTNode;
// Exprs
class UnaryASTNode;
class BinaryASTNode;
class VariableRefASTNode;
class CallExprAST;
class AssignmentASTNode;

// Stmts
class ExprStatementASTNode;
class BlockASTNode;
class IfElseASTNode;
class WhileASTNode;
class ReturnStmtASTNode;
class AssignmentStmtASTNode;
class EmptyStatementASTNode;

// Decls

class DeclASTNode;
class VariableDeclASTNode;
class FunctionParameterASTNode;
class FunctionDeclASTNode;
class ExternFunctionDeclASTNode;
class ProgramASTNode;

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//
#pragma region AST

template <typename T>
static std::vector<std::unique_ptr<ASTPrint>> map_printer(std::vector<std::unique_ptr<T>> &nodes)
{
  std::vector<std::unique_ptr<ASTPrint>> asts;
  for (auto &node : nodes)
  {
    asts.push_back(std::move(node->to_ast_print()));
  }
  return asts;
}

/// ASTNode - Base class for all AST nodes.
class ASTNode
{
public:
  virtual ~ASTNode() {}
  auto codegen() {return nullptr; };
  virtual std::string to_string() const { return "ASTNode"; };

  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    return make_ast_print("", "", std::vector<std::unique_ptr<ASTPrint>>());
  }

  virtual void print(std::string indent, std::string str = "", bool last = false)
  {

    std::string name = std::string(typeid(*this).name());
    name.erase(std::remove_if(name.begin(), name.end(), [](char c)
                              { return std::isdigit(c); }),
               name.end());

    std::cout << indent << (last ? "└─" : "├─") << name;
    if (str != "")
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
  auto codegen() { return nullptr; };
  virtual void print(std::string indent) { ASTNode::print(indent, ""); };
};

class ExprASTNode : public StatementASTNode
{
public:
  ExprASTNode() {}
  auto codegen() { return nullptr; };
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
  auto codegen();
  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    return make_ast_print(
        "IntASTNode",
        std::to_string(Val),
        std::vector<std::unique_ptr<ASTPrint>>());
  };
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

/// AST representation of a float literal
class FloatASTNode : public ExprASTNode
{
  float Val;
  TOKEN Tok;
  std::string Name;

public:
  FloatASTNode(TOKEN tok, float val) : Val(val), Tok(tok) {}
  auto codegen();
  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    return make_ast_print(
        "FloatASTNode",
        std::to_string(Val),
        std::vector<std::unique_ptr<ASTPrint>>());
  };
};

/// AST representation of a boolean value
class BoolASTNode : public ExprASTNode
{
  bool Val;
  TOKEN Tok;
  std::string Name;

public:
  BoolASTNode(TOKEN tok, bool val) : Val(val), Tok(tok) {}
  auto codegen();
  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    return make_ast_print(
        "BoolASTNode",
        std::to_string(Val),
        std::vector<std::unique_ptr<ASTPrint>>());
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
  auto codegen();

  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    auto children = std::vector<std::unique_ptr<ASTPrint>>();

    children.push_back(Operand->to_ast_print());
    return make_ast_print( 
        "UnaryASTNode",
        "Op: " + Tok.lexeme,
        std::move(children)
    );
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
  auto codegen();

  std::unique_ptr<ASTPrint> to_ast_print()
  {
    auto children = std::vector<std::unique_ptr<ASTPrint>>();

    children.push_back(LHS->to_ast_print());
    children.push_back(RHS->to_ast_print());

    return make_ast_print(
        "BinaryASTNode",
        "Op: " + Tok.lexeme,
        std::move(children));
  };
};

/// AST representation of a variable reference
class VariableRefASTNode : public ExprASTNode
{
  std::string Name;
  TOKEN Tok;

public:
  VariableRefASTNode(TOKEN tok, const std::string &Name) : Name(Name) {}
  auto codegen();
  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    return make_ast_print(
        "VariableRefASTNode",
        std::string(Name),
        std::vector<std::unique_ptr<ASTPrint>>()
      );
  };
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
  auto codegen();

  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    auto children = std::vector<std::unique_ptr<ASTPrint>>();

    children.push_back(make_ast_print("Args:", "", map_printer(Args)));

    return make_ast_print(
        "CallExprAST",
        "FunctionName: " + FunctionName,
        std::move(children)
      );
  };
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
  auto codegen() { return nullptr; };
  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    auto children = std::vector<std::unique_ptr<ASTPrint>>();

    children.push_back(RHS->to_ast_print());

    return make_ast_print(
        "AssignmentASTNode",
        "Name: " + Name,
        std::move(children)
    );
  };
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
  auto codegen() { return nullptr; };

  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    auto children = std::vector<std::unique_ptr<ASTPrint>>();

    children.push_back(Expr->to_ast_print());

    return make_ast_print(
        "ExprStatementASTNode",
        "",
        std::move(children)
    );
  }
};

class VariableDeclASTNode;

class BlockASTNode : public StatementASTNode
{
  std::vector<std::unique_ptr<DeclASTNode>> Declarations;
  std::vector<std::unique_ptr<StatementASTNode>> Statements;

public:
  BlockASTNode(std::vector<std::unique_ptr<DeclASTNode>> &&declarations,
    std::vector<std::unique_ptr<StatementASTNode>> &&statements) 
    : Declarations(std::move(declarations)), Statements(std::move(statements)) {}
  auto codegen() { return nullptr; };
  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    auto children = std::vector<std::unique_ptr<ASTPrint>>();

    children.push_back(make_ast_children("Declarations:", map_printer(Declarations)));
    children.push_back(make_ast_children("Statements:", map_printer(Statements)));

    return make_ast_print(
        "BlockASTNode",
        "",
        std::move(children)
      );
  }
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
  auto codegen() { return nullptr; };
  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    auto children = std::vector<std::unique_ptr<ASTPrint>>();

    children.push_back(make_ast_labelled("Cond:", Cond->to_ast_print()));
    children.push_back(make_ast_labelled("Then:", Then->to_ast_print()));
    // TODO: Check if else has body
    children.push_back(make_ast_labelled("Else:", Else->to_ast_print()));

    return make_ast_print(
        "IfElseASTNode",
        "",
        // std::vector<std::unique_ptr<ASTPrint>>{
        //   make_ast_labelled("Cond:", Cond->to_ast_print()),
        //   make_ast_labelled("Then:", Then->to_ast_print()), 
        //   make_ast_labelled("Else:", Else->to_ast_print())
        // }
        std::move(children)
      );
  }
};

class WhileASTNode : public StatementASTNode
{
  std::unique_ptr<ExprASTNode> Cond;
  std::unique_ptr<BlockASTNode> Body;

public:
  WhileASTNode(std::unique_ptr<ExprASTNode> Cond, std::unique_ptr<BlockASTNode> Body)
      : Cond(std::move(Cond)), Body(std::move(Body)) {}
  auto codegen() { return nullptr; };

  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    auto children = std::vector<std::unique_ptr<ASTPrint>>();

    children.push_back(make_ast_labelled("Cond:", Cond->to_ast_print()));
    children.push_back(make_ast_labelled("Body:", Body->to_ast_print()));

    return make_ast_print(
        "WhileASTNode",
        "",
        std::move(children)
        );
  }
};

class ReturnStmtASTNode : public StatementASTNode
{
  std::unique_ptr<ExprASTNode> Expr;

public:
  ReturnStmtASTNode(std::unique_ptr<ExprASTNode> expr) : Expr(std::move(expr)) {}
  auto codegen() { return nullptr; };

  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    auto children = std::vector<std::unique_ptr<ASTPrint>>();

    children.push_back(make_ast_labelled("Expr:", Expr->to_ast_print()));

    return make_ast_print(
        "WhileASTNode",
        "",
        std::move(children)
      );
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
  auto codegen() { return nullptr; };
  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    auto children = std::vector<std::unique_ptr<ASTPrint>>();

    children.push_back(RHS->to_ast_print());

    return make_ast_print(
        "AssignmentStmt",
        "Name: " + Name,
        std::move(children)
        );
  }
};
class EmptyStatementASTNode : public StatementASTNode
{

public:
  EmptyStatementASTNode() {}
  auto codegen() { return nullptr; };
  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    return make_ast_print(
        "EmptyStatementASTNode",
        "",
        std::vector<std::unique_ptr<ASTPrint>>()
        );
  }
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
  auto codegen() { return nullptr; };
  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    return make_ast_print(
        "",
        "",
        std::vector<std::unique_ptr<ASTPrint>>()
        );
  }
};

class VariableDeclASTNode : public DeclASTNode
{
  std::string Name;
  TOKEN Tok;
  VariableType Type;

public:
  VariableDeclASTNode(TOKEN tok, const std::string &Name, VariableType type) : Name(std::move(Name)), Type(type) {}
  auto codegen() { return nullptr; };

  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    return make_ast_print(
        "VariableDeclASTNode",
        "Name: " + Name,
        std::vector<std::unique_ptr<ASTPrint>>(
            // ASTPrintLeaf("Type: " + std::to_string(Type)),
        ));
  }
};

class FunctionParameterASTNode : public VariableDeclASTNode
{
  // Inherit constructor
  using VariableDeclASTNode::VariableDeclASTNode;

public:
  auto codegen() { return nullptr; };
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
  auto codegen() { return nullptr; };

  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    auto children = std::vector<std::unique_ptr<ASTPrint>>();

    children.push_back(make_ast_children("Args:", map_printer(Args)));
    children.push_back(make_ast_labelled("Body:", Body->to_ast_print()));
    children.push_back(make_ast_leaf("ReturnType: " + std::to_string(ReturnType)));

    return make_ast_print(
        "FunctionDeclASTNode",
        "Name: " + Name,
        std::move(children)
      );
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
  auto codegen() { return nullptr; };

  virtual std::unique_ptr<ASTPrint> to_ast_print()
  {
    auto children = std::vector<std::unique_ptr<ASTPrint>>();

    children.push_back(make_ast_children("Args:", map_printer(Args)));
    children.push_back(make_ast_leaf("ReturnType: " + std::to_string(ReturnType)));

    return make_ast_print(
        "ExternFunctionDeclASTNode",
        "Name: " + Name,
        std::move(children)
      );
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
  auto codegen() { return nullptr; };

  std::unique_ptr<ASTPrint> to_ast_print() override
  {
    auto children = std::vector<std::unique_ptr<ASTPrint>>();

    children.push_back(make_ast_children("ExternDeclarations:", map_printer(ExternDeclarations)));
    children.push_back(make_ast_children("Declarations:", map_printer(Declarations)));

    return make_ast_print(
        "ProgramASTNode",
        "",
        std::move(children)
      );
  }
};
#pragma endregion
#pragma endregion

#endif