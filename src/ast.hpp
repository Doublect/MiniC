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
static std::vector<std::shared_ptr<ASTPrint>> map_printer(std::vector<std::unique_ptr<T>> &nodes)
{
  std::vector<std::shared_ptr<ASTPrint>> asts;
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
  virtual ~ASTNode() = default;
  virtual Value *codegen() = 0;
  virtual std::string to_string() { return "ASTNode"; };

  virtual std::shared_ptr<ASTPrint> to_ast_print() = 0;
};

class StatementASTNode : public ASTNode
{
public:
  StatementASTNode() = default;
  virtual Value *codegen() override = 0;
  virtual std::shared_ptr<ASTPrint> to_ast_print() override = 0;
};

class ExprASTNode : public StatementASTNode
{
protected:
  TypeSpecType Type;
public:
  ExprASTNode() = default;
  virtual Value *codegen() override = 0;
  virtual std::shared_ptr<ASTPrint> to_ast_print() override = 0;

  virtual TypeSpecType getType() { return Type; };
};

/// AST representation of an integer number
class IntASTNode : public ExprASTNode
{
  int Val;
  TOKEN Tok;
  std::string Name;

public:
  IntASTNode(TOKEN tok, int val) : Val(val), Tok(tok) {
    Type = TypeSpecType::INT;
  }
  Value *codegen() override;
  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    return make_ast_print(
        "IntASTNode",
        std::to_string(Val),
        std::vector<std::shared_ptr<ASTPrint>>());
  };
};

/// AST representation of a float literal
class FloatASTNode : public ExprASTNode
{
  float Val;
  TOKEN Tok;
  std::string Name;

public:
  FloatASTNode(TOKEN tok, float val) : Val(val), Tok(tok) {
    Type = TypeSpecType::FLOAT;
  }
  Value *codegen() override;
  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    return make_ast_print(
        "FloatASTNode",
        std::to_string(Val),
        std::vector<std::shared_ptr<ASTPrint>>());
  };
};

/// AST representation of a boolean value
class BoolASTNode : public ExprASTNode
{
  bool Val;
  TOKEN Tok;
  std::string Name;

public:
  BoolASTNode(TOKEN tok, bool val) : Val(val), Tok(tok) {
    Type = TypeSpecType::BOOL;
  }
  Value *codegen() override;
  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    return make_ast_print(
        "BoolASTNode",
        std::to_string(Val),
        std::vector<std::shared_ptr<ASTPrint>>());
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
      : Op(op), Operand(std::move(operand)), Tok(tok) { Type = Operand->getType(); }
  Value *codegen() override;

  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    return make_ast_print( 
        "UnaryASTNode",
        "Op: " + Tok.lexeme,
        {Operand->to_ast_print()}
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
  Value *codegen() override;

  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    return make_ast_print(
        "BinaryASTNode",
        "Op: " + Tok.lexeme,
        {LHS->to_ast_print(), RHS->to_ast_print()}
    );        
  };
};

/// AST representation of a variable reference
class VariableRefASTNode : public ExprASTNode
{
  std::string Name;
  TOKEN Tok;

public:
  VariableRefASTNode(TOKEN tok, std::string &&Name) : Name(Name) {}
  Value *codegen() override;
  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    return make_ast_print(
        "VariableRefASTNode",
        std::string(Name),
        std::vector<std::shared_ptr<ASTPrint>>()
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
  CallExprAST(TOKEN tok, std::string &&funcName,
              std::vector<std::unique_ptr<ExprASTNode>> Args)
      : tok(tok), FunctionName(funcName), Args(std::move(Args)) {}
  Value *codegen() override;

  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    return make_ast_print(
        "CallExprAST",
        "FunctionName: " + FunctionName,
        {make_ast_print("Args:", "", map_printer(Args))}
      );
  };
};

class AssignmentASTNode : public ExprASTNode
{
  std::string Name;
  std::unique_ptr<ExprASTNode> RHS;
  TOKEN Tok;

public:
  AssignmentASTNode(TOKEN tok, std::string &&Name,
                    std::unique_ptr<ExprASTNode> RHS)
      : Name(Name), RHS(std::move(RHS)) {}
  Value *codegen() override;
  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    return make_ast_print(
        "AssignmentASTNode",
        "Name: " + Name,
        {RHS->to_ast_print()}
    );
  };
};

///----------------------------------------------------------------------------
/// Statements
///----------------------------------------------------------------------------
#pragma region Statements

class VariableDeclASTNode;

class BlockASTNode : public StatementASTNode
{
  std::vector<std::unique_ptr<DeclASTNode>> Declarations;
  std::vector<std::unique_ptr<StatementASTNode>> Statements;

public:
  BlockASTNode(std::vector<std::unique_ptr<DeclASTNode>> &&declarations,
    std::vector<std::unique_ptr<StatementASTNode>> &&statements) 
    : Declarations(std::move(declarations)), Statements(std::move(statements)) {}
  Value *codegen() override;
  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    return make_ast_print(
        "BlockASTNode",
        "",
        {
          make_ast_children("Declarations:", map_printer(Declarations)),
          make_ast_children("Statements:", map_printer(Statements))
        }
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
  Value *codegen() override;
  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    std::vector<std::shared_ptr<ASTPrint>> children {
      make_ast_labelled("Cond:", Cond->to_ast_print()),
      make_ast_labelled("Then:", Then->to_ast_print())
    };
    //TODO: recheck brevity

    if(Else)
      children.push_back(make_ast_labelled("Else:", Else->to_ast_print()));

    return make_ast_print(
        "IfElseASTNode",
        "",
        std::move(children)
      );
  }
};

class WhileASTNode : public StatementASTNode
{
  std::unique_ptr<ExprASTNode> Cond;
  std::unique_ptr<StatementASTNode> Body;

public:
  WhileASTNode(std::unique_ptr<ExprASTNode> Cond, std::unique_ptr<StatementASTNode> Body)
      : Cond(std::move(Cond)), Body(std::move(Body)) {}
  Value *codegen() override;

  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    return make_ast_print(
        "WhileASTNode",
        "",
        {
          make_ast_labelled("Cond:", Cond->to_ast_print()),
          make_ast_labelled("Body:", Body->to_ast_print())
        }
      );
  }
};

class ReturnStmtASTNode : public StatementASTNode
{
  std::unique_ptr<ExprASTNode> Expr;

public:
  ReturnStmtASTNode(std::unique_ptr<ExprASTNode> expr) : Expr(std::move(expr)) {}
  Value *codegen() override;

  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    auto children = std::vector<std::shared_ptr<ASTPrint>>();

    if(Expr) {
      children.push_back(make_ast_labelled("Expr:", Expr->to_ast_print()));
    }

    return make_ast_print(
        "WhileASTNode",
        "",
        std::move(children)
      );
  }
};

class EmptyStatementASTNode : public StatementASTNode
{

public:
  EmptyStatementASTNode() {}
  Value *codegen() override { return nullptr; };
  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    return make_ast_print("EmptyStatementASTNode");
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
  DeclASTNode() = default;

  virtual Value *codegen() override = 0;
  virtual std::shared_ptr<ASTPrint> to_ast_print() override = 0;
};

class VariableDeclASTNode : public DeclASTNode
{
protected:
  std::string Name;
  TOKEN Tok;
  VariableType Type;

public:
  VariableDeclASTNode(TOKEN tok, std::string &Name, VariableType type) : Name(std::move(Name)), Type(type) {}
  Value *codegen() override;

  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    return make_ast_print(
        "VariableDeclASTNode",
        "Name: " + Name,
        { make_ast_leaf("Type: " + std::to_string((int)Type)) }
        );
  }
  VariableType getType() { return Type; }
  std::string getName() { return Name; }
};

class FunctionParameterASTNode : public VariableDeclASTNode
{
  // Inherit constructor
  using VariableDeclASTNode::VariableDeclASTNode;

public:
  using VariableDeclASTNode::codegen;
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
  Value *codegen() override;

  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    return make_ast_print(
        "FunctionDeclASTNode",
        "Name: " + Name,
        {
          make_ast_children("Args:", map_printer(Args)),
          make_ast_labelled("Body:", Body->to_ast_print()),
          make_ast_leaf("ReturnType: " + std::to_string(ReturnType))
        }
      );
  }
};

class ExternFunctionDeclASTNode : public DeclASTNode
{
  std::string Name;
  std::vector<std::unique_ptr<FunctionParameterASTNode>> Args;
  TypeSpecType ReturnType;

public:
  ExternFunctionDeclASTNode(std::string Name,
                            std::vector<std::unique_ptr<FunctionParameterASTNode>> Args,
                            TypeSpecType returnType)
      : Name(Name), Args(std::move(Args)), ReturnType(returnType) {}
  Value *codegen() override;

  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    return make_ast_print(
        "ExternFunctionDeclASTNode",
        "Name: " + Name,
        {
          make_ast_children("Args:", map_printer(Args)),
          make_ast_leaf("ReturnType: " + std::to_string(ReturnType))
        }
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
  Value *codegen() override;

  std::shared_ptr<ASTPrint> to_ast_print() override
  {
    return make_ast_print(
        "ProgramASTNode",
        "",
        {
          make_ast_children("ExternDeclarations:", map_printer(ExternDeclarations)),
          make_ast_children("Declarations:", map_printer(Declarations))
        }
      );
  }
};
#pragma endregion
#pragma endregion

#endif