#ifndef AST_H
#define AST_H
    

#include "llvm/IR/Value.h"

#include <iostream>
#include <string>
#include <vector>

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
//   virtual Value *codegen();
//   virtual std::string to_string();

//   virtual std::unique_ptr<ASTPrint> to_ast_print();

//   virtual void print(std::string indent, std::string str = "", bool last = false);

//   virtual void print_string(std::string indent, std::string str, bool last = false);
// };

// template <typename T>
// static std::vector<std::unique_ptr<ASTPrint>> map_printer(const std::vector<std::unique_ptr<T>> &nodes);

// class StatementASTNode : public ASTNode
// {
// public:
//     StatementASTNode();
//     virtual Value *codegen();
//     virtual void print(std::string indent);
//     };

// class ExprASTNode : public StatementASTNode
// {
// public:
//     ExprASTNode();
//     virtual Value *codegen();
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
//     virtual Value *codegen();
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
//     virtual Value *codegen();
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
//     virtual Value *codegen();
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
//     virtual Value *codegen();

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
//     virtual Value *codegen();

//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// /// AST representation of a variable reference
// class VariableRefASTNode : public ExprASTNode
// {
//     std::string Name;
//     TOKEN Tok;

// public:
//     VariableRefASTNode(TOKEN tok, const std::string &Name);
//     virtual Value *codegen();
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
//     virtual Value *codegen();

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
//     virtual Value *codegen();
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
//     virtual Value *codegen();

//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class VariableDeclASTNode;

// class BlockASTNode : public StatementASTNode
// {
//     std::vector<std::unique_ptr<VariableDeclASTNode>> Declarations;
//     std::vector<std::unique_ptr<StatementASTNode>> Statements;

// public:
//     BlockASTNode(std::vector<std::unique_ptr<StatementASTNode>> statements);
//     virtual Value *codegen();
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
//     virtual Value *codegen();
//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class WhileASTNode : public StatementASTNode
// {
//     std::unique_ptr<ExprASTNode> Cond;
//     std::unique_ptr<BlockASTNode> Body;

// public:
//     WhileASTNode(std::unique_ptr<ExprASTNode> Cond, std::unique_ptr<BlockASTNode> Body);
//     virtual Value *codegen();

//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class ReturnStmtASTNode : public StatementASTNode
// {
//     std::unique_ptr<ExprASTNode> Expr;

// public:
//     ReturnStmtASTNode(std::unique_ptr<ExprASTNode> expr);
//     virtual Value *codegen();

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
//     virtual Value *codegen();
//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class EmptyStatementASTNode : public StatementASTNode
// {

// public:
//     EmptyStatementASTNode();
//     virtual Value *codegen();
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
//     virtual Value *codegen();
//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class VariableDeclASTNode : public DeclASTNode
// {
//     std::string Name;
//     TOKEN Tok;
//     VariableType Type;

// public:
//     VariableDeclASTNode(TOKEN tok, const std::string &Name, VariableType type);
//     virtual Value *codegen();

//     virtual std::unique_ptr<ASTPrint> to_ast_print();
// };

// class FunctionParameterASTNode : public VariableDeclASTNode
// {
//     // Inherit constructor
//     using VariableDeclASTNode::VariableDeclASTNode;

// public:
//     virtual Value *codegen();
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
//     virtual Value *codegen();

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
//     virtual Value *codegen();

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
//     virtual Value *codegen();

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

#include "ast.cpp"

#endif