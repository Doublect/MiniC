#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"

#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include<map>

#include "code_gen.hpp"

#include "ast.hpp"

#define Build(var, comp, a, b) var == comp ? a(L, R, Name) : b(L, R, Name)

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

LLVMContext TheContext;
IRBuilder<> Builder(TheContext);
std::unique_ptr<Module> TheModule;
std::map<std::string, Value *> NamedValues;

#pragma region Expressions

auto IntASTNode::codegen() {
    return ConstantInt::get(TheContext, APSInt(Val));
}

auto FloatASTNode::codegen() {
    return ConstantFP::get(TheContext, APFloat(Val));
}

auto BoolASTNode::codegen() {
    //return Constant
    return ConstantFP::get(TheContext, APFloat((float)Val));
}

std::function<Value*(TOKEN_TYPE Op, llvm::Value *L, const llvm::Twine &Name)> unary_op_builder =
    [](TOKEN_TYPE Op, llvm::Value *L, const llvm::Twine &Name) -> Value *{
        switch(Op) {
            case TOKEN_TYPE::MINUS:
                return Builder.CreateFNeg(L, Name);
            case TOKEN_TYPE::NOT:
                return Builder.CreateNot(L, Name);
            default:
                throw "HEllo";
                //TODO: Error
        }        
    };

auto UnaryASTNode::codegen() {
    Value *L = Operand->codegen();
    return unary_op_builder(Op, L, "unary");
}

std::function<Value *(VariableType type, TOKEN_TYPE Op, llvm::Value *L, llvm::Value *R, const llvm::Twine &Name)> operation_function =
    [](VariableType type, TOKEN_TYPE Op, llvm::Value *L, llvm::Value *R, const llvm::Twine &Name) -> Value* {
        //std::function<Value *(llvm::Value *L, llvm::Value *R, const llvm::Twine &Name)> *func;
        std::vector<Value *> vals{L, R};
        
        switch(Op) {
            case TOKEN_TYPE::PLUS:
                return type == VariableType::INT 
                    ? Builder.CreateAdd(L, R, Name)
                    : Builder.CreateFAdd(L, R, Name);
            case TOKEN_TYPE::MINUS:
                return type == VariableType::INT 
                    ? Builder.CreateSub(L, R, Name)
                    : Builder.CreateFSub(L, R, Name);
            case TOKEN_TYPE::DIV:
                return type == VariableType::INT 
                    ? Builder.CreateSDiv(L, R, Name)
                    : Builder.CreateFDiv(L, R, Name);
            case TOKEN_TYPE::ASTERIX:
                return type == VariableType::INT
                    ? Builder.CreateMul(L, R, Name)
                    : Builder.CreateFMul(L, R, Name);
            case TOKEN_TYPE::MOD:
                return type == VariableType::INT
                    ? Builder.CreateSRem(L, R, Name)
                    : Builder.CreateFRem(L, R, Name);
            //Boolean ops:
            case TOKEN_TYPE::OR:
                return Builder.CreateOr(ArrayRef<Value *>(vals));
            case TOKEN_TYPE::AND:
                return Builder.CreateAnd(ArrayRef<Value *>(vals));

            //Comparisons:
            case TOKEN_TYPE::EQ:
                return Build(type, VariableType::INT, Builder.CreateICmpEQ, Builder.CreateFCmpOEQ);
            case TOKEN_TYPE::NE:
                return Build(type, VariableType::INT, Builder.CreateICmpNE, Builder.CreateFCmpONE);
                // return type == VariableType::INT
                //     ? Builder.CreateICmpEQ(L, R, Name)
                //     // Check truth
                //     : Builder.CreateFCmpOEQ(L, R, Name);
            case TOKEN_TYPE::GE:
                return Build(type, VariableType::INT, Builder.CreateICmpSGE, Builder.CreateFCmpOGE);
            case TOKEN_TYPE::GT:
                return Build(type, VariableType::INT, Builder.CreateICmpSGT, Builder.CreateFCmpOGT);
            case TOKEN_TYPE::LE:
                return Build(type, VariableType::INT, Builder.CreateICmpSLE, Builder.CreateFCmpOLE);
            case TOKEN_TYPE::LT:
                return Build(type, VariableType::INT, Builder.CreateICmpSLT, Builder.CreateFCmpOLT);
            default:
                return nullptr;
        }
    };

auto BinaryASTNode::codegen() {
    Value *L = LHS->codegen();
    Value *R = RHS->codegen();

    //TODO: type checking
    VariableType type = VariableType::INT;

    return operation_function(type, Op, L, R, "binary_op");
}

auto VariableRefASTNode::codegen() {
    return NamedValues[Name];
}

auto CallExprAST::codegen() {
    Function *Function = TheModule->getFunction(FunctionName);

    //TODO: error, non-existent
    //TODO: error, arg-size & types

    std::vector<Value *> ArgsIR;

    for(auto &arg : Args) {
        ArgsIR.push_back(arg->codegen());
    }

    return Builder.CreateCall(Function, ArgsIR, "call_tmp");
}

#pragma endregion