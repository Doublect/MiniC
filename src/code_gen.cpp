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

#define BuildInt(var, a, b) var != VariableType::FLOAT ? a(L, R, Name) : b(L, R, Name)

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

LLVMContext TheContext;
IRBuilder Builder(TheContext);
std::unique_ptr<Module> TheModule;
std::map<std::string, AllocaInst *> NamedValues;

static AllocaInst *CreateAllocaInst(Function *TheFunction, const std::string &VarName) {
    IRBuilder TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());

    //Todo: Add support for other types
    return TmpB.CreateAlloca(Type::getInt32Ty(TheContext), 0, VarName.c_str());
}

static AllocaInst *CreateBlockAlloca(BasicBlock *TheBlock, const std::string &VarName) {
    IRBuilder TmpB(TheBlock, TheBlock->begin());

    return TmpB.CreateAlloca(Type::getInt32Ty(TheContext), 0, VarName.c_str());
}

static Type *GetType(VariableType Type) {
    switch (Type) {
        case VariableType::INT:
        case VariableType::BOOL:
            return Type::getInt32Ty(TheContext);
        case VariableType::FLOAT:
            return Type::getFloatTy(TheContext);
        default:
            return nullptr;
    }
}

static Type *GetType(TypeSpecType tst) {
    switch (tst) {
        case TypeSpecType::VOID:
            return Type::getVoidTy(TheContext);
        default:
            return GetType((VariableType) tst);
    }
}

#pragma region Expressions

Value *IntASTNode::codegen() {
    return ConstantInt::get(TheContext, APSInt(Val));
}

Value *FloatASTNode::codegen() {
    return ConstantFP::get(TheContext, APFloat(Val));
}

Value *BoolASTNode::codegen() {
    //return Constant
    return ConstantInt::get(TheContext, APSInt((int)Val));
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

Value *UnaryASTNode::codegen() {
    Value *L = Operand->codegen();
    return unary_op_builder(Op, L, "unary");
}

std::function<Value *(VariableType type, TOKEN_TYPE Op, llvm::Value *L, llvm::Value *R, const llvm::Twine &Name)> operation_function =
    [](VariableType type, TOKEN_TYPE Op, llvm::Value *L, llvm::Value *R, const llvm::Twine &Name) -> Value* {
        //std::function<Value *(llvm::Value *L, llvm::Value *R, const llvm::Twine &Name)> *func;
        std::vector<Value *> vals{L, R};
        

        switch(Op) {
            case TOKEN_TYPE::PLUS:
                return BuildInt(type, Builder.CreateAdd, Builder.CreateFAdd);
            case TOKEN_TYPE::MINUS:
                return BuildInt(type, Builder.CreateSub, Builder.CreateFSub);
            case TOKEN_TYPE::DIV:
                return BuildInt(type, Builder.CreateSDiv, Builder.CreateFDiv);
            case TOKEN_TYPE::ASTERIX:
                return BuildInt(type, Builder.CreateMul, Builder.CreateFMul);
            case TOKEN_TYPE::MOD:
                return BuildInt(type, Builder.CreateSRem, Builder.CreateFRem);
            //Boolean ops:
            case TOKEN_TYPE::OR:
                return Builder.CreateOr(ArrayRef<Value *>(vals));
            case TOKEN_TYPE::AND:
                return Builder.CreateAnd(ArrayRef<Value *>(vals));

            //Comparisons:
            case TOKEN_TYPE::EQ:
                return BuildInt(type, Builder.CreateICmpEQ, Builder.CreateFCmpOEQ);
            case TOKEN_TYPE::NE:
                return BuildInt(type, Builder.CreateICmpNE, Builder.CreateFCmpONE);
                // return type == VariableType::INT
                //     ? Builder.CreateICmpEQ(L, R, Name)
                //     // Check truth
                //     : Builder.CreateFCmpOEQ(L, R, Name);
            case TOKEN_TYPE::GE:
                return BuildInt(type, Builder.CreateICmpSGE, Builder.CreateFCmpOGE);
            case TOKEN_TYPE::GT:
                return BuildInt(type, Builder.CreateICmpSGT, Builder.CreateFCmpOGT);
            case TOKEN_TYPE::LE:
                return BuildInt(type, Builder.CreateICmpSLE, Builder.CreateFCmpOLE);
            case TOKEN_TYPE::LT:
                return BuildInt(type, Builder.CreateICmpSLT, Builder.CreateFCmpOLT);
            default:
                return nullptr;
        }
    };

Value *BinaryASTNode::codegen() {
    Value *L = LHS->codegen();
    Value *R = RHS->codegen();

    //TODO: type checking
    VariableType type = VariableType::INT;

    return operation_function(type, Op, L, R, "binary_op");
}

Value *VariableRefASTNode::codegen() {
    AllocaInst *V = NamedValues[Name];

    if(!V) {
        // TODO errors
        //return LogErrorV("Undeclared variable name: " + Name);
    }

    return Builder.CreateLoad(V->getAllocatedType(), V, Name.c_str());
}

Value *CallExprAST::codegen() {
    Function *Function = TheModule->getFunction(FunctionName);

    //TODO: error, non-existent
    //TODO: error, arg-size & types

    std::vector<Value *> ArgsIR;

    for(auto &arg : Args) {
        ArgsIR.push_back(arg->codegen());
    }

    return Builder.CreateCall(Function, ArgsIR, "call_tmp");
}

Value *AssignmentASTNode::codegen() {
    if(NamedValues.find(Name) == NamedValues.end()) {
        //TODO: throw error
    }

    Value *Val = RHS->codegen();

    if(!Val) {
        //TODO: throw error
        //return nullptr;
    }

    AllocaInst *Alloca = NamedValues[Name];
    Builder.CreateStore(Val, Alloca);
    return Val;
}

#pragma endregion

#pragma region Statements

Value *BlockASTNode::codegen() {
    BasicBlock *block = BasicBlock::Create(TheContext, "block");
    
    std::vector<Value *> declCode;
    for(auto &decl: this->Declarations) {
        declCode.push_back(decl->codegen());
    }

    std::vector<Value *> stmtCode;
    for(auto &stmt: this->Statements) {
        stmtCode.push_back(stmt->codegen());
    }

    //block->getInstList().insert(block->end(), declCode.begin(), declCode.end());
    //block->getInstList().insert(block->end(), stmtCode.begin(), stmtCode.end());

    return block;
}

Value *IfElseASTNode::codegen() {
    Value *CondV = Cond->codegen();
    if(!CondV) {
        return nullptr;
    }

    CondV = Builder.CreateICmpNE(CondV, ConstantInt::get(TheContext, APInt(1, 0, true)), "ifcond");

    Function *TheFunction = Builder.GetInsertBlock()->getParent();

    BasicBlock *ThenBB = BasicBlock::Create(TheContext, "then");
    BasicBlock *ElseBB = BasicBlock::Create(TheContext, "else");

    Builder.CreateCondBr(CondV, ThenBB, ElseBB);

    // Then value
    Builder.SetInsertPoint(ThenBB);

    Value *ThenV = Then->codegen();
    if(!ThenV)
        return nullptr;

    // Else value
    TheFunction->getBasicBlockList().push_back(ElseBB);
    Builder.SetInsertPoint(ElseBB);

    Value *ElseV = Else->codegen();
    if(!ElseV)
        return nullptr;

    return TheFunction;
}

Value *WhileASTNode::codegen() {
    Function *TheFunction = Builder.GetInsertBlock()->getParent();

    BasicBlock *CondBB = BasicBlock::Create(TheContext, "whilecond");
    BasicBlock *BodyBB = BasicBlock::Create(TheContext, "whilebody");
    BasicBlock *AfterBB = BasicBlock::Create(TheContext, "afterwhile");

    Builder.CreateBr(CondBB);

    // Condition
    Builder.SetInsertPoint(CondBB);

    Value *CondV = Cond->codegen();
    if(!CondV) {
        return nullptr;
    }

    CondV = Builder.CreateICmpNE(CondV, ConstantInt::get(TheContext, APInt(1, 0, true)), "whilecond");

    Builder.CreateCondBr(CondV, BodyBB, AfterBB);

    // Body
    Builder.SetInsertPoint(BodyBB);

    Value *BodyV = Body->codegen();
    if(!BodyV) {
        return nullptr;
    }

    Builder.CreateBr(CondBB);

    // After
    Builder.SetInsertPoint(AfterBB);

    return TheFunction;
}

Value *ReturnStmtASTNode::codegen() {
    Value *RetVal = this->Expr->codegen();

    Builder.CreateRet(RetVal);

    return RetVal;
}

Value *AssignmentStmtASTNode::codegen() {
    if(NamedValues.find(Name) == NamedValues.end()) {
        //TODO: throw error
    }

    Value *Val = RHS->codegen();

    if(!Val) {
        //TODO: throw error
        //return nullptr;
    }

    AllocaInst *Alloca = NamedValues[Name];
    Builder.CreateStore(Val, Alloca);
    return Val;
}

#pragma endregion

#pragma region Declarations

Value *VariableDeclASTNode::codegen() {
    AllocaInst *Alloca = CreateBlockAlloca(Builder.GetInsertBlock(), Name);

    //TODO: Redeclaration/shadowing
    NamedValues[Name] = Alloca;

    return Alloca;
}

Value *FunctionDeclASTNode::codegen() {
    //TODO: Types
    std::vector<Type *> ArgsTypes;

    for(auto &arg : Args) {
        ArgsTypes.push_back(GetType(arg->getType()));
    }

    //TODO: types
    FunctionType *FT = FunctionType::get(GetType(ReturnType), ArgsTypes, false);

    Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

    for(auto &arg : F->args()) {
        arg.setName(Args[arg.getArgNo()]->getName());
    }

    //TODO: error, redeclaration

    // Function body
    BasicBlock *BB = BasicBlock::Create(TheContext, "entry", F);
    Builder.SetInsertPoint(BB);


    NamedValues.clear();
    for(auto &arg: F->args()) {
        AllocaInst *Alloca = CreateBlockAlloca(BB, arg.getName().str());

        Builder.CreateStore(&arg, Alloca);

        NamedValues[arg.getName().str()] = Alloca;
    }

    // Do the body's code generation
    Body->codegen();

    // Ensure the function is valid
    if(verifyFunction(*F)) {
        return F;
    } else {
        //TODO: error
        return F;
    }
}

Value *ExternFunctionDeclASTNode::codegen() {
    std::vector<Type *> ArgsTypes;

    for(auto &arg : Args) {
        ArgsTypes.push_back(GetType(arg->getType()));
    }

    //TODO: types
    FunctionType *FT = FunctionType::get(GetType(ReturnType), ArgsTypes, false);

    Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

    for(auto &arg : F->args()) {
        arg.setName(Args[arg.getArgNo()]->getName());
    }

    return F;
}

Value *ProgramASTNode::codegen() {
    std::vector<Value *> externDeclCode;
    for(auto &decl: this->ExternDeclarations) {
        externDeclCode.push_back(decl->codegen());
    }

    std::vector<Value *> declCode;
    for(auto &decl: this->Declarations) {
        declCode.push_back(decl->codegen());
    }

    return nullptr;
}