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
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include<map>
#include<memory>
#include <ostream>
#include<stack>

#include "code_gen.hpp"

#include "ast.hpp"

#define BuildInt(var, a, b) var != VariableType::FLOAT ? a(L, R, Name) : b(L, R, Name)

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

std::unique_ptr<LLVMContext> TheContext;
std::unique_ptr<Module> TheModule;
std::unique_ptr<IRBuilder<>> Builder;
std::map<std::string, AllocaInst *> NamedValues;
std::stack<Function *> FunctionStack;

static AllocaInst *CreateAllocaInst(Function *TheFunction, const std::string &VarName) {
    IRBuilder TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());

    std::cout << "Creating alloca for " << VarName << std::endl;
    //Todo: Add support for other types
    return TmpB.CreateAlloca(
        Type::getInt32Ty(*TheContext), 
        0, 
        VarName.c_str());
}

static Type *GetType(VariableType Type) {
    switch (Type) {
        case VariableType::INT:
        case VariableType::BOOL:
            return Type::getInt32Ty(*TheContext);
        case VariableType::FLOAT:
            return Type::getFloatTy(*TheContext);
        default:
            return nullptr;
    }
}

static Type *GetType(TypeSpecType tst) {
    switch (tst) {
        case TypeSpecType::VOID:
            return Type::getVoidTy(*TheContext);
        default:
            return GetType((VariableType) tst);
    }
}

#pragma region Expressions

Value *IntASTNode::codegen() {
    //std::cout << "Found int: " << Val << std::endl;
    return ConstantInt::get(*TheContext, APInt(32, Val, true));
}

Value *FloatASTNode::codegen() {
    //std::cout << "Found float: " << Val << std::endl;
    return ConstantFP::get(*TheContext, APFloat(Val));
}

Value *BoolASTNode::codegen() {
    //return Constant
    //std::cout << "Found bool: " << Val << std::endl;
    return ConstantInt::get(*TheContext, APInt(32, (int)Val, true));
}

std::function<Value*(TOKEN_TYPE Op, llvm::Value *L, const llvm::Twine &Name)> unary_op_builder =
    [](TOKEN_TYPE Op, llvm::Value *L, const llvm::Twine &Name) -> Value *{
        switch(Op) {
            case TOKEN_TYPE::MINUS:
                return Builder->CreateFNeg(L, Name);
            case TOKEN_TYPE::NOT:
                return Builder->CreateNot(L, Name);
            default:
                return nullptr;
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
        
        // std::cout << "Op: " << Op << std::endl;
        // std::cout << "L: " << L->getType()->getTypeID() << std::endl;
        // std::cout << "R: " << R->getType()->getTypeID() << std::endl;
        switch(Op) {
            case TOKEN_TYPE::PLUS:
                return BuildInt(type, Builder->CreateAdd, Builder->CreateFAdd);
            case TOKEN_TYPE::MINUS:
                return BuildInt(type, Builder->CreateSub, Builder->CreateFSub);
            case TOKEN_TYPE::DIV:
                return BuildInt(type, Builder->CreateSDiv, Builder->CreateFDiv);
            case TOKEN_TYPE::ASTERIX:
                return BuildInt(type, Builder->CreateMul, Builder->CreateFMul);
            case TOKEN_TYPE::MOD:
                return BuildInt(type, Builder->CreateSRem, Builder->CreateFRem);
            //Boolean ops:
            case TOKEN_TYPE::OR:
                return Builder->CreateOr(ArrayRef<Value *>(vals));
            case TOKEN_TYPE::AND:
                return Builder->CreateAnd(ArrayRef<Value *>(vals));

            //Comparisons:
            case TOKEN_TYPE::EQ:
                return BuildInt(type, Builder->CreateICmpEQ, Builder->CreateFCmpOEQ);
            case TOKEN_TYPE::NE:
                return BuildInt(type, Builder->CreateICmpNE, Builder->CreateFCmpONE);
                // return type == VariableType::INT
                //     ? Builder->CreateICmpEQ(L, R, Name)
                //     // Check truth
                //     : Builder->CreateFCmpOEQ(L, R, Name);
            case TOKEN_TYPE::GE:
                return BuildInt(type, Builder->CreateICmpSGE, Builder->CreateFCmpOGE);
            case TOKEN_TYPE::GT:
                return BuildInt(type, Builder->CreateICmpSGT, Builder->CreateFCmpOGT);
            case TOKEN_TYPE::LE:
                return BuildInt(type, Builder->CreateICmpSLE, Builder->CreateFCmpOLE);
            case TOKEN_TYPE::LT:
                return BuildInt(type, Builder->CreateICmpSLT, Builder->CreateFCmpOLT);
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
    //std::cout << "Found variable ref: " << Name << std::endl;
    //std::cout << Builder->GetInsertBlock()->getModule() << std::endl;
    if(!V) {
        std::cout << "ERORR: Unknown variable name " << Name << std::endl;
        return nullptr;
        // TODO errors
        //return LogErrorV("Undeclared variable name: " + Name);
    }
    // std::cout << "Found variable: " << Name << " Loading type: " << V->getAllocatedType()->getTypeID() << std::endl;
    return Builder->CreateLoad(V->getAllocatedType(), V, Name.c_str());
}

Value *CallExprAST::codegen() {
    Function *Function = TheModule->getFunction(FunctionName);

    //TODO: error, non-existent
    //TODO: error, arg-size & types

    std::vector<Value *> ArgsIR;

    for(auto &arg : Args) {
        ArgsIR.push_back(arg->codegen());
    }

    return Builder->CreateCall(Function, ArgsIR, "call_tmp");
}

Value *AssignmentASTNode::codegen() {
    if(NamedValues.find(Name) == NamedValues.end()) {
        std::cout << "ERROR: Failed find variable" << std::endl;
        //TODO: throw error
    }

    Value *Val = RHS->codegen();

    if(!Val) {
        //TODO: throw error
        //return nullptr;
    }

    AllocaInst *Alloca = NamedValues[Name];
    Builder->CreateStore(Val, Alloca);
    return Val;
}

#pragma endregion

#pragma region Statements

/// BlockASTNode does writes to the parent block, it does not create one by itself
Value *BlockASTNode::codegen() {

    std::vector<Value *> declCode;
    for(auto &decl: this->Declarations) {
        declCode.push_back(decl->codegen());
    }

    std::vector<Value *> stmtCode;
    for(auto &stmt: this->Statements) {
        stmtCode.push_back(stmt->codegen());
    }

    return Builder->GetInsertBlock();
}

Value *IfElseASTNode::codegen() {
    Value *CondV = Cond->codegen();
    if(!CondV) {
        return nullptr;
    }

    CondV = Builder->CreateICmpNE(CondV, ConstantInt::get(*TheContext, APInt(1, 0, true)), "ifcond");

    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
    BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "after");

    // Jump to respective block, depending on condition
    Builder->CreateCondBr(CondV, ThenBB, ElseBB);

    // Then value
    Builder->SetInsertPoint(ThenBB);

    Value *ThenV = Then->codegen();
    Builder->CreateBr(AfterBB);

    if(!ThenV)
        return nullptr;

    // Else value
    TheFunction->getBasicBlockList().push_back(ElseBB);
    Builder->SetInsertPoint(ElseBB);

    Value *ElseV = Else->codegen();
    if(!ElseV)
        return nullptr;

    Builder->CreateBr(AfterBB);
    TheFunction->getBasicBlockList().push_back(AfterBB);

    Builder->SetInsertPoint(AfterBB);

    return TheFunction;
}

Value *WhileASTNode::codegen() {
    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    FunctionStack.push(TheFunction);

    BasicBlock *CondBB = BasicBlock::Create(*TheContext, "whilecond", TheFunction);
    BasicBlock *BodyBB =  BasicBlock::Create(*TheContext, "whilebody");
    BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "afterwhile");

    Builder->CreateBr(CondBB);

    // Condition
    Builder->SetInsertPoint(CondBB);

    Value *CondV = Cond->codegen();
    if(!CondV) {
        return nullptr;
    }

    CondV = Builder->CreateICmpNE(CondV, ConstantInt::get(*TheContext, APInt(1, 0, true)), "whilecond");

    Builder->CreateCondBr(CondV, BodyBB, AfterBB);

    // Create body
    TheFunction->getBasicBlockList().push_back(BodyBB);
    Builder->SetInsertPoint(BodyBB);
    Body->codegen();
    if(!BodyBB) {
        return nullptr;
    }
    // End of While body, reevaluate condition
    Builder->CreateBr(CondBB);

    // After
    TheFunction->getBasicBlockList().push_back(AfterBB);
    Builder->SetInsertPoint(AfterBB);
    FunctionStack.pop();
    return TheFunction;
}

Value *ReturnStmtASTNode::codegen() {
    Value *RetVal = this->Expr->codegen();

    Builder->CreateRet(RetVal);

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
    Builder->CreateStore(Val, Alloca);
    return Val;
}

#pragma endregion

#pragma region Declarations

Value *VariableDeclASTNode::codegen() {
    //BasicBlock *CurBlock = Builder->GetInsertBlock();
    AllocaInst *Alloca = CreateAllocaInst(FunctionStack.top(), Name);

    //TODO: Redeclaration/shadowing
    NamedValues[Name] = Alloca;
    std::cout << "Declaring variable " << Name << std::endl;

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

    FunctionStack.push(F);
    for(auto &arg : F->args()) {
        arg.setName(Args[arg.getArgNo()]->getName());
    }

    //TODO: error, redeclaration

    // Function body
    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", F);
    Builder->SetInsertPoint(BB);


    //NamedValues.clear();
    for(auto &arg: F->args()) {
        AllocaInst *Alloca = CreateAllocaInst(F, arg.getName().str());

        Builder->CreateStore(&arg, Alloca);

        NamedValues[arg.getName().str()] = Alloca;
    }

    // Do the body's code generation
    Body->codegen();


    //TODO: REMOVE FUNC PARAMS from named values
    FunctionStack.pop();
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

static void InitializeModule() {
  // Open a new context and module.
  TheContext = std::make_unique<LLVMContext>();
  TheModule = std::make_unique<Module>("mini-c", *TheContext);

  // Create a new builder for the module.
  Builder = std::make_unique<IRBuilder<>>(*TheContext);
}

Value *ProgramASTNode::codegen() {
    InitializeModule();

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