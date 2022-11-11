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
#include<ostream>
#include<stack>
#include<set>

#include "code_gen.hpp"

#include "ast.hpp"
#include "helpers.hpp"

#define BuildInt(var, a, b) var != Type::TypeID::FloatTyID ? a(L, R, Name) : b(L, R, Name)

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

std::unique_ptr<LLVMContext> TheContext;
std::unique_ptr<Module> TheModule;
std::unique_ptr<IRBuilder<>> Builder;

class VariableScopeManager {
    std::map<std::string, GlobalVariable *> GlobalVariables;
    std::map<std::string, std::stack<AllocaInst *>> NamedValues;
    std::stack<std::set<std::string>> ScopeStack;

public:
    VariableScopeManager() {
        ScopeStack.push(std::set<std::string>());
    }

    void pushScope() {
        ScopeStack.push(std::set<std::string>());

        print_a_debug("Opening scope" + std::to_string(ScopeStack.size()));
    }

    void popScope() {
        print_a_debug("Closing scope" + std::to_string(ScopeStack.size()));

        for (auto &Name : ScopeStack.top()) {
            NamedValues[Name].pop();
        }
        ScopeStack.pop();
    }

    void addVariable(const std::string &Name, AllocaInst *Alloca) {
        NamedValues[Name].push(Alloca);
        ScopeStack.top().insert(Name);
    }

    void addVariable(const std::string &Name, GlobalVariable *Global) {
        //TODO: redecl?
        GlobalVariables[Name] = Global;
    }

    Value *getVariable(const std::string &Name) {
        print_a_debug("Searching for variable: " + Name);

        if(NamedValues.contains(Name) && NamedValues[Name].size() > 0) {
            return NamedValues[Name].top();
        }

        if(GlobalVariables.contains(Name)) {
            return GlobalVariables[Name];
        }
        // TODO: Error
        return nullptr;
    }

    Type *getType(const std::string &Name) {
        if(NamedValues.contains(Name) && NamedValues[Name].size() > 0) {
            return NamedValues[Name].top()->getAllocatedType();
        }

        if(GlobalVariables.contains(Name)) {
            return GlobalVariables[Name]->getValueType();
        }
        // TODO: Error
        return nullptr;
    }
};

VariableScopeManager VariableScope;

Value *ensureInteger(Value *V) {
    if (V->getType()->isIntegerTy()) {
        if(V->getType()->getIntegerBitWidth() == 1) {
            return Builder->CreateZExt(V, Type::getInt32Ty(*TheContext), "bool_to_int");
        }
        return V;
    }
    // TODO: disallow
    return Builder->CreateFPToSI(V, Type::getInt32Ty(*TheContext), "inttmp");
}

Value *ensureFloat(Value *V) {
    if (V->getType()->isFloatingPointTy()) {
        return V;
    }

    V = ensureInteger(V);
    return Builder->CreateSIToFP(V, Type::getFloatTy(*TheContext), "floattmp");
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

static AllocaInst *CreateAllocaArg(Function *TheFunction, const std::string &VarName, Type *Type) {
    IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());

    print_a_debug("Creating alloca for " + VarName);

    return TmpB.CreateAlloca(
        Type, 
        0, 
        VarName.c_str());
}

#pragma region Expressions

Value *IntASTNode::codegen() {
    // std::cout << "Found int: " << Val << std::endl;
    return ConstantInt::get(*TheContext, APInt(32, Val, true));
}

Value *FloatASTNode::codegen() {
    // std::cout << "Found float: " << Val << std::endl;
    return ConstantFP::get(*TheContext, APFloat(Val));
}

Value *BoolASTNode::codegen() {
    // std::cout << "Found bool: " << Val << std::endl;
    return ConstantInt::get(*TheContext, APInt(32, (int)Val, true));
}

std::function<Value*(TOKEN_TYPE Op, llvm::Value *L, const llvm::Twine &Name)> unary_op_builder =
    [](TOKEN_TYPE Op, llvm::Value *L, const llvm::Twine &Name) -> Value *{
        switch(Op) {
            case TOKEN_TYPE::MINUS:
                return L->getType()->getTypeID() == Type::TypeID::FloatTyID ? 
                    Builder->CreateFNeg(L, Name) : 
                    Builder->CreateNeg(L, Name);
            case TOKEN_TYPE::NOT:
                //return Builder->CreateNot(L, Name);
                return L->getType()->getTypeID() == Type::TypeID::FloatTyID ? 
                    ensureInteger(Builder->CreateFCmpOEQ(L, ConstantFP::get(*TheContext, APFloat(0.0)), Name)) : 
                    ensureInteger(Builder->CreateICmpEQ(L, ConstantInt::get(*TheContext, APInt(32, 0, true)), Name)); 
            default:
                return nullptr;
                //TODO: Error
        }       
    };

Value *UnaryASTNode::codegen() {
    Value *L = Operand->codegen();
    return unary_op_builder(Op, L, "unary");
}

auto operation_function =
    [](Type::TypeID type, TOKEN_TYPE Op, llvm::Value *L, llvm::Value *R, const llvm::Twine &Name) -> Value* {
        //std::function<Value *(llvm::Value *L, llvm::Value *R, const llvm::Twine &Name)> *func;
        std::vector<Value *> vals{L, R};
        
        std::cout << "Op: " << Op << std::endl;
        std::cout << "L: " << std::endl;
        L->print(llvm::outs());
        std::cout << std::endl << "R: " << std::endl;
        R->print(llvm::outs());
        std::cout << std::endl << "Type: " << std::to_string((int)type) << std::endl;
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

    //TODO: bool type checking
    
    Type::TypeID typeID = std::min(L->getType()->getTypeID(), R->getType()->getTypeID());

    // std::cout << L->getType()->getTypeID() << " " << R->getType()->getTypeID() << std::endl;

    if(typeID == Type::TypeID::IntegerTyID) {
        L = ensureInteger(L);
        R = ensureInteger(R);
    }
    if(typeID == Type::TypeID::FloatTyID) {
        L = ensureFloat(L);
        R = ensureFloat(R);
    }

    auto res = operation_function(typeID, Op, L, R, "binary_op");

    if(typeID == Type::TypeID::IntegerTyID) {
        res = ensureInteger(res);
    } else {
        res = ensureFloat(res);
    }
    //TODO: emit warning
    return res;
}

Value *VariableRefASTNode::codegen() {
    Value *V = VariableScope.getVariable(Name);
    //std::cout << "Found variable ref: " << Name << std::endl;
    //std::cout << Builder->GetInsertBlock()->getModule() << std::endl;
    llvm::Type *type = VariableScope.getType(Name);
    
    if(!V || !type) {
        std::cout << "ERROR: Unknown variable name " << Name << std::endl;
        // TODO errors
        //return LogErrorV("Undeclared variable name: " + Name);
    }
    // std::cout << "Found variable: " << Name << " Loading type: " << V->getAllocatedType()->getTypeID() << std::endl;

    return Builder->CreateLoad(type, V, Name.c_str());
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
    Value *Val = RHS->codegen();

    if(!Val) {
        //TODO: throw error
        //return nullptr;
    }

    Value *Alloca = VariableScope.getVariable(Name);
    Builder->CreateStore(Val, Alloca);
    return Val;
}

#pragma endregion

#pragma region Statements

/// BlockASTNode does writes to the parent block, it does not create one by itself
Value *BlockASTNode::codegen() {
    VariableScope.pushScope();

    std::vector<Value *> declCode;
    for(auto &decl: this->Declarations) {
        declCode.push_back(decl->codegen());
    }

    std::vector<Value *> stmtCode;
    for(auto &stmt: this->Statements) {
        stmtCode.push_back(stmt->codegen());
    }

    VariableScope.popScope();
    return Builder->GetInsertBlock();
}

Value *IfElseASTNode::codegen() {
    Value *CondV = Cond->codegen();
    if(!CondV) {
        return nullptr;
    }
    
    // TODO
    if(CondV->getType()->getTypeID() != Type::TypeID::FloatTyID) {
        CondV = Builder->CreateICmpNE(CondV, ConstantInt::get(*TheContext, APInt(32, 0, true)), "ifcond");
    } else {
        CondV = Builder->CreateFCmpONE(CondV, ConstantFP::get(*TheContext, APFloat(0.0f)), "ifcond");
    }


    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
    BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "after");

    // Jump to respective block, depending on condition
    Builder->CreateCondBr(CondV, ThenBB, ElseBB);

    // Then value
    Builder->SetInsertPoint(ThenBB);

    // Create scope for both then and else (as only one of the get executed)
    VariableScope.pushScope();

    Value *ThenV = Then->codegen();
    Builder->CreateBr(AfterBB);

    if(!ThenV)
        return nullptr;

    // Else value
    TheFunction->getBasicBlockList().push_back(ElseBB);
    Builder->SetInsertPoint(ElseBB);

    if(Else) {
        Value *ElseV = Else->codegen();

        if(!ElseV)
            return nullptr;
    }
    
    Builder->CreateBr(AfterBB);

    // Pop variable scope
    VariableScope.popScope();

    Builder->CreateBr(AfterBB);
    TheFunction->getBasicBlockList().push_back(AfterBB);

    Builder->SetInsertPoint(AfterBB);

    return TheFunction;
}

Value *WhileASTNode::codegen() {
    Function *TheFunction = Builder->GetInsertBlock()->getParent();

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

    //CondV = Builder->CreateICmpNE(CondV, ConstantInt::get(*TheContext, APInt(32, 0, true)), "whilecond");

    if(CondV->getType()->getTypeID() != Type::TypeID::FloatTyID) {
        CondV = Builder->CreateICmpNE(CondV, ConstantInt::get(*TheContext, APInt(32, 0, true)), "whilecond");
    } else {
        CondV = Builder->CreateFCmpONE(CondV, ConstantFP::get(*TheContext, APFloat(0.0f)), "whilecond");
    }

    Builder->CreateCondBr(CondV, BodyBB, AfterBB);

    // Create new scope for body 
    VariableScope.pushScope();

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

    // Pop scope
    VariableScope.popScope();
    return TheFunction;
}

Value *ReturnStmtASTNode::codegen() {
    Value *RetVal = Expr ? Expr->codegen() : nullptr;

    Builder->CreateRet(RetVal);

    return RetVal;
}

#pragma endregion

#pragma region Declarations

Value *VariableDeclASTNode::codegen() {
    if(Builder->GetInsertBlock() == nullptr) {
        Constant *consta = Type == VariableType::FLOAT ? (Constant *)ConstantFP::get(*TheContext, APFloat(0.0f)) : (Constant *)ConstantInt::get(*TheContext, APInt(32, 0, true));
        GlobalVariable* g =
            new GlobalVariable(*TheModule, GetType(Type), false, GlobalValue::CommonLinkage, consta, Name);
        
        //std::cout << "Global variable: " << (g->getType()->getTypeID() == Type::TypeID::PointerTyID) << std::endl;

        VariableScope.addVariable(Name, g);
        return g;
    } else {
        Function *TheFunction = Builder->GetInsertBlock()->getParent();
        AllocaInst *Alloca = CreateAllocaArg(TheFunction, Name, GetType(Type));

        VariableScope.addVariable(Name, Alloca);
        std::cout << "Declaring variable " << Name << std::endl;
        return Alloca;
    }
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
    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", F);
    Builder->SetInsertPoint(BB);


    // Create a new scope for the function
    VariableScope.pushScope();
    for(auto &arg: F->args()) {
        AllocaInst *Alloca = CreateAllocaArg(F, arg.getName().str(), arg.getType());

        Builder->CreateStore(&arg, Alloca);
        VariableScope.addVariable(arg.getName().str(), Alloca);
    }

    // Do the body's code generation
    Body->codegen();


    VariableScope.popScope();
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
    for(auto &decl: ExternDeclarations) {
        externDeclCode.push_back(decl->codegen());
    }

    std::vector<Value *> declCode;
    for(auto &decl: Declarations) {
        declCode.push_back(decl->codegen());
    }

    return nullptr;
}
