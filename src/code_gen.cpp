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
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include <llvm/IR/Attributes.h>
#include<map>
#include<memory>
#include<ostream>
#include<stack>
#include<set>

#include "code_gen.hpp"

#include "ast.hpp"
#include "code_gen_helpers.hpp"
#include "errors.hpp"
#include "helpers.hpp"

#define BuildInt(var, a, b) var != Type::TypeID::FloatTyID ? a(L, R, Name) : b(L, R, Name)

extern std::string fileName;

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

std::shared_ptr<LLVMContext> TheContext;
std::unique_ptr<Module> TheModule;
std::shared_ptr<IRBuilder<>> Builder;

VariableScopeManager VariableScope;
std::unique_ptr<VariableCastManager> VariableCaster;

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

static std::string GetTypeName(Type *type) {
    switch(type->getTypeID()) {
        case Type::TypeID::IntegerTyID:
            return "integer";

        case Type::TypeID::FloatTyID:
            return "float";

        case Type::TypeID::PointerTyID:
            return "pointer";

        default:
            return "unknown";
    }
}

static AllocaInst *CreateAllocaArg(Function *TheFunction, const std::string &VarName, Type *Type) {
    IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());

    PrintDebug("Creating alloca for " + VarName);

    return TmpB.CreateAlloca(
        Type, 
        nullptr, 
        VarName.c_str());
}

#pragma region Expressions

Value *IntASTNode::codegen() {
    return ConstantInt::get(*TheContext, APInt(32, Val, true));
}

Value *FloatASTNode::codegen() {
    return ConstantFP::get(*TheContext, APFloat(Val));
}

Value *BoolASTNode::codegen() {
    return ConstantInt::get(*TheContext, APInt(32, (int)Val, true));
}

inline Value *BuildUnaryOperator(TOKEN_TYPE Op, llvm::Value *L, const llvm::Twine &Name) {
        switch(Op) {
            case TOKEN_TYPE::MINUS:
                return L->getType()->getTypeID() == Type::TypeID::FloatTyID ? 
                    Builder->CreateFNeg(L, Name) : 
                    Builder->CreateNeg(L, Name);
            case TOKEN_TYPE::NOT:
                // Comparison operators are guaranteed to return i1
                return L->getType()->getTypeID() == Type::TypeID::FloatTyID ? 
                        VariableCaster->ensureInteger(Builder->CreateFCmpOEQ(L, ConstantFP::get(*TheContext, APFloat(0.0f)), Name)) : 
                        VariableCaster->ensureInteger(Builder->CreateICmpEQ(L, ConstantInt::get(*TheContext, APInt(32, 0, true)), Name));
            default:
                throw std::runtime_error("Invalid unary operator");
        }       
    };

Value *UnaryASTNode::codegen() {
    Value *L = Operand->codegen();

    return BuildUnaryOperator(Op, L, "unary");
}

inline Value *BuildBinaryOperator(Type::TypeID type, TOKEN_TYPE Op, llvm::Value *L, llvm::Value *R, const llvm::Twine &Name) {
        std::vector<Value *> vals{L, R};

        switch(Op) {
            // Arithmetic ops:
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

            // Boolean ops:
            case TOKEN_TYPE::OR:
                return Builder->CreateOr(ArrayRef<Value *>(vals));
            case TOKEN_TYPE::AND:
                return Builder->CreateAnd(ArrayRef<Value *>(vals));

            // Comparisons:
            case TOKEN_TYPE::EQ:
                return BuildInt(type, Builder->CreateICmpEQ, Builder->CreateFCmpOEQ);
            case TOKEN_TYPE::NE:
                return BuildInt(type, Builder->CreateICmpNE, Builder->CreateFCmpONE);
            case TOKEN_TYPE::GE:
                return BuildInt(type, Builder->CreateICmpSGE, Builder->CreateFCmpOGE);
            case TOKEN_TYPE::GT:
                return BuildInt(type, Builder->CreateICmpSGT, Builder->CreateFCmpOGT);
            case TOKEN_TYPE::LE:
                return BuildInt(type, Builder->CreateICmpSLE, Builder->CreateFCmpOLE);
            case TOKEN_TYPE::LT:
                return BuildInt(type, Builder->CreateICmpSLT, Builder->CreateFCmpOLT);

            default:
                throw std::runtime_error("Invalid binary operator");
        }
    };

Value *BinaryASTNode::codegen() {
    Value *L = LHS->codegen();
    Value *R = RHS->codegen();
    PrintDebug(
            "L: " + std::to_string((int)L->getType()->getTypeID()) 
            + " R: " + std::to_string((int)R->getType()->getTypeID()) 
        );

    auto [L_cast, R_cast, typeID] = VariableCaster->ensureSharedType(L, R);

    if(L_cast == nullptr) {
        SemanticError("Cannot upcast float to integer", LHS->getToken(), fileName);
    }
    if(R_cast == nullptr) {
        SemanticError("Cannot upcast float to integer", RHS->getToken(), fileName);
    }

    auto res = BuildBinaryOperator(typeID, Op, L_cast, R_cast, "binary_op");

    return res;
}

Value *VariableRefASTNode::codegen() {
    auto res = VariableScope.getVariable(Name);

    if(!res.has_value()) {
        SemanticError("Use of undeclared variable: `" + Name + "`.", Tok, fileName);
    }
    
    auto [V, Type] = res.value();

    PrintDebug("Found variable reference: " + Name);

    return Builder->CreateLoad(Type, V, Name.c_str());
}

Value *CallExprAST::codegen() {
    Function *Function = TheModule->getFunction(FunctionName);

    if(!Function) {
        ErrorC("Unknown function referenced: " + FunctionName, Tok).msg(fileName);
    }
    if(Function->getFunctionType()->getNumParams() != Args.size()) {
        SemanticError("Function `" + FunctionName + "` expects `" + std::to_string(Function->getFunctionType()->getNumParams()) + "` arguments, but `" + std::to_string(Args.size()) + "` were provided", Tok, fileName);
    }

    std::vector<Value *> ArgsIR;

    for(int i = 0; i < Args.size(); i++) {

        Value *argValue = Args[i]->codegen();

        if(argValue->getType() != Function->getFunctionType()->getParamType(i)) {
            SemanticWarning("Implicit cast from `" + type_to_string(argValue->getType()) + "` to `" + type_to_string(Function->getFunctionType()->getParamType(i)) + "`.", Tok, fileName);
            
            argValue = VariableCaster->ensureParamType(argValue, Function->getFunctionType()->getParamType(i));
        }

        ArgsIR.push_back(argValue);
    }

    // A name can not be assigned to a function call which returns void
    std::string name = Function->getReturnType()->isVoidTy() ? "" : "call_tmp";

    return Builder->CreateCall(Function, ArgsIR, name);
}

Value *AssignmentASTNode::codegen() {
    Value *Val = RHS->codegen();

    if(!Val) {
        SemanticError("Invalid assignment of expression", Tok, fileName);
    }


    auto res = VariableScope.getVariable(Name);
    if(!res.has_value()) {
        SemanticError("Use of undeclared variable: `" + Name + "`.", Tok, fileName);
    }
    
    auto [Alloca, AllType] = res.value();

    Val = VariableCaster->ensureType(Val, AllType->getTypeID());
    if(!Val) {
        SemanticError("Cannot assign value of type `" + GetTypeName(Val->getType()) 
            + "` to variable of type `" + GetTypeName(AllType) + "`.", Tok, fileName);
    }

    Builder->CreateStore(Val, Alloca);
    return Val;
}

#pragma endregion

#pragma region Statements

/// BlockASTNode does writes to the parent block, it does not create one by itself
Value *BlockASTNode::codegen() {
    if(hasScope)
        VariableScope.pushScope();

    std::vector<Value *> declCode;
    for(auto &decl: this->Declarations) {
        declCode.push_back(decl->codegen());
    }

    std::vector<Value *> stmtCode;
    for(auto &stmt: this->Statements) {
        stmtCode.push_back(stmt->codegen());
    }

    if(hasScope)
        VariableScope.popScope();
    return Builder->GetInsertBlock();
}

Value *addConditionComparison(Value *CondV) {
    if(CondV->getType()->getTypeID() != Type::TypeID::FloatTyID) {
        return Builder->CreateICmpNE(CondV, ConstantInt::get(*TheContext, APInt(CondV->getType()->getIntegerBitWidth(), 0, true)), "whilecond");
    } else {
        return Builder->CreateFCmpONE(CondV, ConstantFP::get(*TheContext, APFloat(0.0f)), "whilecond");
    }
}

Value *IfElseASTNode::codegen() {
    Value *CondV = Cond->codegen();
    if(!CondV) {
        return nullptr;
    }
    
    CondV = addConditionComparison(CondV);

    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
    BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "after");

    // Jump to respective block, depending on condition
    Builder->CreateCondBr(CondV, ThenBB, ElseBB);

    // Then value
    Builder->SetInsertPoint(ThenBB);

    Value *ThenV = Then->codegen();

    if(!Builder->GetInsertBlock()->getTerminator()) {
        Builder->CreateBr(AfterBB);
    }

    if(!ThenV)
        return nullptr;

    // Else value
    TheFunction->getBasicBlockList().push_back(ElseBB);
    Builder->SetInsertPoint(ElseBB);

    if(Else) {
        Value *ElseV = Else->codegen();
    }

    if(!Builder->GetInsertBlock()->getTerminator()) {
        Builder->CreateBr(AfterBB);
    }
    TheFunction->getBasicBlockList().push_back(AfterBB);

    Builder->SetInsertPoint(AfterBB);

    return TheFunction;
}

Value *LazyOrASTNode::codegen() {
    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    BasicBlock *LeftBB =
        BasicBlock::Create(*TheContext, "left", TheFunction);
    BasicBlock *RightBB = BasicBlock::Create(*TheContext, "right");
    BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "orcont");

    Builder->CreateBr(LeftBB);
    Builder->SetInsertPoint(LeftBB);
    Value *Left = LHS->codegen();

    if(!Left) {
        return nullptr;
    }

    BasicBlock *LeftReturnBB = Builder->GetInsertBlock();

    Left = addConditionComparison(Left);
    Builder->CreateCondBr(Left, AfterBB, RightBB);

    TheFunction->getBasicBlockList().push_back(RightBB);
    Builder->SetInsertPoint(RightBB);
    Value *Right = RHS->codegen();

    if(!Right) {
        return nullptr;
    }

    BasicBlock *RightReturnBB = Builder->GetInsertBlock();

    Builder->CreateBr(AfterBB);
    TheFunction->getBasicBlockList().push_back(AfterBB);
    Builder->SetInsertPoint(AfterBB);

    PHINode *PN = Builder->CreatePHI(Type::getInt32Ty(*TheContext), 2, "orres");

    PN->addIncoming(ConstantInt::get(*TheContext, APInt(32, 1, true)), LeftReturnBB);
    PN->addIncoming(Right, RightReturnBB);

    return PN;
}

Value *LazyAndASTNode::codegen() {
    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    BasicBlock *LeftBB =
        BasicBlock::Create(*TheContext, "left", TheFunction);
    BasicBlock *RightBB = BasicBlock::Create(*TheContext, "right");
    BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "andcont");

    Builder->CreateBr(LeftBB);
    Builder->SetInsertPoint(LeftBB);
    Value *Left = LHS->codegen();

    if(!Left) {
        return nullptr;
    }

    BasicBlock *LeftReturnBB = Builder->GetInsertBlock();

    Left = addConditionComparison(Left);
    Builder->CreateCondBr(Left, RightBB, AfterBB);

    TheFunction->getBasicBlockList().push_back(RightBB);
    Builder->SetInsertPoint(RightBB);
    Value *Right = RHS->codegen();

    if(!Right) {
        return nullptr;
    }

    BasicBlock *RightReturnBB = Builder->GetInsertBlock();
    Builder->CreateBr(AfterBB);
    TheFunction->getBasicBlockList().push_back(AfterBB);
    Builder->SetInsertPoint(AfterBB);

    PHINode *PN = Builder->CreatePHI(Type::getInt32Ty(*TheContext), 2, "andres");

    PN->addIncoming(ConstantInt::get(*TheContext, APInt(32, 0, true)), LeftReturnBB);
    PN->addIncoming(Right, RightReturnBB);

    return PN;
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

    addConditionComparison(CondV);
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
    if(!Builder->GetInsertBlock()->getTerminator()) {
        Builder->CreateBr(CondBB);
    }

    // After
    TheFunction->getBasicBlockList().push_back(AfterBB);
    Builder->SetInsertPoint(AfterBB);

    // Pop scope
    VariableScope.popScope();
    return TheFunction;
}

Value *ReturnStmtASTNode::codegen() {
    Value *RetVal = Expr ? Expr->codegen() : nullptr;
    Function *F = Builder->GetInsertBlock()->getParent();

    if(!F) {
        SemanticError("Return statement outside of function", Tok, fileName);
    }

    // If there is a return value, check if it is the same type as the function (int, bool, float)
    // If there is no return value, check if the function is void
    if((!RetVal && Type::getVoidTy(*TheContext) != F->getReturnType()) || (RetVal && RetVal->getType() != F->getReturnType())) {
        std::string retValName = RetVal ? type_to_string(RetVal->getType()) : "void";
        SemanticError("Cannot return value of type `" + retValName + "` in function returning type `" + type_to_string(F->getReturnType()) + "`.", Tok, fileName);
    }

    Builder->CreateRet(RetVal);
    BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "afterreturn", F);
    Builder->SetInsertPoint(AfterBB);

    return RetVal;
}

#pragma endregion

#pragma region Declarations

Value *VariableDeclASTNode::codegen() {

    if(VariableScope.isGlobalScope()) {
        // Create default value for global (0)
        Constant *consta = Type == VariableType::FLOAT ? (Constant *)ConstantFP::get(*TheContext, APFloat(0.0f)) : (Constant *)ConstantInt::get(*TheContext, APInt(32, 0, true));
        GlobalVariable* g =
            new GlobalVariable(*TheModule, GetType(Type), false, GlobalValue::CommonLinkage, consta, Name);
        
        if(!VariableScope.addVariable(Name, g)) {
            SemanticError("Global variable `" + Name + "` has already been declared.", Tok, fileName);
        }
        return g;
    } else {
        Function *TheFunction = Builder->GetInsertBlock()->getParent();
        AllocaInst *Alloca = CreateAllocaArg(TheFunction, Name, GetType(Type));

        if(!VariableScope.addVariable(Name, Alloca)) {
            SemanticError("Variable `" + Name + "` already declared in this scope.", Tok, fileName);
        }
        
        return Alloca;
    }
}

Value *FunctionDeclASTNode::codegen() {
    std::vector<Type *> ArgsTypes;

    // Add all the argument types to the function
    for(auto &arg : Args) {
        ArgsTypes.push_back(GetType(arg->getType()));
    }

    // Create the function prototype
    FunctionType *FT = FunctionType::get(GetType(ReturnType), ArgsTypes, false);

    if(TheModule->getFunction(Name)) {
        SemanticError("Function `" + Name + "` previously declared.", this->Tok, fileName);
    }

    Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

    // Set the names of the arguments
    for(auto &arg : F->args()) {
        arg.setName(Args[arg.getArgNo()]->getName());
    }

    // Function body
    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", F);
    Builder->SetInsertPoint(BB);


    // Create a new scope for the function
    VariableScope.pushScope();

    int i = 0;
    for(auto &arg: F->args()) {
        AllocaInst *Alloca = CreateAllocaArg(F, arg.getName().str(), ArgsTypes[i]);

        Builder->CreateStore(&arg, Alloca);
        VariableScope.addVariable(arg.getName().str(), Alloca);

        i++;
    }

    // Do the body's code generation
    Body->codegen();

    // Add default return value, respecting the return type
    if(!Builder->GetInsertBlock()->getTerminator()) {
        switch(F->getReturnType()->getTypeID()) {
            case Type::TypeID::FloatTyID:
                Builder->CreateRet(ConstantFP::get(*TheContext, APFloat(0.0f)));
                break;
            case Type::TypeID::IntegerTyID:
                Builder->CreateRet(ConstantInt::get(*TheContext, APInt(32, 0, true)));
                break;
            default:
                Builder->CreateRetVoid();
                break;
        }
    }

    VariableScope.popScope();

    // As creating new blocks upon returns may lead to blocks with no predecessors,
    // prune the functions basic block 'tree'
    EliminateUnreachableBlocks(*F);
        
    // Ensure the function is valid
    if(verifyFunction(*F, &llvm::errs())) {
        SemanticError("Function `" + Name + "` is invalid. See error output.", this->Tok, fileName);
    }


    return F;
}

Value *ExternFunctionDeclASTNode::codegen() {
    std::vector<Type *> ArgsTypes;

    for(auto &arg : Args) {
        ArgsTypes.push_back(GetType(arg->getType()));
    }

    FunctionType *FT = FunctionType::get(GetType(ReturnType), ArgsTypes, false);

    Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

    for(auto &arg : F->args()) {
        arg.setName(Args[arg.getArgNo()]->getName());
    }

    return F;
}

static void InitializeModule() {
  // Open a new context and module.
  TheContext = std::make_shared<LLVMContext>();
  TheModule = std::make_unique<Module>("mini-c", *TheContext);

  // Create a new builder for the module.
  Builder = std::make_shared<IRBuilder<>>(*TheContext);
  VariableCaster = std::make_unique<VariableCastManager>(VariableCastManager(Builder, TheContext));
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
