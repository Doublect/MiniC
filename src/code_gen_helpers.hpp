#ifndef CODE_GEN_HELPERS_H
#define CODE_GEN_HELPERS_H
    #include "llvm/IR/GlobalVariable.h"
    #include "llvm/IR/Instructions.h"
    #include "llvm/IR/IRBuilder.h"
    #include "llvm/IR/Value.h"


    #include<map>
    #include<memory>
    #include<ostream>
    #include<stack>
    #include<set>
    #include<tuple>

    #include "helpers.hpp"

    using namespace llvm;

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

        std::tuple<Value *, Type *>getVariable(const std::string &Name) {
            print_a_debug("Searching for variable: " + Name);

            if(NamedValues.contains(Name) && NamedValues[Name].size() > 0) {
                return std::make_tuple(NamedValues[Name].top(), NamedValues[Name].top()->getAllocatedType());
            }

            if(GlobalVariables.contains(Name)) {
                return std::make_tuple(GlobalVariables[Name], GlobalVariables[Name]->getValueType());
            }
            // TODO: Error
            throw std::runtime_error("Variable not found: " + Name);
        }

        bool isGlobalVariableDeclared(const std::string &Name) {
            return GlobalVariables.contains(Name);
        }
    };

    // The lifetime of caster is the same as the lifetime of the builder.
    class VariableCastManager {
        std::shared_ptr<IRBuilder<>> Builder;
        std::shared_ptr<LLVMContext> TheContext;

    public:
        VariableCastManager(std::shared_ptr<IRBuilder<>> Builder, std::shared_ptr<LLVMContext> Context) 
            : Builder(Builder), TheContext(Context) {}

        Value *ensureInteger(Value *V) 
        {
            if (V->getType()->isIntegerTy()) {
                if(V->getType()->getIntegerBitWidth() == 1) {
                    return Builder->CreateZExt(V, Type::getInt32Ty(*TheContext), "bool_to_int");
                }
                return V;
            }
            // TODO: disallow
            throw std::runtime_error("Cannot upcast non-integer to integer");
        }

        Value *ensureFloat(Value *V)
        {
            if (V->getType()->isFloatTy()) {
                return V;
            }

            V = ensureInteger(V);
            return Builder->CreateSIToFP(V, Type::getFloatTy(*TheContext), "floattmp");
        }

        Value *ensureType(Value *V, Type::TypeID type) {
            if (V->getType()->getTypeID() == type) {
                return V;
            }

            if (type == Type::IntegerTyID) {
                return ensureInteger(V);
            }

            return ensureFloat(V);
        }

        std::tuple<Value *, Value *, Type::TypeID> ensureSharedType(Value *L, Value *R) {
            // bool == int < float
            Type::TypeID typeID = std::min(L->getType()->getTypeID(), R->getType()->getTypeID());
            
            return std::make_tuple(ensureType(L, typeID), ensureType(R, typeID), typeID); 
        }

        Value *narrowingCast(Value *L, Type *R) {
            // widening cast
            if(R->getTypeID() > L->getType()->getTypeID()) {
                return ensureType(L, R->getTypeID());
            }

            // narrowing cast
            if(R->getTypeID() == Type::TypeID::FloatTyID) {
                return L;
            }

            if(L->getType()->isFloatTy()) {
                L = Builder->CreateFPToSI(L, Type::getInt32Ty(*TheContext), "float_to_int32");
            }

            if(R->getIntegerBitWidth() == 32) {
                return L;
            }

            return Builder->CreateICmpNE(L, ConstantInt::get(*TheContext, APInt(32, 0, true)), "int32_to_bool");
        }
    };

    class SemanticProblem {
        std::string msg;
        int LineNo, ColumnNo;

    public:
        SemanticProblem(std::string msg, int LineNo, int ColumnNo) : msg(msg), LineNo(LineNo), ColumnNo(ColumnNo) {
            print();
        }
        SemanticProblem(std::string msg, TOKEN tok) : msg(msg), LineNo(tok.lineNo), ColumnNo(tok.columnNo) {
            print();
        }

        virtual void print() {
            std::cout << "Semantic error at line " << LineNo << ", column " << ColumnNo << ": " << msg << std::endl;
        }
    };


    class SemanticError {
        SemanticProblem *problem;

    public:
        SemanticError(std::string msg, int LineNo, int ColumnNo) : problem(new SemanticProblem(msg, LineNo, ColumnNo)) {
            exit(1);
        }
        SemanticError(std::string msg, TOKEN tok) : problem(new SemanticProblem(msg, tok)) {
            exit(1);
        }
    };

    class SemanticWarning {
        SemanticProblem *problem;

    public:
        SemanticWarning(std::string msg, int LineNo, int ColumnNo) : problem(new SemanticProblem(msg, LineNo, ColumnNo)) {}
        SemanticWarning(std::string msg, TOKEN tok) : problem(new SemanticProblem(msg, tok)) {}
    };

    constexpr std::string type_to_string(Type *type) {
        if(type->isFloatTy()) {
            return "float";
        }
        if(type->isIntegerTy()) {
            if(type->getIntegerBitWidth() == 1) {
                return "bool";
            }

            return "int";
        }

        throw std::runtime_error("Unexpected type");
    }
#endif