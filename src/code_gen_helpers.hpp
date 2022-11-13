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
            return Builder->CreateFPToSI(V, Type::getInt32Ty(*TheContext), "inttmp");
        }

        Value *ensureFloat(Value *V)
        {
            if (V->getType()->isFloatTy()) {
                return V;
            }

            V = ensureInteger(V);
            return Builder->CreateSIToFP(V, Type::getFloatTy(*TheContext), "floattmp");
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
#endif