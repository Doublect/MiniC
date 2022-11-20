#ifndef CODE_GEN_HELPERS_H
#define CODE_GEN_HELPERS_H
    #include "llvm/IR/GlobalVariable.h"
    #include "llvm/IR/Instructions.h"
    #include "llvm/IR/IRBuilder.h"
    #include "llvm/IR/Value.h"


    #include<map>
    #include<memory>
    #include<optional>
    #include<ostream>
    #include<stack>
    #include<set>
    #include<tuple>

    #include "errors.hpp"
    #include "helpers.hpp"

    using namespace llvm;

    extern std::string fileName;

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

        bool addVariable(const std::string &Name, AllocaInst *Alloca) {
            if(ScopeStack.top().contains(Name)) {
                return false;
            }

            NamedValues[Name].push(Alloca);
            ScopeStack.top().insert(Name);

            return true;
        }

        bool addVariable(const std::string &Name, GlobalVariable *Global) {
            if(GlobalVariables.contains(Name)) {
                return false;
            }

            GlobalVariables[Name] = Global;
            return true;
        }

        std::optional<std::tuple<Value *, Type *>>getVariable(const std::string &Name) {
            print_a_debug("Searching for variable: " + Name);

            if(NamedValues.contains(Name) && NamedValues[Name].size() > 0) {
                return std::make_optional(std::make_tuple(NamedValues[Name].top(), NamedValues[Name].top()->getAllocatedType()));
            }

            if(GlobalVariables.contains(Name)) {
                return std::make_optional(std::make_tuple(GlobalVariables[Name], GlobalVariables[Name]->getValueType()));
            }
            // TODO: Error
            return std::nullopt;
        }

        bool isGlobalVariableDeclared(const std::string &Name) {
            return GlobalVariables.contains(Name);
        }
    };

    // Responsible for finding and casting to the correct type
    // The lifetime of caster is the same as the lifetime of the builder, therefore we can store a shared_ptr to it.
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

            throw std::runtime_error("Cannot upcast float to integer");
        }

        Value *ensureFloat(Value *V)
        {
            if (V->getType()->isFloatTy()) {
                return V;
            }

            V = ensureInteger(V);
            return Builder->CreateSIToFP(V, Type::getFloatTy(*TheContext), "floattmp");
        }

        // Ensure that the value is of the correct type, does not allow lossy casts
        Value *ensureType(Value *V, Type::TypeID type) {
            if (V->getType()->getTypeID() == type) {
                return V;
            }

            if (type == Type::IntegerTyID) {
                return ensureInteger(V);
            }

            return ensureFloat(V);
        }

        // Ensure that the value is of the correct type, allows for lossy casts (FP to Int)
        Value *ensureParamType(Value *V, Type *type) {
            if (V->getType() == type) {
                return V;
            }

            if (type->isIntegerTy()) {
                // V is float, but param is int/bool (lossy cast)
                if (V->getType() == Type::getFloatTy(*TheContext)) {
                    return Builder->CreateFPToSI(V, type, "float_to_int");
                }

                return ensureInteger(V);
            }

            return ensureFloat(V);
        }

        std::tuple<Value *, Value *, Type::TypeID> ensureSharedType(Value *L, Value *R) {
            // bool == int < float
            Type::TypeID typeID = std::min(L->getType()->getTypeID(), R->getType()->getTypeID());
            
            return std::make_tuple(ensureType(L, typeID), ensureType(R, typeID), typeID); 
        }
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