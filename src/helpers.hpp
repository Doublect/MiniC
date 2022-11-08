#ifndef HELPERS_H
#define HELPERS_H
    #include <memory>
    #include <type_traits>
    #include <iostream>

    #include "lexer.hpp"

    enum TypeSpecType {
        VOID = -2,
        BOOL = -3,
        INT = -4,
        FLOAT = -5,
    };

    enum class VariableType {
        BOOL = TypeSpecType::BOOL,
        INT = TypeSpecType::INT,
        FLOAT = TypeSpecType::FLOAT,
    };
    template <typename Base, typename Derived> std::unique_ptr<Base> unique_ptr_cast(Derived &&p) {
        return std::unique_ptr<Base>(std::make_unique<Derived>(std::move(p)));
    }
    template <typename Base, typename Derived> constexpr std::shared_ptr<Base> shared_ptr_cast(Derived &&p) {
        return std::shared_ptr<Base>(std::make_shared<Derived>(std::move(p)));
    }

    class ErrorT {
        std::string message;
        
        public:
            ErrorT() : message("") {} //std::cout << "Empty error" << std::endl;
            ErrorT(std::string msg): message(msg) {}
            
            ErrorT(std::string msg, TOKEN tok) {
                ErrorT(msg, tok.lineNo, tok.columnNo);
            }
            ErrorT(std::string msg, int line, int col): message(msg) {
                message = msg + "\n" + std::to_string(line) + ":" + std::to_string(col);
            }

            std::string msg() {
                return message;
            }

    };


    template<typename T> class ResultMonad {
        std::unique_ptr<T> val;
        ErrorT err = ErrorT();
        bool success_val = false;

        public:
            ResultMonad(ResultMonad<T> &&other) {
                std::cout << "Move constructor" << std::endl;
                val = std::move(other.val);
                err = other.err;
                success_val = other.success_val;
            }
            ResultMonad(std::unique_ptr<T> &&val) : val(std::move(val)), success_val(true) {}
            //ResultMonad(T val) : val(std::make_unique<T>(std::move(val))), success_val(true) {}
            ResultMonad(ErrorT err) : err(err) {}
            ResultMonad(ErrorT err, bool b) : err(err) {}

            template <class U = T>
                requires(std::conjunction_v<std::is_convertible<T, U>>)
            constexpr ResultMonad& operator=(U&& v) {
                if (success_val)
                    val = std::move<U>(v);
                else {
                    err = std::move<U>(v);
                }

                return *this;
            }

            constexpr const std::unique_ptr<T>&& unwrap() const&& {
                return std::move(val);
            }

            constexpr std::unique_ptr<T>&& unwrap() && {
                return std::move(val);
            }

            T* unwrap_val() {
                return std::move(val.release());
            }

            constexpr ErrorT& error() & {
                return err;
            }
            constexpr ErrorT&& error() && {
                return std::move(err);
            }
            constexpr const ErrorT&& error() const&& {
                return std::move(err);
            }

            const bool success() {
                return success_val;
            }

            template<typename U>
                requires(std::conjunction_v<std::is_convertible<T, U>>)
            constexpr operator ResultMonad<U>() {
                std::cout << "Converted A" << std::endl;
                if(this->success_val) {
                    ResultMonad<U> res(std::move(val));
                    return std::move(res);
                } else {
                    ResultMonad<U> res(err);
                    return std::move(res);
                }
            }

            template<typename U>
            constexpr operator ResultMonad<U>() {
                ResultMonad<U> res(err, false);
                std::cout << "Converted B: " << typeid(T).name() << " " << typeid(U).name()  << std::endl;
                return std::move(res);
            }
    };

    // template<typename T>
    // ResultMonad<T> make_result(T&& val) {
    //     return ResultMonad<T>(std::make_unique<T>(std::move(val)));
    // }

    template<typename T, typename U>
        requires(std::conjunction_v<std::is_convertible<T, U>>)
    ResultMonad<T> make_result(U&& val) {
        return ResultMonad<T>(std::make_unique(std::move(val)));
    }

    template<typename T>
    ResultMonad<T> make_result_ptr(std::unique_ptr<T>&& val) {
        return ResultMonad<T>(std::move(val));
    }

    TypeSpecType least_constraining_value(TypeSpecType a, TypeSpecType b);

    template<typename T> 
        requires (std::is_void_v<T>)
    class ResultMonad<T> {

    };
#endif