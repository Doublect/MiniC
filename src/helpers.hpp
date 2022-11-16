#ifndef HELPERS_H
#define HELPERS_H
    #include <fstream>
    #include <iostream>
    #include <map>
    #include <memory>
    #include <string>
    #include <type_traits>

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

    const std::map<TOKEN_TYPE, std::string> token_type_to_string{
        {TOKEN_TYPE::IDENT, "identifier"},
        {TOKEN_TYPE::ASSIGN, "="},
        {TOKEN_TYPE::LBRA, "{"},
        {TOKEN_TYPE::RBRA, "}"},
        {TOKEN_TYPE::LPAR, "("},
        {TOKEN_TYPE::RPAR, ")"},
        {TOKEN_TYPE::SC, ";"},
        {TOKEN_TYPE::COMMA, ","},

        {TOKEN_TYPE::VOID_TOK, "void"},
        {TOKEN_TYPE::BOOL_TOK, "bool"},
        {TOKEN_TYPE::INT_TOK, "int"},
        {TOKEN_TYPE::FLOAT_TOK, "float"},

        {TOKEN_TYPE::EXTERN, "extern"},
        {TOKEN_TYPE::IF, "if"},
        {TOKEN_TYPE::ELSE, "else"},
        {TOKEN_TYPE::WHILE, "while"},
        {TOKEN_TYPE::RETURN, "return"},

        {TOKEN_TYPE::TRUE, "true"},
        {TOKEN_TYPE::FALSE, "false"},

        {TOKEN_TYPE::INT_LIT, "integer literal"},
        {TOKEN_TYPE::FLOAT_LIT, "float literal"},
        {TOKEN_TYPE::BOOL_LIT, "bool literal"},

        {TOKEN_TYPE::AND, "&&"},
        {TOKEN_TYPE::OR, "||"},

        {TOKEN_TYPE::PLUS, "+"},
        {TOKEN_TYPE::MINUS, "-"},
        {TOKEN_TYPE::ASTERIX, "*"},
        {TOKEN_TYPE::DIV, "/"},
        {TOKEN_TYPE::MOD, "%"},
        {TOKEN_TYPE::NOT, "!"},

        {TOKEN_TYPE::EQ, "=="},
        {TOKEN_TYPE::NE, "!="},
        {TOKEN_TYPE::LE, "<="},
        {TOKEN_TYPE::LT, "<"},
        {TOKEN_TYPE::GE, ">="},
        {TOKEN_TYPE::GT, ">"},

        {TOKEN_TYPE::EOF_TOK, "<end of file>"},

        {TOKEN_TYPE::INVALID, "<invalid token>"},
    };

    class ErrorT {
        protected:
            std::string message;
            int lineNo, columnNo;

            std::string padding(int n) {
                std::string s = "";
                for (int i = 0; i < n; i++) {
                    s += " ";
                }
                return s;
            }

            std::string file_line(const std::string& filename, int line) {
                std::ifstream f(filename);
                std::string contents;
                while (line > 0) {
                    std::getline(f, contents);
                    line--;
                }

                return contents;
            }

            std::string pointer(int column) {
                std::string pointer;
                for (int i = 1; i < column; i++) {
                    pointer += " ";
                }
                pointer += "^\n";
                return pointer;
            }

            std::string error(std::string msg) const {
                return "\033[1;31merror:\033[0m " + msg;
            }

            ErrorT(TOKEN tok) : lineNo(tok.lineNo), columnNo(tok.columnNo) {}
            ErrorT(int line, int col) : lineNo(line), columnNo(col) {}
        public:
            ErrorT() : message("") {} //std::cout << "Empty error" << std::endl;
            ErrorT(std::string msg): message(msg) {}
            
            ErrorT(std::string msg, TOKEN tok) : message(msg), lineNo(tok.lineNo), columnNo(tok.columnNo) {std::cout << "ErrorT: " << tok.lineNo << std::endl;}
            ErrorT(std::string msg, int line, int col) : message(msg), lineNo(line), columnNo(col) {}

            virtual std::string msg() {
                return message;
            }

            virtual std::string msg(const std::string& filename) {
                if(lineNo) {
                    return message + "\n" + code_highlight(filename);
                }

                return message;
            }

            virtual std::string code_highlight(const std::string& filename) {
                if(lineNo) {
                    return padding(4) + file_line(filename, lineNo) + "\n" + padding(4) + pointer(columnNo);
                }

                return "";
            }

    };

    class ExpectedTokenErrorT : public ErrorT {
        TOKEN_TYPE expected;
        TOKEN got;

        public:
            ExpectedTokenErrorT(TOKEN_TYPE expected, const TOKEN& got) : expected(expected), got(got), ErrorT(got) {}

            std::string msg(const std::string& filename) override {
                std::string s = "Expected `" + token_type_to_string.at(expected) + "`, got " + token_type_to_string.at((TOKEN_TYPE)got.type);
                s += "\n" + code_highlight(filename);
                return error(s);
            }
    };

    class CustomExpectedTokenErrorT : public ErrorT {
        TOKEN got;
        std::string type;

        public:
            CustomExpectedTokenErrorT(std::string s, const TOKEN& got) : type(s), got(got), ErrorT(got) {}

            std::string msg(const std::string& filename) override {
                std::string s = "Expected `" + type + "`, got " + token_type_to_string.at((TOKEN_TYPE)got.type);
                s += "\n" + code_highlight(filename);
                return error(s);
            }
    };

    template<typename T> class ResultMonad {
        std::unique_ptr<T> val;
        std::unique_ptr<ErrorT> err = std::make_unique<ErrorT>(ErrorT());
        bool success_val = false;

        public:
            ResultMonad(ResultMonad<T> &&other) {
                val = std::move(other.val);
                err = std::move(other.err);
                success_val = other.success_val;
            }

            template<typename U> 
                requires(std::is_convertible_v<U*, T*>)
            ResultMonad(ResultMonad<U> &&other) {
                err = std::move(other.error());
                success_val = other.success();
                val = std::move(other).unwrap();
            }

            template<typename U> 
            ResultMonad(ResultMonad<U> &&other) {
                if(other.success()) {
                    std::cout << "Warning: Move constructor can only convert from a ResultMonad<T> to a ResultMonad<U> if T is convertible to U." << std::endl;
                }
                err = std::move(other.error());
                success_val = false;
            }
            ResultMonad(std::unique_ptr<T> &&val) : val(std::move(val)), success_val(true) {}
            ResultMonad(std::unique_ptr<ErrorT> err) : err(std::move(err)) {}

            constexpr const std::unique_ptr<T>&& unwrap() const&& {
                return std::move(val);
            }

            constexpr std::unique_ptr<T>&& unwrap() && {
                return std::move(val);
            }

            T* unwrap_val() {
                return std::move(val.release());
            }

            constexpr std::unique_ptr<ErrorT>& error() & {
                return err;
            }
            constexpr std::unique_ptr<ErrorT>&& error() && {
                return std::move(err);
            }
            constexpr const std::unique_ptr<ErrorT>&& error() const&& {
                return std::move(err);
            }

            const bool success() {
                return success_val;
            }

            template<typename U>
                requires(std::is_convertible_v<U*, T*>)
            ResultMonad<T> operator=(ResultMonad<U> &&other) {
                if (other.success()) {
                    val = std::move(other.unwrap());
                    success_val = true;
                } else {
                    err = other.error();
                    success_val = false;
                }

                return this;
            }
    };

    template<typename T>
    ResultMonad<T> make_result(T&& val) {
        return ResultMonad<T>(std::make_unique<T>(std::move(val)));
    }

    template<typename T>
    ResultMonad<T> make_result(std::unique_ptr<ErrorT>&& err) {
        return ResultMonad<T>(std::move(err));
    }

    template<typename T>
    ResultMonad<T> make_result(ErrorT&& err) {
        return ResultMonad<T>(std::make_unique<ErrorT>(err));
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

    //#define DEBUG_MODE
    #ifdef DEBUG_MODE
        constexpr void print_a_debug(const std::string& msg) {
            std::cout << msg << std::endl;
        }
    #else
        constexpr void print_a_debug(const std::string& msg) {
            // Do nothing
        }
    #endif
#endif
