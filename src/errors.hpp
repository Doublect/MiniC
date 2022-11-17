#ifndef ERRORS_H
#define ERRORS_H
    #include <fstream>
    #include <iostream>
    #include <map>
    #include <string>

    #include "lexer.hpp"

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

            std::string pointer(int column) const {
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

            std::string warning(std::string msg) const {
                return "\033[1;33mwarning:\033[0m " + msg;
            }

            ErrorT(TOKEN tok) : lineNo(tok.lineNo), columnNo(tok.columnNo) {}
            ErrorT(int line, int col) : lineNo(line), columnNo(col) {}
        public:
            ErrorT() : message("") {}
            ErrorT(std::string msg): message(msg) {}
            
            ErrorT(std::string msg, TOKEN tok) : message(msg), lineNo(tok.lineNo), columnNo(tok.columnNo) {}
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

            virtual void print(const std::string& filename) {
                std::cout << msg(filename) << std::endl;
            }

            virtual std::string code_highlight(const std::string& filename) {
                if(lineNo) {
                    return padding(4) + file_line(filename, lineNo) + "\n" + padding(4) + pointer(columnNo);
                }

                return "";
            }

    };

    class ErrorC : public ErrorT {
        public:
            ErrorC(std::string msg, TOKEN tok) : ErrorT(msg, tok) {}

            std::string msg(const std::string& filename) override {
                return ErrorT::error(message + "\n" + ErrorT::code_highlight(filename));
            }
    };

    class WarningC : public ErrorT {
        public:
            WarningC(std::string msg, TOKEN tok) : ErrorT(msg, tok) {}

            std::string msg(const std::string& filename) override {
                return ErrorT::warning(message + "\n" + ErrorT::code_highlight(filename));
            }
    };

    class ExpectedTokenErrorT : public ErrorC {
        TOKEN_TYPE expected;
        TOKEN got;

        public:
            ExpectedTokenErrorT(TOKEN_TYPE expected, const TOKEN& got) 
                : expected(expected), got(got), 
                    ErrorC("Expected `" + token_type_to_string.at(expected) + "`, got " + token_type_to_string.at((TOKEN_TYPE)got.type), got) {}
    };

    class CustomExpectedTokenErrorT : public ErrorC {
        TOKEN got;
        std::string type;

        public:
            CustomExpectedTokenErrorT(std::string s, const TOKEN& got) 
                : type(s), got(got), 
                    ErrorC("Expected `" + s + "`, got " + token_type_to_string.at((TOKEN_TYPE)got.type), got) {}
    };

    class SemanticError : public ErrorC {

    public:
        SemanticError(std::string msg, TOKEN tok, std::string fileName) : ErrorC(msg, tok) {
            print(fileName);
            exit(1);
        }
    };

    class SemanticWarning : public WarningC {

    public:
        SemanticWarning(std::string msg, TOKEN tok) : WarningC(msg, tok) {}
        SemanticWarning(std::string msg, TOKEN tok, std::string fileName) : WarningC(msg, tok) {
            print(fileName);
        }
    };

#endif