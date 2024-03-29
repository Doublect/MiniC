#ifndef LEXER_H
#define LEXER_H
    #include<stdio.h>
    #include<string>

    // The lexer returns one of these for known things.
    enum TOKEN_TYPE {

        IDENT = -1,        // [a-zA-Z_][a-zA-Z_0-9]*
        ASSIGN = int('='), // '='

        // delimiters
        LBRA = int('{'),  // left brace
        RBRA = int('}'),  // right brace
        LPAR = int('('),  // left parenthesis
        RPAR = int(')'),  // right parenthesis
        SC = int(';'),    // semicolon
        COMMA = int(','), // comma

        //DONE: changed order of lexer tokens
        // types
        VOID_TOK = -2,  // "void"
        BOOL_TOK = -3,  // "bool"
        INT_TOK = -4,   // "int"
        FLOAT_TOK = -5, // "float"

        // keywords
        EXTERN = -6,  // "extern"
        IF = -7,      // "if"
        ELSE = -8,    // "else"
        WHILE = -9,   // "while"
        RETURN = -10, // "return"
        TRUE   = -12,     // "true"
        FALSE   = -13,     // "false"

        // literals
        INT_LIT = -14,   // [0-9]+
        FLOAT_LIT = -15, // [0-9]+.[0-9]+
        BOOL_LIT = -16,  // "true" or "false" key words

        // logical operators
        AND = -17, // "&&"
        OR = -18,  // "||"

        // operators
        PLUS = int('+'),    // addition or unary plus
        MINUS = int('-'),   // substraction or unary negative
        ASTERIX = int('*'), // multiplication
        DIV = int('/'),     // division
        MOD = int('%'),     // modular
        NOT = int('!'),     // unary negation

        // comparison operators
        EQ = -19,      // equal
        NE = -20,      // not equal
        LE = -21,      // less than or equal to
        LT = int('<'), // less than
        GE = -23,      // greater than or equal to
        GT = int('>'), // greater than

        // special tokens
        EOF_TOK = 0, // signal end of file

        // invalid
        INVALID = -100 // signal invalid token
    };

    // TOKEN struct is used to keep track of information about a token
    struct TOKEN {
        int type = -100;
        std::string lexeme;
        int lineNo;
        int columnNo;
    };
    TOKEN gettok(FILE* pFile);

#endif