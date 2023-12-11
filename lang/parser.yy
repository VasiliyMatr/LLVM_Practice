%require "3.2"

%language "C++"
%skeleton "lalr1.cc"

%define api.value.type variant

%param { yy::Driver* driver }

%code requires {
#include <string>

#include <lang/ast.hpp>

namespace yy { class Driver; }

}

%code {
#include <lang/driver.hpp>

namespace yy {

parser::token_type yylex(parser::semantic_type *yylval, Driver *driver);

} // namespace yy

}

%token
    ROUND_BR_OPEN
    ROUND_BR_CLOSE
    PLUS
    MINUS
    ASTERISK
    SLASH
    LESS
    LESS_EQUAL
    GREATER
    GREATER_EQUAL
    EQUAL
    NOT_EQUAL
    ASSIGN

    SEMICOLON
;

%token <const lang::ASTNode::Id *> ID;

%token <const lang::ASTNode::IntVal *> INT_VAL;

%token <const lang::ASTNode::FixedVal *> FIXED_VAL;

%nterm <const lang::InterfaceASTNode *>
    AST
    Expression
    Compare
    AddSub
    MulDiv
    Unary
    ExprBr
    Value
;

%%

AST:
    Expression SEMICOLON { driver->setASTRoot($1); YYACCEPT; }
;

Expression:
    Compare { $$ = $1; }
    | ID ASSIGN Expression { $$ = driver->createAssign($1, $3); }
;

Compare:
    AddSub { $$ = $1; }
    | Compare LESS AddSub { $$ = driver->createBinOp(Driver::BinaryOpType::CMP_LESS, $1, $3); }
    | Compare LESS_EQUAL AddSub { $$ = driver->createBinOp(Driver::BinaryOpType::CMP_LESS_EQUAL, $1, $3); }
    | Compare GREATER AddSub { $$ = driver->createBinOp(Driver::BinaryOpType::CMP_GREATER, $1, $3); }
    | Compare GREATER_EQUAL AddSub { $$ = driver->createBinOp(Driver::BinaryOpType::CMP_GREATER_EQUAL, $1, $3); }
    | Compare EQUAL AddSub { $$ = driver->createBinOp(Driver::BinaryOpType::CMP_EQUAL, $1, $3); }
    | Compare NOT_EQUAL AddSub { $$ = driver->createBinOp(Driver::BinaryOpType::CMP_NOT_EQUAL, $1, $3); }
;

AddSub:
    MulDiv { $$ = $1; }
    | AddSub PLUS MulDiv { $$ = driver->createBinOp(Driver::BinaryOpType::ADD, $1, $3); }
    | AddSub MINUS MulDiv { $$ = driver->createBinOp(Driver::BinaryOpType::SUB, $1, $3); }
;

MulDiv:
    Unary { $$ = $1; }
    | MulDiv ASTERISK Unary { $$ = driver->createBinOp(Driver::BinaryOpType::MUL, $1, $3); }
    | MulDiv SLASH Unary { $$ = driver->createBinOp(Driver::BinaryOpType::DIV, $1, $3); }
;

Unary:
    ExprBr { $$ = $1; }
    | PLUS ExprBr { $$ = driver->createUnOp(Driver::UnaryOpType::UN_PLUS, $2); }
    | MINUS ExprBr { $$ = driver->createUnOp(Driver::UnaryOpType::UN_MINUS, $2); }
;

ExprBr:
    Value { $$ = $1; }
    | ROUND_BR_OPEN Expression ROUND_BR_CLOSE { $$ = $2; }
;

Value:
    INT_VAL { $$ = $1; }
    | FIXED_VAL { $$ = $1; }
    | ID { $$ = $1; }
;

%%

namespace yy {

parser::token_type yylex(parser::semantic_type *yylval, Driver *driver) {
    return driver->yylex(yylval);
}

void yy::parser::error(const std::string &error) {
    std::cerr << "Parsing error: " << error << std::endl;
    std::terminate();
}

}
