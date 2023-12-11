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
    CURLY_BR_OPEN
    CURLY_BR_CLOSE

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

    INT_TYPE
    FIXED_TYPE

    IF
;

%token <lang::ASTNode::Id *> ID;

%token <lang::ASTNode::IntVal *> INT_VAL;

%token <lang::ASTNode::FixedVal *> FIXED_VAL;

%nterm <lang::InterfaceASTNode *>
    AST
    Expression
    Compare
    AddSub
    MulDiv
    Unary
    ExprBr
    Value
;

%nterm <lang::InterfaceStatement *>
    Statement
    IfStatement
    VarDef
    ExprStatement
;

%%

AST:
    Statement { driver->setASTRoot($1); YYACCEPT; }
;

Statement:
    IfStatement Statement { $1->append($2); $$ = $1; }
    | VarDef Statement { $1->append($2); $$ = $1; }
    | ExprStatement Statement { $1->append($2); $$ = $1; }
    | %empty { $$ = nullptr; }

IfStatement:
    IF ROUND_BR_OPEN Expression ROUND_BR_CLOSE CURLY_BR_OPEN Statement CURLY_BR_CLOSE
        { $$ = driver->create<lang::ASTNode::IfStatement>($3, $6); }

VarDef:
    INT_TYPE ID ASSIGN Expression SEMICOLON
        { $$ = driver->create<lang::ASTNode::VarDef>(lang::ASTNode::VarType::INT, $2, $4); }
    | FIXED_TYPE ID ASSIGN Expression SEMICOLON
        { $$ = driver->create<lang::ASTNode::VarDef>(lang::ASTNode::VarType::FIXED, $2, $4); }

ExprStatement:
    Expression SEMICOLON { $$ = driver->create<lang::ASTNode::ExprStatement>($1); }

Expression:
    Compare { $$ = $1; }
    | ID ASSIGN Expression { $$ = driver->create<lang::ASTNode::Assign>($1, $3); }
;

Compare:
    AddSub { $$ = $1; }
    | Compare LESS AddSub
        { $$ = driver->create<lang::ASTNode::BinaryOp>(lang::ASTNode::BinOpType::CMP_LESS, $1, $3); }
    | Compare LESS_EQUAL AddSub
        { $$ = driver->create<lang::ASTNode::BinaryOp>(lang::ASTNode::BinOpType::CMP_LESS_EQUAL, $1, $3); }
    | Compare GREATER AddSub
        { $$ = driver->create<lang::ASTNode::BinaryOp>(lang::ASTNode::BinOpType::CMP_GREATER, $1, $3); }
    | Compare GREATER_EQUAL AddSub
        { $$ = driver->create<lang::ASTNode::BinaryOp>(lang::ASTNode::BinOpType::CMP_GREATER_EQUAL, $1, $3); }
    | Compare EQUAL AddSub
        { $$ = driver->create<lang::ASTNode::BinaryOp>(lang::ASTNode::BinOpType::CMP_EQUAL, $1, $3); }
    | Compare NOT_EQUAL AddSub
        { $$ = driver->create<lang::ASTNode::BinaryOp>(lang::ASTNode::BinOpType::CMP_NOT_EQUAL, $1, $3); }
;

AddSub:
    MulDiv { $$ = $1; }
    | AddSub PLUS MulDiv
        { $$ = driver->create<lang::ASTNode::BinaryOp>(lang::ASTNode::BinOpType::ADD, $1, $3); }
    | AddSub MINUS MulDiv
        { $$ = driver->create<lang::ASTNode::BinaryOp>(lang::ASTNode::BinOpType::SUB, $1, $3); }
;

MulDiv:
    Unary { $$ = $1; }
    | MulDiv ASTERISK Unary
        { $$ = driver->create<lang::ASTNode::BinaryOp>(lang::ASTNode::BinOpType::MUL, $1, $3); }
    | MulDiv SLASH Unary
        { $$ = driver->create<lang::ASTNode::BinaryOp>(lang::ASTNode::BinOpType::DIV, $1, $3); }
;

Unary:
    ExprBr { $$ = $1; }
    | PLUS ExprBr
        { $$ = driver->create<lang::ASTNode::UnaryOp>(lang::ASTNode::UnOpType::UN_PLUS, $2); }
    | MINUS ExprBr
        { $$ = driver->create<lang::ASTNode::UnaryOp>(lang::ASTNode::UnOpType::UN_MINUS, $2); }
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
