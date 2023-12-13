%require "3.2"

%language "C++"
%skeleton "lalr1.cc"

%define api.value.type variant

%define parse.trace
%define parse.lac full

%define parse.error detailed
%locations

%param { yy::Driver* driver }

%code requires {
#include <string>

#include <lang/ast.hpp>

namespace yy { class Driver; }

}

%code {
#include <lang/driver.hpp>

namespace yy {

parser::token_type yylex(parser::semantic_type *yylval, parser::location_type *yylloc, Driver *driver);

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
    COMMA

    INT_TYPE
    FIXED_TYPE

    IF
    WHILE
    RETURN
;

%token <std::string> ID;

%token <lang::ast::node::IntVal *> INT_VAL;

%token <lang::ast::node::FixedVal *> FIXED_VAL;

%nterm <lang::ast::VarType> VarType;

%nterm <lang::ast::BinOpType> CmpOp;

%nterm <lang::ast::ChainNode *>
    AST
    GlobDef
    Statement
;

%nterm <lang::ast::node::VarDef *> VarDef;
%nterm <lang::ast::node::FuncDef *> FuncDef;
%nterm <lang::ast::node::FuncArg *> FuncArg;
%nterm <lang::ast::node::FuncArg *> FuncArgNonEmpty;

%nterm <lang::ast::node::Return *> Return;
%nterm <lang::ast::node::WhileStatement *> WhileStatement;
%nterm <lang::ast::node::IfStatement *> IfStatement;
%nterm <lang::ast::node::ExprStatement *> ExprStatement;

%nterm <lang::ast::node::CallArg *> CallArg;
%nterm <lang::ast::node::CallArg *> CallArgNonEmpty;

%nterm <lang::ast::InterfaceExpr *>
    Expression
    Compare
    AddSub
    MulDiv
    Unary
    ExprBr
    Value
    Call
;

%%

AST:
    GlobDef { driver->setASTRoot($1); YYACCEPT; }
;

GlobDef:
    FuncDef GlobDef { $1->append($2); $$ = $1; } |
    VarDef GlobDef { $1->append($2); $$ = $1; } |
    %empty { $$ = nullptr; }
;

FuncDef:
    VarType ID ROUND_BR_OPEN FuncArg ROUND_BR_CLOSE CURLY_BR_OPEN Statement CURLY_BR_CLOSE
        { $$ = driver->create<lang::ast::node::FuncDef>($1, $2, $4, $7); }
;

FuncArg:
    FuncArgNonEmpty { $$ = $1; } |
    %empty { $$ = nullptr; }
;

FuncArgNonEmpty:
    VarType ID COMMA FuncArgNonEmpty
        { $$ = driver->create<lang::ast::node::FuncArg>($1, $2); $$->append($4); } |
    VarType ID
        { $$ = driver->create<lang::ast::node::FuncArg>($1, $2); }
;

Statement:
    Return Statement { $1->append($2); $$ = $1; } |
    WhileStatement Statement { $1->append($2); $$ = $1; } |
    IfStatement Statement { $1->append($2); $$ = $1; } |
    VarDef Statement { $1->append($2); $$ = $1; } |
    ExprStatement Statement { $1->append($2); $$ = $1; } |
    %empty { $$ = nullptr; }
;

Return:
    RETURN Expression SEMICOLON { $$ = driver->create<lang::ast::node::Return>($2); }
;

WhileStatement:
    WHILE ROUND_BR_OPEN Expression ROUND_BR_CLOSE CURLY_BR_OPEN Statement CURLY_BR_CLOSE
        { $$ = driver->create<lang::ast::node::WhileStatement>($3, $6); }
;

IfStatement:
    IF ROUND_BR_OPEN Expression ROUND_BR_CLOSE CURLY_BR_OPEN Statement CURLY_BR_CLOSE
        { $$ = driver->create<lang::ast::node::IfStatement>($3, $6); }
;

VarDef:
    VarType ID ASSIGN Expression SEMICOLON
        { $$ = driver->create<lang::ast::node::VarDef>($1, $2, $4); }
;

VarType:
    INT_TYPE { $$ = lang::ast::VarType::INT; } |
    FIXED_TYPE { $$ = lang::ast::VarType::FIXED; }
;

ExprStatement:
    Expression SEMICOLON { $$ = driver->create<lang::ast::node::ExprStatement>($1); }
;

Expression:
    Compare { $$ = $1; } |
    ID ASSIGN Expression { $$ = driver->create<lang::ast::node::Assign>($1, $3); }
;

Compare:
    AddSub { $$ = $1; } |
    Compare CmpOp AddSub { $$ = driver->create<lang::ast::node::BinaryOp>($2, $1, $3); }
;

CmpOp:
    LESS { $$ = lang::ast::BinOpType::CMP_LESS; } |
    LESS_EQUAL { $$ = lang::ast::BinOpType::CMP_LESS_EQUAL; } |
    GREATER { $$ = lang::ast::BinOpType::CMP_GREATER; } |
    GREATER_EQUAL { $$ = lang::ast::BinOpType::CMP_GREATER_EQUAL; } |
    EQUAL { $$ = lang::ast::BinOpType::CMP_EQUAL; } |
    NOT_EQUAL { $$ = lang::ast::BinOpType::CMP_NOT_EQUAL; }
;

AddSub:
    MulDiv { $$ = $1; } |
    AddSub PLUS MulDiv
        { $$ = driver->create<lang::ast::node::BinaryOp>(lang::ast::BinOpType::ADD, $1, $3); } |
    AddSub MINUS MulDiv
        { $$ = driver->create<lang::ast::node::BinaryOp>(lang::ast::BinOpType::SUB, $1, $3); }
;

MulDiv:
    Unary { $$ = $1; } |
    MulDiv ASTERISK Unary
        { $$ = driver->create<lang::ast::node::BinaryOp>(lang::ast::BinOpType::MUL, $1, $3); } |
    MulDiv SLASH Unary
        { $$ = driver->create<lang::ast::node::BinaryOp>(lang::ast::BinOpType::DIV, $1, $3); }
;

Unary:
    ExprBr { $$ = $1; } |
    PLUS ExprBr
        { $$ = driver->create<lang::ast::node::UnaryOp>(lang::ast::UnOpType::UN_PLUS, $2); } |
    MINUS ExprBr
        { $$ = driver->create<lang::ast::node::UnaryOp>(lang::ast::UnOpType::UN_MINUS, $2); }
;

ExprBr:
    Value { $$ = $1; } |
    ROUND_BR_OPEN Expression ROUND_BR_CLOSE { $$ = $2; }
;

Value:
    Call { $$ = $1; } |
    INT_VAL { $$ = $1; } |
    FIXED_VAL { $$ = $1; } |
    ID { $$ = driver->create<lang::ast::node::VarVal>($1); }
;

Call:
    ID ROUND_BR_OPEN CallArg ROUND_BR_CLOSE
        { $$ = driver->create<lang::ast::node::Call>($1, $3); }
;

CallArg:
    CallArgNonEmpty { $$ = $1; } |
    %empty { $$ = nullptr; }
;

CallArgNonEmpty:
    Expression COMMA CallArgNonEmpty
        { $$ = driver->create<lang::ast::node::CallArg>($1); $$->append($3); } |
    Expression
        { $$ = driver->create<lang::ast::node::CallArg>($1); }
;

%%

namespace yy {

parser::token_type yylex(parser::semantic_type* yylval,
    parser::location_type *yylloc, Driver* driver)
{
    return driver->yylex(yylval);
}

void parser::error(const parser::location_type &loc, const std::string& msg)
{
    std::cout << loc << ':' << msg << std::endl;
}

} // namespace yy
