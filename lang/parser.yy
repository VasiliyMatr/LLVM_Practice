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

%nterm <lang::ValType> ValType;

%nterm <lang::BinOpKind> CmpOp;

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
%nterm <lang::ast::node::While *> While;
%nterm <lang::ast::node::If *> If;
%nterm <lang::ast::node::ExprStmt *> ExprStmt;

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
    ValType ID ROUND_BR_OPEN FuncArg ROUND_BR_CLOSE CURLY_BR_OPEN Statement CURLY_BR_CLOSE
        { $$ = driver->create<lang::ast::node::FuncDef>($1, $2, $4, $7); }
;

FuncArg:
    FuncArgNonEmpty { $$ = $1; } |
    %empty { $$ = nullptr; }
;

FuncArgNonEmpty:
    ValType ID COMMA FuncArgNonEmpty
        { $$ = driver->create<lang::ast::node::FuncArg>($1, $2); $$->append($4); } |
    ValType ID
        { $$ = driver->create<lang::ast::node::FuncArg>($1, $2); }
;

Statement:
    Return Statement { $1->append($2); $$ = $1; } |
    While Statement { $1->append($2); $$ = $1; } |
    If Statement { $1->append($2); $$ = $1; } |
    VarDef Statement { $1->append($2); $$ = $1; } |
    ExprStmt Statement { $1->append($2); $$ = $1; } |
    %empty { $$ = nullptr; }
;

Return:
    RETURN Expression SEMICOLON { $$ = driver->create<lang::ast::node::Return>($2); }
;

While:
    WHILE ROUND_BR_OPEN Expression ROUND_BR_CLOSE CURLY_BR_OPEN Statement CURLY_BR_CLOSE
        { $$ = driver->create<lang::ast::node::While>($3, $6); }
;

If:
    IF ROUND_BR_OPEN Expression ROUND_BR_CLOSE CURLY_BR_OPEN Statement CURLY_BR_CLOSE
        { $$ = driver->create<lang::ast::node::If>($3, $6); }
;

VarDef:
    ValType ID ASSIGN Expression SEMICOLON
        { $$ = driver->create<lang::ast::node::VarDef>($1, $2, $4); }
;

ValType:
    INT_TYPE { $$ = lang::ValType::INT; } |
    FIXED_TYPE { $$ = lang::ValType::FIXED; }
;

ExprStmt:
    Expression SEMICOLON { $$ = driver->create<lang::ast::node::ExprStmt>($1); }
;

Expression:
    Compare { $$ = $1; } |
    ID ASSIGN Expression { $$ = driver->create<lang::ast::node::Assign>($1, $3); }
;

Compare:
    AddSub { $$ = $1; } |
    Compare CmpOp AddSub { $$ = driver->create<lang::ast::node::BinOp>($2, $1, $3); }
;

CmpOp:
    LESS { $$ = lang::BinOpKind::CMP_LESS; } |
    LESS_EQUAL { $$ = lang::BinOpKind::CMP_LESS_EQUAL; } |
    GREATER { $$ = lang::BinOpKind::CMP_GREATER; } |
    GREATER_EQUAL { $$ = lang::BinOpKind::CMP_GREATER_EQUAL; } |
    EQUAL { $$ = lang::BinOpKind::CMP_EQUAL; } |
    NOT_EQUAL { $$ = lang::BinOpKind::CMP_NOT_EQUAL; }
;

AddSub:
    MulDiv { $$ = $1; } |
    AddSub PLUS MulDiv
        { $$ = driver->create<lang::ast::node::BinOp>(lang::BinOpKind::ADD, $1, $3); } |
    AddSub MINUS MulDiv
        { $$ = driver->create<lang::ast::node::BinOp>(lang::BinOpKind::SUB, $1, $3); }
;

MulDiv:
    Unary { $$ = $1; } |
    MulDiv ASTERISK Unary
        { $$ = driver->create<lang::ast::node::BinOp>(lang::BinOpKind::MUL, $1, $3); } |
    MulDiv SLASH Unary
        { $$ = driver->create<lang::ast::node::BinOp>(lang::BinOpKind::DIV, $1, $3); }
;

Unary:
    ExprBr { $$ = $1; } |
    PLUS ExprBr
        { $$ = driver->create<lang::ast::node::UnOp>(lang::UnOpKind::PLUS, $2); } |
    MINUS ExprBr
        { $$ = driver->create<lang::ast::node::UnOp>(lang::UnOpKind::MINUS, $2); }
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
