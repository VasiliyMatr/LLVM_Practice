%option c++
%option yylineno noyywrap

%{

#include <yy_parser.tab.hpp>

%}

WHITE_SPACE         [\n\t\v ]+
COMMENT             "//".*

IDENTIFIER          [A-Za-z_][A-Za-z0-9_]*

INT_NUM             ([1-9][0-9]*|0)

%%

{WHITE_SPACE}           ; // skip
{COMMENT}               ; // skip

"("                     { return yy::parser::token_type::ROUND_BR_OPEN; }
")"                     { return yy::parser::token_type::ROUND_BR_CLOSE; }
"+"                     { return yy::parser::token_type::PLUS; }
"-"                     { return yy::parser::token_type::MINUS; }
"*"                     { return yy::parser::token_type::ASTERISK; }
"/"                     { return yy::parser::token_type::SLASH; }
"<"                     { return yy::parser::token_type::LESS_THAN; }
">"                     { return yy::parser::token_type::GREATER_THAN; }
"=="                    { return yy::parser::token_type::EQUAL; }
"!="                    { return yy::parser::token_type::NOT_EQUAL; }
"="                     { return yy::parser::token_type::ASSIGN; }

";"                     { return yy::parser::token_type::SEMICOLON; }

{IDENTIFIER}            { return yy::parser::token_type::ID; }

{INT_NUM}"."{INT_NUM}   { return yy::parser::token_type::FIXED_VAL; }
{INT_NUM}               { return yy::parser::token_type::INT_VAL; }

.                       { throw yy::parser::syntax_error("Invalid token: "); }


%%