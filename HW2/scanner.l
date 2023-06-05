/* The Definition Section will be copied to 
the top of generated C program. 
Include header files, declare variables*/
%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <stdbool.h>
    #include "y.tab.h"
    int lineCount = 0;
    char line_content[300];

%}
/*The Rules Section is for writing regular expression to recognize tokens. When pattern is matched, then execute action[Regular expression rule] { The things you want to do; }*/
/*keywords   for|do|while|break|continue|if|else|return|struct|switch|case|default|void|int|double|float|char|const|signed|unsigned|short|long*/
/*macros     NULL|__COUNTER__|__LINE__|INT_MAX|INT_MIN|CHAR_MAX|CHAR_MIN|MAX|MIN*/
Identifiers [a-zA-Z_][0-9a-zA-Z_]*
/*to escape character : \* = {*}, \\ = {\}*/
/*operators \+|\-|\*|\/|\%|\+\+|\-\-|<|<=|>|>=|==|!=|=|&&|\|\||!|&|\|*/
punctuations :|;|,|\.|\[|\]|\(|\)|\{|\}
/* signless integer */
integer [0-9]+
/* signless float */
floating ([0-9]+)\.[0-9]*|([0-9]*)\.[0-9]+
/* . = {a, b, c, d .....}*/
string \"(\\.|[^"\\])*\"
char '([^']|\\.|\\\\|\\\"|\\'|([\x20-\x7e]{-}[\\'"])|(\\[0-7]{1,3})|(\\x[a-fA-F0-9]+)|(\\u[a-fA-F0-9]{4})|(\\U[a-fA-F0-9]{8}))'
newline \n
%%

"for"                           {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return FOR;
                                }
"do"                            {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return DO;
                                }
"while"                         {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return WHILE;
                                }
"break"                         {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return BREAK;
                                }
"continue"                      {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return CONTINUE;
                                }
"if"                            {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return IF;
                                }
"else"                          {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return ELSE;
                                }
"return"                        {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return RETURN;
                                }
"struct"                        {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return STRUCT;
                                }
"switch"                        {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return SWITCH;
                                }
"case"                          {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return CASE;
                                }
"default"                       {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return DEFAULT;
                                }

"void"                          {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return TYPE_VOID;
                                }
"int"                           {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return TYPE_INT;
                                }
"double"                        {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return TYPE_DOUBLE;
                                }
"float"                         {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return TYPE_FLOAT;
                                }
"char"                          {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return TYPE_CHAR;
                                }

"const"                         {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return CONST;
                                }
"signed"                        {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return SIGNED;
                                }
"unsigned"                      {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return UNSIGNED;
                                }
"short"                         {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return SHORT;
                                }
"long"                          {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return LONG;
                                }

"NULL"                          {yylval.strVal = "0"; return EXP_NULL;}

{Identifiers}                   {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return IDENT;
                                }

{integer}                       {
                                    yylval.intVal = atoi(yytext);
                                    return INT;
                                }

{floating}                      {
                                    yylval.floatVal = atof(yytext);
                                    return FLOAT;

                                }

{string}                        {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return STRING;

                                }

{char}                          {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return CHAR;

                                }
    /* 
        operators
    */
"+"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return '+';

                                }
"-"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return '-';

                                }
"*"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return '*';

                                }
"/"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return '/';

                                }
"%"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return '%';

                                }
"++"                            {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return INCREMENT;
                                }
"--"                            {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return DECREMENT;
                                }
"<"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return LT;
                                }
"<="                            {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return LE;
                                }
">"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return GT;
                                }
">="                            {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return GE;
                                }
"=="                            {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return EQ;
                                }
"!="                            {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return N_EQ;
                                }
"="                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return '=';

                                }
"&&"                            {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return L_AND;
                                }
"||"                            {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return L_OR;
                                }
"!"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return '!';

                                }
"~"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return '~';

                                }
"^"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return '^';

                                }
"&"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return '&';

                                }
"|"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return '|';

                                }
">>"                            {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return RIGHT_SHIFT;
                                }
"<<"                            {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return LEFT_SHIFT;
                                }
"["                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return '[';

                                }
"]"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return ']';

                                }
"("                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return '(';

                                }
")"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return ')';

                                }
    /*
        punctuation
    */
":"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return ':';

                                }
";"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return ';';

                                }
","                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return ',';

                                }
"{"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return '{';

                                }
"}"                             {
                                    char* dest = malloc(strlen(yytext));
                                    if(dest != NULL) strcpy(dest, yytext);
                                    yylval.strVal = dest;
                                    return '}';

                                }
%%
