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
%x SINGLE_LINE_COMMENT 
%x COMMENT
%%
<COMMENT>\/\/ {
    //printf("cmt//\n");
    
}
\/\/ {
    //printf("normal //\n");
    
    BEGIN SINGLE_LINE_COMMENT;
}
<SINGLE_LINE_COMMENT>\\\n {
    
}
<SINGLE_LINE_COMMENT>. {
    
    
}
<SINGLE_LINE_COMMENT>\n {
    
    BEGIN 0;
}

"/*" {

BEGIN COMMENT;
}
<COMMENT>"*/" {
BEGIN 0;}
<COMMENT>. {
}
<COMMENT>\n {
}
#pragma[ \t]+source[ \t]+on {
}
#pragma[ \t]+source[ \t]+off {
}
#pragma[ \t]+token[ \t]+on {
}
#pragma[ \t]+token[ \t]+off {
}

{newline} {
    
}
"for"                           {
                                    yylval.strVal = strdup(yytext);
                                    return FOR;
                                }
"do"                            {
                                    yylval.strVal = strdup(yytext);
                                    return DO;
                                }
"while"                         {
                                    yylval.strVal = strdup(yytext);
                                    return WHILE;
                                }
"break"                         {
                                    yylval.strVal = strdup(yytext);
                                    return BREAK;
                                }
"continue"                      {
                                    yylval.strVal = strdup(yytext);
                                    return CONTINUE;
                                }
"if"                            {
                                    yylval.strVal = strdup(yytext);
                                    return IF;
                                }
"else"                          {
                                    yylval.strVal = strdup(yytext);
                                    return ELSE;
                                }
"return"                        {
                                    yylval.strVal = strdup(yytext);
                                    return RETURN;
                                }
"struct"                        {
                                    yylval.strVal = strdup(yytext);
                                    return STRUCT;
                                }
"switch"                        {
                                    yylval.strVal = strdup(yytext);
                                    return SWITCH;
                                }
"case"                          {
                                    yylval.strVal = strdup(yytext);
                                    return CASE;
                                }
"default"                       {
                                    yylval.strVal = strdup(yytext);
                                    return DEFAULT;
                                }

"void"                          {
                                    yylval.strVal = strdup(yytext);
                                    return TYPE_VOID;
                                }
"int"                           {
                                    yylval.strVal = strdup(yytext);
                                    return TYPE_INT;
                                }
"double"                        {
                                    yylval.strVal = strdup(yytext);
                                    return TYPE_DOUBLE;
                                }
"float"                         {
                                    yylval.strVal = strdup(yytext);
                                    return TYPE_FLOAT;
                                }
"char"                          {
                                    yylval.strVal = strdup(yytext);
                                    return TYPE_CHAR;
                                }

"const"                         {
                                    yylval.strVal = strdup(yytext);
                                    return CONST;
                                }
"signed"                        {
                                    yylval.strVal = strdup(yytext);
                                    return SIGNED;
                                }
"unsigned"                      {
                                    yylval.strVal = strdup(yytext);
                                    return UNSIGNED;
                                }
"short"                         {
                                    yylval.strVal = strdup(yytext);
                                    return SHORT;
                                }
"long"                          {
                                    yylval.strVal = strdup(yytext);
                                    return LONG;
                                }
"digitalWrite"                  {
                                    yylval.strVal = strdup(yytext);
                                    return DIGITALWRITE;
                                }
"delay"                         {
                                    yylval.strVal = strdup(yytext);
                                    return DELAY;
                                }

"NULL"                          {yylval.strVal = "0"; return EXP_NULL;}

"HIGH" {
    yylval.intVal = 1; return HIGH;
}

"LOW" {
    yylval.intVal = 0; return LOW;
}
{Identifiers}                   {
                                    yylval.strVal = strdup(yytext);
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
                                    yylval.strVal = strdup(yytext);
                                    return STRING;

                                }

{char}                          {
                                    yylval.strVal = strdup(yytext);
                                    return CHAR;

                                }
    /* 
        operators
    */
"+"                             {
                                    yylval.strVal = strdup(yytext);
                                    return '+';

                                }
"-"                             {
                                    yylval.strVal = strdup(yytext);
                                    return '-';

                                }
"*"                             {
                                    yylval.strVal = strdup(yytext);
                                    return '*';

                                }
"/"                             {
                                    yylval.strVal = strdup(yytext);
                                    return '/';

                                }
"%"                             {
                                    yylval.strVal = strdup(yytext);
                                    return '%';

                                }
"++"                            {
                                    yylval.strVal = strdup(yytext);
                                    return INCREMENT;
                                }
"--"                            {
                                    yylval.strVal = strdup(yytext);
                                    return DECREMENT;
                                }
"<"                             {
                                    yylval.strVal = strdup(yytext);
                                    return LT;
                                }
"<="                            {
                                    yylval.strVal = strdup(yytext);
                                    return LE;
                                }
">"                             {
                                    yylval.strVal = strdup(yytext);
                                    return GT;
                                }
">="                            {
                                    yylval.strVal = strdup(yytext);
                                    return GE;
                                }
"=="                            {
                                    yylval.strVal = strdup(yytext);
                                    return EQ;
                                }
"!="                            {
                                    yylval.strVal = strdup(yytext);
                                    return N_EQ;
                                }
"="                             {
                                    yylval.strVal = strdup(yytext);
                                    return '=';

                                }
"&&"                            {
                                    yylval.strVal = strdup(yytext);
                                    return L_AND;
                                }
"||"                            {
                                    yylval.strVal = strdup(yytext);
                                    return L_OR;
                                }
"!"                             {
                                    yylval.strVal = strdup(yytext);
                                    return '!';

                                }
"~"                             {
                                    yylval.strVal = strdup(yytext);
                                    return '~';

                                }
"^"                             {
                                    yylval.strVal = strdup(yytext);
                                    return '^';

                                }
"&"                             {
                                    yylval.strVal = strdup(yytext);
                                    return '&';

                                }
"|"                             {
                                    yylval.strVal = strdup(yytext);
                                    return '|';

                                }
">>"                            {
                                    yylval.strVal = strdup(yytext);
                                    return RIGHT_SHIFT;
                                }
"<<"                            {
                                    yylval.strVal = strdup(yytext);
                                    return LEFT_SHIFT;
                                }
"["                             {
                                    yylval.strVal = strdup(yytext);
                                    return '[';

                                }
"]"                             {
                                    yylval.strVal = strdup(yytext);
                                    return ']';

                                }
"("                             {
                                    yylval.strVal = strdup(yytext);
                                    return '(';

                                }
")"                             {
                                    yylval.strVal = strdup(yytext);
                                    return ')';

                                }
    /*
        punctuation
    */
":"                             {
                                    yylval.strVal = strdup(yytext);
                                    return ':';

                                }
";"                             {
                                    yylval.strVal = strdup(yytext);
                                    return ';';

                                }
","                             {
                                    yylval.strVal = strdup(yytext);
                                    return ',';

                                }
"{"                             {
                                    yylval.strVal = strdup(yytext);
                                    return '{';

                                }
"}"                             {
                                    yylval.strVal = strdup(yytext);
                                    return '}';

                                }

. {
    
}


%%