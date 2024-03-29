%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>

    char* S_SCALAR_DECL = "<scalar_decl>\0";
    char* E_SCALAR_DECL = "</scalar_decl>\0";
    char* S_ARRAY_DECL = "<array_decl>\0";
    char* E_ARRAY_DECL = "</array_decl>\0";
    char* S_FUNC_DECL = "<func_decl>\0";
    char* E_FUNC_DECL = "</func_decl>\0";
    char* S_FUNC_DEF = "<func_def>\0";
    char* E_FUNC_DEF = "</func_def>\0";
    char* S_EXPR = "<expr>\0";
    char* E_EXPR = "</expr>\0";
    char* S_STMT = "<stmt>\0";
    char* E_STMT = "</stmt>\0";

    char* L_BRACKET = "(\0";
    char* R_BRACKET = ")\0";
    char* L_SQR_BRACKET = "[\0";
    char* R_SQR_BRACKET = "]\0";
    char* L_PARENTHESIS = "{\0";
    char* R_PARENTHESIS = "}\0";

    char* int_to_string(int val){
        int temp = val;
        int digit_num = 1;
        while(temp > 9){
            temp /= 10;
            digit_num ++;
        }
        digit_num += 10;
        char* rst = malloc(sizeof(char) * digit_num);
        sprintf(rst, "%d", val);
        return rst;
    }

    char* float_to_string(double val){
        char* rst = malloc(sizeof(char) * 40);
        sprintf(rst, "%f", val);
        return rst;
    }
%}

%union{
    int intVal;
    double floatVal;
    char* strVal;
}

%token <strVal> FOR DO WHILE BREAK CONTINUE IF ELSE RETURN STRUCT SWITCH CASE DEFAULT
%token <strVal> TYPE_VOID TYPE_INT TYPE_DOUBLE TYPE_FLOAT TYPE_CHAR
%token <strVal> CONST SIGNED UNSIGNED SHORT LONG
%token <intVal> INT 
%token <strVal> IDENT STRING CHAR EXP_NULL
%token <floatVal> FLOAT 
// operators
%token <strVal> INCREMENT DECREMENT 
%token <strVal> EQ N_EQ
%token <strVal> L_AND L_OR
%token <strVal> '(' ')' '{' '}' '[' ']' '!' '~'
%right <strVal> '='
%left <strVal> '+' '-' '*' '/' '%' '^' '&' '|' 
%left <strVal> LT LE GT GE 
%left <strVal> RIGHT_SHIFT LEFT_SHIFT
//start symbol
%start start_symbol 
%type <strVal> start_builder variable_declarations scalar_declaration array_declaration array_declaration
%type <strVal> type idents type_specifier 
%type <strVal> scalar_initialize array_initialize
%type <strVal> function arrays declarator parameters for_content
%type <strVal> array_remaining array_contents function_declarations function_definitions
%type <strVal> expr expression_with_high_prec literal suffix_expr arguments prefix_expr unary_op mul_expr add_sub_expr 
%type <strVal> shift_expr comparison_expr B_and_expr B_xor_expr B_or_expr L_and_expr L_or_expr 
%type <strVal> compound_stmt compound_stmt_content statements expr_stmt if_else_stmt 
%type <strVal> switch_stmt while_stmt for_stmt return_stmt break_stmt continue_stmt
%type <strVal> switch_clauses switch_clause_content statement_list
//types 

%%
start_symbol: start_builder
            | start_symbol start_builder
            ;
start_builder: variable_declarations { printf("%s", $1); $$ = NULL; }
             | function { printf("%s", $1); $$ = NULL; }
             ;
variable_declarations: scalar_declaration ';' { 
                               char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen(E_SCALAR_DECL) + 10));
                               strcpy(s, $1);
                               strcat(s, ";\0");
                               strcat(s, E_SCALAR_DECL);
                               $$ = s;
                               }
                     | array_declaration ';' { 
                               char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen(E_ARRAY_DECL) + 10));
                               strcpy(s, $1);
                               strcat(s, ";\0");
                               strcat(s, E_ARRAY_DECL);
                               $$ = s;
                               }
                     ;
scalar_declaration: type idents { 
                               char* s = malloc(sizeof(char) * (strlen(S_SCALAR_DECL) + strlen($1) + strlen($2) + 10));
                               strcpy(s, S_SCALAR_DECL);
                               strcat(s, $1);
                               strcat(s, $2);
                               
                               $$ = s;
                               }
                  ;
type: type_specifier
    | type_specifier type   { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)+ 10));
                               strcpy(s, $1);
                               strcat(s, $2);
                               $$ = s;
                            }
    | CONST type_specifier { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)+ 10));
                               strcpy(s, $1);
                               strcat(s, $2);
                               $$ = s;
                               }
    | CONST type_specifier type { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + strlen($3)+ 10));
                               strcpy(s, $1);
                               strcat(s, $2);
                               strcat(s, $3);
                               $$ = s;
                               }
    | CONST
    ;
type_specifier: SIGNED
              | UNSIGNED
              | LONG 
              | SHORT
              | TYPE_INT
              | TYPE_CHAR
              | TYPE_FLOAT
              | TYPE_DOUBLE
              | TYPE_VOID
              ;
idents: '*' IDENT             { 
                               char* s = malloc(sizeof(char) * (1 + strlen($2)+ 10));
                               strcpy(s, "*\0");
                               strcat(s, $2);
                               $$ = s;
                              }
      | IDENT
      | '*' IDENT ',' idents  { 
                               char* s = malloc(sizeof(char) * (1 + strlen($2) + 1 + strlen($4)+ 10));
                               strcpy(s, "*\0");
                               strcat(s, $2);
                               strcat(s, ",\0");
                               strcat(s, $4);
                               $$ = s;
                              }
      | IDENT ',' idents      { 
                               char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3)+ 10));
                               strcpy(s, $1);
                               strcat(s, ",\0");
                               strcat(s, $3);
                               $$ = s;
                              }
      | '*' IDENT scalar_initialize { 
                               char* s = malloc(sizeof(char) * (1 + strlen($2) + strlen($3)+ 1));
                               strcpy(s, "*\0");
                               strcat(s, $2);
                               strcat(s, $3);
                               $$ = s;
                              }
      | IDENT scalar_initialize { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)+ 10));
                               strcpy(s, $1);
                               strcat(s, $2);
                               $$ = s;
                               }
      | '*' IDENT scalar_initialize ',' idents { 
                               char* s = malloc(sizeof(char) * (1 + strlen($2) + strlen($3) + 1 + strlen($5)+ 10));
                               strcpy(s, "*\0");
                               strcat(s, $2);
                               strcat(s, $3);
                               strcat(s, ",\0");
                               strcat(s, $5);
                               $$ = s;
                              }
      | IDENT scalar_initialize ',' idents { 
                                    char* s = malloc(sizeof(char) * ( strlen($1) + strlen($2) + 1 + strlen($4)+ 10));
                                    strcpy(s, $1);
                                    strcat(s, $2);
                                    strcat(s, ",\0");
                                    strcat(s, $4);
                                    $$ = s;
                              }
      ;
scalar_initialize: '=' expr;  { 
                                    char* s = malloc(sizeof(char) * (1 + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR)+ 10));
                                    strcpy(s, "=\0");
                                    strcat(s, S_EXPR);
                                    strcat(s, $2);
                                    strcat(s, E_EXPR);
                                    $$ = s;
                              }

array_declaration: type arrays { 
                                    char* s = malloc(sizeof(char) * (strlen(S_ARRAY_DECL) + strlen($1) + strlen($2) + 10));
                                    strcpy(s, S_ARRAY_DECL);
                                    strcat(s, $1);
                                    strcat(s, $2);
                                    
                                    $$ = s;
                               }
                 ;
arrays: '*' IDENT array_remaining   { 
                               char* s = malloc(sizeof(char) * (1 + strlen($2) + strlen($3)+ 10));
                               strcpy(s, "*\0");
                               strcat(s, $2);
                               strcat(s, $3);
                               $$ = s;
                              }
      | IDENT array_remaining   { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)+ 10));
                               strcpy(s, $1);
                               strcat(s, $2);
                               $$ = s;
                            }
      | '*' IDENT array_remaining ',' arrays    { 
                                                    char* s = malloc(sizeof(char) * (1 + strlen($2) + strlen($3) + 1 + strlen($5)+ 10));
                                                    strcpy(s, "*\0");
                                                    strcat(s, $2);
                                                    strcat(s, $3);
                                                    strcat(s, ",\0");
                                                    strcat(s, $5);
                                                    $$ = s;
                                                }
      | IDENT array_remaining ',' arrays        { 
                                                    char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + 1 + strlen($4)+ 10));
                                                    strcpy(s, $1);
                                                    strcat(s, $2);
                                                    strcat(s, ",\0");
                                                    strcat(s, $4);
                                                    $$ = s;
                                                }
      | '*' IDENT array_remaining '=' array_initialize  { 
                                                    char* s = malloc(sizeof(char) * (1 + strlen($2) + strlen($3) + 1 + strlen($5)+ 10));
                                                    strcpy(s, "*\0");
                                                    strcat(s, $2);
                                                    strcat(s, $3);
                                                    strcat(s, "=\0");
                                                    strcat(s, $5);
                                                    $$ = s;
                                                }
      | IDENT array_remaining '=' array_initialize  { 
                                                    char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + 1 + strlen($4)+ 10));
                                                    strcpy(s, $1);
                                                    strcat(s, $2);
                                                    strcat(s, "=\0");
                                                    strcat(s, $4);
                                                    $$ = s;
                                                }
      ;

array_remaining: '[' expr ']' { 
                               char* s = malloc(sizeof(char) * (1 + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR) + 1+ 10));
                               strcpy(s, L_SQR_BRACKET);
                               strcat(s, S_EXPR);
                               strcat(s, $2);
                               strcat(s, E_EXPR);
                               strcat(s, R_SQR_BRACKET);
                               $$ = s;
                               }
               | '[' expr ']' array_remaining { 
                               char* s = malloc(sizeof(char) * (1 + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR) + 1 + strlen($4)+ 10));
                               strcpy(s, L_SQR_BRACKET);
                               strcat(s, S_EXPR);
                               strcat(s, $2);
                               strcat(s, E_EXPR);
                               strcat(s, R_SQR_BRACKET);
                               strcat(s, $4);
                               $$ = s;
                               }
               ;
array_initialize:  expr  { 
                            char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR)+ 10));
                               strcpy(s, S_EXPR);
                               strcat(s, $1);
                               strcat(s, E_EXPR);
                               $$ = s;
                        }

                |  '{' array_contents '}'  { 
                               char* s = malloc(sizeof(char) * (1 + strlen($2) + 1+ 10));
                               strcpy(s, L_PARENTHESIS);
                               strcat(s, $2);
                               strcat(s, R_PARENTHESIS);
                               $$ = s;
                               }
                ;
array_contents: array_initialize 
              | array_initialize ',' array_contents   { 
                               char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3)+ 10));
                               strcpy(s, $1);
                               strcat(s, ",\0");
                               strcat(s, $3);
                               $$ = s;
                               } 
              ; 
function: function_declarations function_definitions { 
                               char* s = malloc(sizeof(char) * (strlen(S_FUNC_DEF) + strlen($1) + strlen($2) + strlen(E_FUNC_DEF) + 10));
                               strcpy(s, S_FUNC_DEF);
                               strcat(s, $1);
                               strcat(s, $2);
                               strcat(s, E_FUNC_DEF);
                               $$ = s;
                               }
        | function_declarations ';' { 
                               char* s = malloc(sizeof(char) * (strlen(S_FUNC_DECL) + strlen($1) + strlen(E_FUNC_DECL)+ 1 + 10));
                               strcpy(s, S_FUNC_DECL);
                               strcat(s, $1);
                               strcat(s, ";\0");
                               strcat(s, E_FUNC_DECL);
                               $$ = s;
                               }
        ;
declarator: '*' IDENT         { 
                               char* s = malloc(sizeof(char) * (1 + strlen($2)+ 10));
                               strcpy(s, "*\0");
                               strcat(s, $2);
                               $$ = s;
                              }
          | IDENT             
          ;
function_declarations: type declarator '(' parameters ')' { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + 1 + strlen($4) + 1 + 10));
                               strcpy(s, $1);
                               strcat(s, $2);
                               strcat(s, L_BRACKET);
                               strcat(s, $4);
                               strcat(s, R_BRACKET);
                               $$ = s;
                               } 
                     | type declarator '(' ')' { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + 1 + 1 + 10));
                               strcpy(s, $1);
                               strcat(s, $2);
                               strcat(s, L_BRACKET);
                               strcat(s, R_BRACKET);
                               $$ = s;
                               } 
                     ;
parameters: parameters ',' type declarator  { 
                                                char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3) + strlen($4)+ 10));
                                                strcpy(s, $1);
                                                strcat(s, ",\0");
                                                strcat(s, $3);
                                                strcat(s, $4);
                                                $$ = s;
                                            } 
          | type declarator                 { 
                                                char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)+ 10));
                                                strcpy(s, $1);
                                                strcat(s, $2);
                                                $$ = s;
                                            }
          ;
function_definitions: compound_stmt
                    ;
/*expression*/
expression_with_high_prec: IDENT    
    | IDENT array_remaining         { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + 1+ 10));
                                        strcpy(s, $1);
                                        strcat(s, $2);
                                        $$ = s;
                                    }
    | literal                      
    | EXP_NULL                          
    | '(' expr ')'                  { 
                                        char* s = malloc(sizeof(char) * (1 + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR) + 1+ 10));
                                        strcpy(s, L_BRACKET);
                                        strcat(s, S_EXPR);
                                        strcat(s, $2);
                                        strcat(s, E_EXPR);
                                        strcat(s, R_BRACKET);
                                        $$ = s;
                                    }
    ;
literal: INT                        {
                                        
                                        char* s = malloc(sizeof(char) * (strlen(int_to_string($1)+ 10)));
                                        strcpy(s, int_to_string($1));
                                        $$ = s;
                                    }
       | FLOAT                      {
                                        
                                        char* s = malloc(sizeof(char) * (strlen(float_to_string($1)+ 10)));
                                        strcpy(s, float_to_string($1));
                                        $$ = s;
                                    }
       | CHAR                       
       | STRING                     
       ;
suffix_expr: expression_with_high_prec
           | suffix_expr INCREMENT  { 
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + strlen($2) + 10));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                        strcat(s, $2);
                                        $$ = s;
                                    }
           | suffix_expr DECREMENT  { 
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + strlen($2) + 10));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                        strcat(s, $2);
                                        $$ = s;
                                    }
           | suffix_expr '(' ')'    { 
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + 2 + 10));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                        strcat(s, L_BRACKET);
                                        strcat(s, R_BRACKET);
                                        $$ = s;
                                    }
           | suffix_expr '(' arguments ')' { 
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + 1 + strlen($3) + 1 + 10));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                        strcat(s, L_BRACKET);
                                        //strcat(s, "hihi");
                                        strcat(s, $3);
                                        strcat(s, R_BRACKET);
                                        $$ = s;
                                    }
           
           ;

arguments: expr                     { 
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR)+ 10));
                                        strcpy(s, S_EXPR);
                                        //strcat(s, "here");
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                        $$ = s;
                                    }
         | expr ',' arguments       { 
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                        strcat(s, ",\0");
                                        //strcat(s, S_EXPR);
                                        strcat(s, $3);
                                        //strcat(s, E_EXPR);
                                        $$ = s;
                                    }
         ;

prefix_expr: suffix_expr
           | unary_op prefix_expr   { 
                                            char* s = malloc(sizeof(char) * (strlen($1) + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR) + 10));
                                            strcpy(s, $1);
                                            strcat(s, S_EXPR);
                                            strcat(s, $2);
                                            strcat(s, E_EXPR);
                                            $$ = s;
                                    }
           | INCREMENT prefix_expr  { 
                                            char* s = malloc(sizeof(char) * (strlen($1) + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR) + 10));
                                            strcpy(s, $1);
                                            strcat(s, S_EXPR);
                                            strcat(s, $2);
                                            strcat(s, E_EXPR);
                                            $$ = s;
                                    }
           | DECREMENT prefix_expr  { 
                                            char* s = malloc(sizeof(char) * (strlen($1) + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR) + 10));
                                            strcpy(s, $1);
                                            strcat(s, S_EXPR);
                                            strcat(s, $2);
                                            strcat(s, E_EXPR);
                                            $$ = s;
                                    }
           | '(' type ')' prefix_expr { 
                                        char* s = malloc(sizeof(char) * (1 + strlen($2) + 1 + strlen(S_EXPR) + strlen($4) + strlen(E_EXPR) + 10));
                                        strcpy(s, L_BRACKET);
                                        strcat(s, $2);
                                        strcat(s, R_BRACKET);
                                        strcat(s, S_EXPR);
                                        strcat(s, $4);
                                        strcat(s, E_EXPR);
                                        $$ = s;
                                      }
           | '(' type '*' ')' prefix_expr  { 
                                        char* s = malloc(sizeof(char) * (1 + strlen($2) + 1 + 1 + strlen(S_EXPR) + strlen($5) + strlen(E_EXPR) + 10));
                                        strcpy(s, L_BRACKET);
                                        strcat(s, $2);
                                        strcat(s, "*\0");
                                        strcat(s, R_BRACKET);
                                        strcat(s, S_EXPR);
                                        strcat(s, $5);
                                        strcat(s, E_EXPR);
                                        $$ = s;
                                    }
           ;
unary_op: '&'
        | '*'
        | '!'
        | '~'
        | '+'
        | '-'
        ;
//left precedence
mul_expr: prefix_expr
        | mul_expr '*' prefix_expr  { 
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                        strcat(s, "*\0");
                                        strcat(s, S_EXPR);
                                        strcat(s, $3);
                                        strcat(s, E_EXPR);
                                        $$ = s;
                                    }
        | mul_expr '/' prefix_expr  { 
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                        strcat(s, "/\0");
                                        strcat(s, S_EXPR);
                                        strcat(s, $3);
                                        strcat(s, E_EXPR);
                                        $$ = s;
                                    }
        | mul_expr '%' prefix_expr  { 
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                        strcat(s, "%\0");
                                        strcat(s, S_EXPR);
                                        strcat(s, $3);
                                        strcat(s, E_EXPR);
                                        $$ = s;
                                    }
        ;
add_sub_expr: mul_expr
        | add_sub_expr '+' mul_expr { 
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                        strcat(s, "+\0");
                                        strcat(s, S_EXPR);
                                        strcat(s, $3);
                                        strcat(s, E_EXPR);
                                        $$ = s;
                                    }
        | add_sub_expr '-' mul_expr { 
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                        strcat(s, "-\0");
                                        strcat(s, S_EXPR);
                                        strcat(s, $3);
                                        strcat(s, E_EXPR);
                                        $$ = s;
                                    }
        ; 
shift_expr: add_sub_expr  
          | shift_expr RIGHT_SHIFT add_sub_expr { 
                                                    char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + strlen($2) + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                                    strcpy(s, S_EXPR);
                                                    strcat(s, $1);
                                                    strcat(s, E_EXPR);
                                                    strcat(s, $2);
                                                    strcat(s, S_EXPR);
                                                    strcat(s, $3);
                                                    strcat(s, E_EXPR);
                                                    $$ = s;
                                                }
          | shift_expr LEFT_SHIFT add_sub_expr  { 
                                                    char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + strlen($2) + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                                    strcpy(s, S_EXPR);
                                                    strcat(s, $1);
                                                    strcat(s, E_EXPR);
                                                    strcat(s, $2);
                                                    strcat(s, S_EXPR);
                                                    strcat(s, $3);
                                                    strcat(s, E_EXPR);
                                                    $$ = s;
                                                }
          ;
comparison_expr: shift_expr
               | comparison_expr LT shift_expr { 
                                char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + strlen($2) + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                strcpy(s, S_EXPR);
                                strcat(s, $1);
                                strcat(s, E_EXPR);
                                strcat(s, $2);
                                strcat(s, S_EXPR);
                                strcat(s, $3);
                                strcat(s, E_EXPR);
                                $$ = s;
                         }
               | comparison_expr LE shift_expr  { 
                                char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + strlen($2) + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                strcpy(s, S_EXPR);
                                strcat(s, $1);
                                strcat(s, E_EXPR);
                                strcat(s, $2);
                                strcat(s, S_EXPR);
                                strcat(s, $3);
                                strcat(s, E_EXPR);
                                $$ = s;
                         }
               | comparison_expr GT shift_expr  { 
                                char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + strlen($2) + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                strcpy(s, S_EXPR);
                                strcat(s, $1);
                                strcat(s, E_EXPR);
                                strcat(s, $2);
                                strcat(s, S_EXPR);
                                strcat(s, $3);
                                strcat(s, E_EXPR);
                                $$ = s;
                         }
               | comparison_expr GE shift_expr  { 
                                char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + strlen($2) + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                strcpy(s, S_EXPR);
                                strcat(s, $1);
                                strcat(s, E_EXPR);
                                strcat(s, $2);
                                strcat(s, S_EXPR);
                                strcat(s, $3);
                                strcat(s, E_EXPR);
                                $$ = s;
                         }
               | comparison_expr EQ shift_expr  { 
                                char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + strlen($2) + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                strcpy(s, S_EXPR);
                                strcat(s, $1);
                                strcat(s, E_EXPR);
                                strcat(s, $2);
                                strcat(s, S_EXPR);
                                strcat(s, $3);
                                strcat(s, E_EXPR);
                                $$ = s;
                         }
               | comparison_expr N_EQ shift_expr { 
                                char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + strlen($2) + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                strcpy(s, S_EXPR);
                                strcat(s, $1);
                                strcat(s, E_EXPR);
                                strcat(s, $2);
                                strcat(s, S_EXPR);
                                strcat(s, $3);
                                strcat(s, E_EXPR);
                                $$ = s;
                         }
               ;
B_and_expr: comparison_expr
          | B_and_expr '&' comparison_expr { 
                                                char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                                strcpy(s, S_EXPR);
                                                strcat(s, $1);
                                                strcat(s, E_EXPR);
                                                strcat(s, "&\0");
                                                strcat(s, S_EXPR);
                                                strcat(s, $3);
                                                strcat(s, E_EXPR);
                                                $$ = s;
                                            }
          ;
B_xor_expr: B_and_expr
          | B_xor_expr '^' B_and_expr   { 
                                            char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                            strcpy(s, S_EXPR);
                                            strcat(s, $1);
                                            strcat(s, E_EXPR);
                                            strcat(s, "^\0");
                                            strcat(s, S_EXPR);
                                            strcat(s, $3);
                                            strcat(s, E_EXPR);
                                            $$ = s;
                                        }
          ;
B_or_expr: B_xor_expr
         | B_or_expr '|' B_xor_expr     { 
                                            char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                            strcpy(s, S_EXPR);
                                            strcat(s, $1);
                                            strcat(s, E_EXPR);
                                            strcat(s, "|\0");
                                            strcat(s, S_EXPR);
                                            strcat(s, $3);
                                            strcat(s, E_EXPR);
                                            $$ = s;
                                        }
         ;
L_and_expr: B_or_expr
          | L_and_expr L_AND B_or_expr  { 
                                char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + strlen($2) + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                strcpy(s, S_EXPR);
                                strcat(s, $1);
                                strcat(s, E_EXPR);
                                strcat(s, $2);
                                strcat(s, S_EXPR);
                                strcat(s, $3);
                                strcat(s, E_EXPR);
                                $$ = s;
                         }
          ;
L_or_expr: L_and_expr
         | L_or_expr L_OR L_and_expr  { 
                                char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + strlen($2) + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 10));
                                strcpy(s, S_EXPR);
                                strcat(s, $1);
                                strcat(s, E_EXPR);
                                strcat(s, $2);
                                strcat(s, S_EXPR);
                                strcat(s, $3);
                                strcat(s, E_EXPR);
                                $$ = s;
                         }
         ;
//right precedence: '=', assignment
expr: L_or_expr
    | L_or_expr '=' expr            { 
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR)+ 10));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                        strcat(s, "=\0");
                                        strcat(s, S_EXPR);
                                        strcat(s, $3);
                                        strcat(s, E_EXPR);
                                        $$ = s;
                                    }
    ;

/*statement*/
compound_stmt: '{' compound_stmt_content '}' { 
                               char* s = malloc(sizeof(char) * (1 + strlen($2) + 1+ 10));
                               strcpy(s, L_PARENTHESIS);
                               strcat(s, $2);
                               strcat(s, R_PARENTHESIS);
                               $$ = s;
                            }
             | '{' '}'      { 
                               char* s = malloc(sizeof(char) * (1 + 1+ 10));
                               strcpy(s, L_PARENTHESIS);
                               strcat(s, R_PARENTHESIS);
                               $$ = s;
                            }   
             ;
compound_stmt_content: statements   {
                                        char* s = malloc(sizeof(char) * (strlen(S_STMT) + strlen($1) + strlen(E_STMT)+ 10));
                                        strcpy(s, S_STMT);
                                        strcat(s, $1);
                                        strcat(s, E_STMT);
                                        $$ = s;
                                    }
                     | statements compound_stmt_content { 
                                        char* s = malloc(sizeof(char) * (strlen(S_STMT) + strlen($1) + strlen(E_STMT) + strlen($2)+ 10));
                                        strcpy(s, S_STMT);
                                        strcat(s, $1);
                                        strcat(s, E_STMT);
                                        strcat(s, $2);
                                        $$ = s;
                                    }
                     | variable_declarations    
                     | variable_declarations compound_stmt_content  { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)+ 10));
                                        strcpy(s, $1);
                                        strcat(s, $2);
                                        $$ = s;
                                    }
                     | function_declarations  
                     | function_declarations compound_stmt_content  { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)+ 10));
                                        strcpy(s, $1);
                                        strcat(s, $2);
                                        $$ = s;
                                    }
                     ;
statements: expr_stmt
          | if_else_stmt   
          | switch_stmt   
          | while_stmt     
          | for_stmt        
          | return_stmt   
          | break_stmt    
          | continue_stmt   
          | compound_stmt 
          ;
expr_stmt: expr ';'                { 
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + 1+ 10));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                        strcat(s, ";");
                                        $$ = s;
                                    }
         ;
if_else_stmt: IF '(' expr ')' compound_stmt { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 1 + strlen(S_STMT) + strlen($5) + strlen(E_STMT)+ 10));
                                        strcpy(s, $1);
                                        strcat(s, L_BRACKET);
                                        strcat(s, S_EXPR);
                                        strcat(s, $3);
                                        strcat(s, E_EXPR);
                                        strcat(s, R_BRACKET);
                                        //strcat(s, S_STMT);
                                        strcat(s, $5);
                                        //strcat(s, E_STMT);
                                        $$ = s;
                                    }
            | IF '(' expr ')' compound_stmt ELSE compound_stmt { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 1 + strlen(S_STMT) + strlen($5) + strlen(E_STMT) + strlen($6) + strlen(S_STMT) + strlen($7) + strlen(E_STMT)+ 10));
                                        strcpy(s, $1);
                                        strcat(s, L_BRACKET);
                                        strcat(s, S_EXPR);
                                        strcat(s, $3);
                                        strcat(s, E_EXPR);
                                        strcat(s, R_BRACKET);
                                        //strcat(s, S_STMT);
                                        strcat(s, $5);
                                        //strcat(s, E_STMT);
                                        strcat(s, $6);
                                        //strcat(s, S_STMT);
                                        strcat(s, $7);
                                        //strcat(s, E_STMT);
                                        $$ = s;
                                    }
            ;
switch_stmt: SWITCH '(' expr ')' '{' switch_clauses '}' { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 1 + 1 + strlen($6) + 1+ 1));
                                        strcpy(s, $1);
                                        strcat(s, L_BRACKET);
                                        strcat(s, S_EXPR);
                                        strcat(s, $3);
                                        strcat(s, E_EXPR);
                                        strcat(s, R_BRACKET);
                                        strcat(s, L_PARENTHESIS);
                                        strcat(s, $6);
                                        strcat(s, R_PARENTHESIS);
                                        $$ = s;
                                    }
            | SWITCH '(' expr ')' '{' '}' { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 1 + 1 + 1+ 10));
                                        strcpy(s, $1);
                                        strcat(s, L_BRACKET);
                                        strcat(s, S_EXPR);
                                        strcat(s, $3);
                                        strcat(s, E_EXPR);
                                        strcat(s, R_BRACKET);
                                        strcat(s, L_PARENTHESIS);
                                        strcat(s, R_PARENTHESIS);
                                        $$ = s;
                                    }
            ;
switch_clauses: switch_clause_content
              | switch_clause_content switch_clauses  { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)+ 10));
                                        strcpy(s, $1);
                                        strcat(s, $2);
                                        $$ = s;
                                    }
              ;
switch_clause_content: CASE expr ':' statement_list   { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR) + 1 + strlen($4)+ 10));
                                        strcpy(s, $1);
                                        strcat(s, S_EXPR);
                                        strcat(s, $2);
                                        strcat(s, E_EXPR);
                                        strcat(s, ":\0");
                                        strcat(s, $4);
                                        $$ = s;
                                    }
                     | CASE expr ':'{ 
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR) + 1+ 10));
                                        strcpy(s, $1);
                                        strcat(s, S_EXPR);
                                        strcat(s, $2);
                                        strcat(s, E_EXPR);
                                        strcat(s, ":\0");
                                        $$ = s;
                                    }
                     | DEFAULT  ':' { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1+ 10));
                                        strcpy(s, $1);
                                        strcat(s, ":\0");
                                        $$ = s;
                                    }
                     | DEFAULT  ':' statement_list   { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3)+ 10));
                                        strcpy(s, $1);
                                        strcat(s, ":\0");
                                        strcat(s, $3);
                                        $$ = s;
                                    }
                     ;
statement_list: statements  {
                                        char* s = malloc(sizeof(char) * (strlen(S_STMT) + strlen($1) + strlen(E_STMT)+ 10));
                                        strcpy(s, S_STMT);
                                        strcat(s, $1);
                                        strcat(s, E_STMT);
                                        $$ = s;
                                    }
              | statements statement_list   { 
                                        char* s = malloc(sizeof(char) * (strlen(S_STMT) + strlen($1) + strlen(E_STMT) + strlen($2)+ 10));
                                        strcpy(s, S_STMT);
                                        strcat(s, $1);
                                        strcat(s, E_STMT);
                                        strcat(s, $2);
                                        $$ = s;
                                    }
              ;
while_stmt: WHILE '(' expr ')' statements { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 1 + strlen(S_STMT) + strlen($5) + strlen(E_STMT)+ 10));
                                        strcpy(s, $1);
                                        strcat(s, L_BRACKET);
                                        strcat(s, S_EXPR);
                                        strcat(s, $3);
                                        strcat(s, E_EXPR);
                                        strcat(s, R_BRACKET);
                                        strcat(s, S_STMT);
                                        strcat(s, $5);
                                        strcat(s, E_STMT);
                                        $$ = s;
                                    }
          | DO statements WHILE '(' expr ')' ';' { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen(S_STMT) + strlen($2) + strlen(E_STMT) + strlen($3) + 1 + strlen(S_EXPR) + strlen($5) + strlen(E_EXPR) + 1 + 1+ 10));
                                        strcpy(s, $1);
                                        strcat(s, S_STMT);
                                        strcat(s, $2);
                                        strcat(s, E_STMT);
                                        strcat(s, $3);
                                        strcat(s, L_BRACKET);
                                        strcat(s, S_EXPR);
                                        strcat(s, $5);
                                        strcat(s, E_EXPR);
                                        strcat(s, R_BRACKET);
                                        strcat(s, ";\0");
                                        $$ = s;
                                    }
          ;
for_stmt: FOR '(' for_content ';' for_content ';' for_content ')' statements { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3) + 1 + strlen($5) + 1 + strlen($7) + 1 + strlen(S_STMT) + strlen($9) + strlen(E_STMT)+ 10));
                                        strcpy(s, $1);
                                        strcat(s, L_BRACKET);
                                        strcat(s, $3);
                                        strcat(s, ";\0");
                                        strcat(s, $5);
                                        strcat(s, ";\0");
                                        strcat(s, $7);
                                        strcat(s, R_BRACKET);
                                        strcat(s, S_STMT);
                                        strcat(s, $9);
                                        strcat(s, E_STMT);
                                        $$ = s;
                                    }
        ;
for_content: /* empty */ {$$ = "";}
           | expr                   { 
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR)+ 10));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                        $$ = s;
                                    }
           ;

return_stmt: RETURN expr ';'        { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR) + 1+ 10));
                                        strcpy(s, $1);
                                        strcat(s, S_EXPR);
                                        strcat(s, $2);
                                        strcat(s, E_EXPR);
                                        strcat(s, ";\0");
                                        $$ = s;
                                    }
           | RETURN ';'             { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1+ 10));
                                        strcpy(s, $1);
                                        strcat(s, ";\0");
                                        $$ = s;
                                    }       
           ;
break_stmt: BREAK ';'               { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1+ 10));
                                        strcpy(s, $1);
                                        strcat(s, ";\0");
                                        $$ = s;
                                    } 
          ;
continue_stmt: CONTINUE ';'         { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1+ 10));
                                        strcpy(s, $1);
                                        strcat(s, ";\0");
                                        $$ = s;
                                    } 
             ;
%%
int main(void) {
    yyparse();
    return 0;
}

void yyerror(char * msg) {
    fprintf(stderr, "Error %s\n", msg);
    exit(1); 
}