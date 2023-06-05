%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>

    char* S_SCALAR_DECL = "<scalar_decl>";
    char* E_SCALAR_DECL = "</scalar_decl>";
    char* S_ARRAY_DECL = "<array_decl>";
    char* E_ARRAY_DECL = "</array_decl>";
    char* S_FUNC_DECL = "<func_decl>";
    char* E_FUNC_DECL = "</func_decl>";
    char* S_FUNC_DEF = "<func_def>";
    char* E_FUNC_DEF = "</func_def>";
    char* S_EXPR = "<expr>";
    char* E_EXPR = "</expr>";
    char* S_STMT = "<stmt>";
    char* E_STMT = "</stmt>";

    char* L_BRACKET = "(";
    char* R_BRACKET = ")";
    char* L_SQR_BRACKET = "[";
    char* R_SQR_BRACKET = "]";
    char* L_PARENTHESIS = "{";
    char* R_PARENTHESIS = "}";

    char* int_to_string(int val){
        int temp = val;
        int digit_num = 1;
        while(temp > 9){
            temp /= 10;
            digit_num ++;
        }
        char* rst = malloc(sizeof(char) * digit_num);
        sprintf(rst, "%d", val);
        return rst;
    }

    char* float_to_string(float val){
        char* rst = malloc(sizeof(char) * 40);
        sprintf(rst, "%f", val);
        return rst;
    }
%}

%union{
    int intVal;
    float floatVal;
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
//types 
%type <strVal> start_builder 
%type <strVal> variable_declarations function
%type <strVal> scalar_declaration array_declaration
%type <strVal> type idents declarator initialize
%type <strVal> arrays array_remaining array_initialize array_contents
%type <strVal> function_declarations function_definitions
%type <strVal> parameters
%type <strVal> general_type sign_or_unsign long_or_short longs terminal_types
%type <strVal> expr expression_with_high_prec literal suffix_expr arguments prefix_expr unary_op 
%type <strVal> mul_expr add_sub_expr shift_expr comparison_expr B_and_expr B_xor_expr B_or_expr L_and_expr L_or_expr
%type <strVal> statements if_else_stmt switch_stmt while_stmt for_stmt return_stmt break_stmt continue_stmt compound_stmt
%type <strVal> switch_clauses switch_clause_content compound_stmt_content for_content
%type <strVal> expr_stmt initialize_idents
%%
//start
start_symbol: start_builder
            | start_symbol start_builder
            ;

start_builder: variable_declarations { printf("%s", $1); $$ = NULL; }
            | function { printf("%s", $1); $$ = NULL; }
            ;
/*declarations*/
function: function_declarations function_definitions { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               $$ = s;
                               }
        ;
variable_declarations: scalar_declaration ';' { 
                               char* s = malloc(sizeof(char) * (strlen($1) + 1));
                               strcpy(s, $1);
                               strcat(s, ";");
                               $$ = s;
                               }
                     | array_declaration ';' { 
                               char* s = malloc(sizeof(char) * (strlen($1) + 1));
                               strcpy(s, $1);
                               strcat(s, ";");
                               $$ = s;
                               }
                     ;
//c = 0, a = 0; is allowed! revise it 
//TODO
scalar_declaration: type initialize_idents { 
                               char* s = malloc(sizeof(char) * (strlen(S_SCALAR_DECL) + strlen($1) + strlen($2) + strlen(E_SCALAR_DECL)));
                               strcpy(s, S_SCALAR_DECL);
                               strcat(s, $1);
                               strcat(s, $2);
                               strcat(s, E_SCALAR_DECL);
                               $$ = s;
                               }
                  ;
initialize_idents: declarator initialize    { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               $$ = s;
                              }
                 | declarator initialize ',' initialize_idents { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + 1 + strlen($4)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               strcat(s, ",");
                               strcat(s, $4);
                               $$ = s;
                              }
                 ;
idents: idents ',' declarator { 
                               char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3)));
                               strcpy(s, $1);
                               strcat(s, ",");
                               strcat(s, $3);
                               $$ = s;
                              }
      | declarator            
      ;
//pointer, non-pointer
declarator: '*' IDENT         { 
                               char* s = malloc(sizeof(char) * (1 + strlen($2)));
                               strcpy(s, "*");
                               strcat(s, $2);
                               $$ = s;
                              }
          | IDENT             
          ;
initialize: /* empty */       {$$ = "";}
          | '=' expr          { 
                               char* s = malloc(sizeof(char) * (1 + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR)));
                               strcpy(s, "=");
                               strcat(s, S_EXPR);
                               strcat(s, $2);
                               strcat(s, E_EXPR);
                               $$ = s;
                              }
          ;
array_declaration: type arrays { 
                               char* s = malloc(sizeof(char) * (strlen(S_ARRAY_DECL) + strlen($1) + strlen($2) + strlen(E_ARRAY_DECL)));
                               strcpy(s, S_ARRAY_DECL);
                               strcpy(s, $1);
                               strcat(s, $2);
                               strcat(s, E_ARRAY_DECL);
                               $$ = s;
                               }
                 | type arrays '=' array_initialize { 
                               char* s = malloc(sizeof(char) * (strlen(S_ARRAY_DECL) + strlen($1) + strlen($2) + 1 + strlen($4) + strlen(E_ARRAY_DECL)));
                               strcpy(s, S_ARRAY_DECL);
                               strcat(s, $1);
                               strcat(s, $2);
                               strcat(s, "=");
                               strcat(s, $4);
                               strcat(s, E_ARRAY_DECL);
                               $$ = s;
                               }
                 ;
arrays: arrays ',' declarator array_remaining { 
                               char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3) + strlen($4)));
                               strcpy(s, $1);
                               strcat(s, ",");
                               strcat(s, $3);
                               strcat(s, $4);
                               $$ = s;
                               } 
      | declarator array_remaining { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               $$ = s;
                               }
      ;
array_remaining: '[' expr ']' { 
                               char* s = malloc(sizeof(char) * (1 + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR) + 1));
                               strcpy(s, L_SQR_BRACKET);
                               strcat(s, S_EXPR);
                               strcat(s, $2);
                               strcat(s, E_EXPR);
                               strcat(s, R_SQR_BRACKET);
                               $$ = s;
                               }
               | '[' expr ']' array_remaining { 
                               char* s = malloc(sizeof(char) * (1 + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR) + 1 + strlen($4)));
                               strcpy(s, L_SQR_BRACKET);
                               strcat(s, S_EXPR);
                               strcat(s, $2);
                               strcat(s, E_EXPR);
                               strcat(s, R_SQR_BRACKET);
                               strcat(s, $4);
                               $$ = s;
                               }
               ;
array_initialize: expr  { 
                            char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR)));
                            strcpy(s, S_EXPR);
                            strcat(s, $1);
                            strcat(s, E_EXPR);
                        }
                | '{' array_contents '}' { 
                               char* s = malloc(sizeof(char) * (1 + strlen($2) + 1));
                               strcpy(s, L_PARENTHESIS);
                               strcat(s, $2);
                               strcat(s, R_PARENTHESIS);
                               $$ = s;
                               }
                ;
array_contents: array_initialize 
              | array_initialize ',' array_contents  { 
                               char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3)));
                               strcpy(s, $1);
                               strcat(s, ",");
                               strcat(s, $3);
                               $$ = s;
                               } 
              ;  
function_declarations: type declarator '(' parameters ')'   { 
                               char* s = malloc(sizeof(char) * (strlen(S_FUNC_DECL) + strlen($1) + strlen($2) + 1 + strlen($4) + 1 + strlen(E_FUNC_DECL)));
                               strcpy(s, S_FUNC_DECL);
                               strcat(s, $1);
                               strcat(s, $2);
                               strcat(s, L_BRACKET);
                               strcat(s, $4);
                               strcat(s, R_BRACKET);
                               strcat(s, E_FUNC_DECL);
                               $$ = s;
                               } 
parameters: /* empty */        {$$ = "";}
          | parameters ',' type declarator  { 
                               char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3) + strlen($4)));
                               strcpy(s, $1);
                               strcat(s, ",");
                               strcat(s, $3);
                               strcat(s, $4);
                               $$ = s;
                               } 
          | type declarator { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               $$ = s;
                            }
          ;
function_definitions: '{' '}' { 
                               char* s = malloc(sizeof(char) * (strlen(S_FUNC_DEF) + 2 + strlen(E_FUNC_DEF)));
                               strcpy(s, S_FUNC_DEF);
                               strcat(s, L_PARENTHESIS);
                               strcat(s, R_PARENTHESIS);
                               strcat(s, E_FUNC_DEF);
                               $$ = s;
                            }
                    | '{' statements '}' { 
                               char* s = malloc(sizeof(char) * (strlen(S_FUNC_DEF) + 1 + strlen(S_STMT) + strlen($2) + strlen(E_STMT) + 1 + strlen(E_FUNC_DEF)));
                               strcpy(s, S_FUNC_DEF);
                               strcat(s, L_PARENTHESIS);
                               strcat(s, S_STMT);
                               strcat(s, $2);
                               strcat(s, E_STMT);
                               strcat(s, R_PARENTHESIS);
                               strcat(s, E_FUNC_DEF);
                               $$ = s;
                            }
type: general_type 
    | CONST general_type { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               $$ = s;
                         }
    | CONST 
    ;
//need revise
general_type: sign_or_unsign long_or_short TYPE_INT { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + strlen($3)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               strcat(s, $3);
                               $$ = s;
                         }
            | sign_or_unsign longs { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               $$ = s;
                         }
            | sign_or_unsign SHORT { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               $$ = s;
                         }
            | sign_or_unsign TYPE_CHAR { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               $$ = s;
                         }
            | terminal_types; 
sign_or_unsign: /* empty */ {$$ = "";}
              | SIGNED      
              | UNSIGNED    
              ;
long_or_short: /* empty */ {$$ = "";}
             | longs       
             | SHORT       
             ;
longs: LONG LONG { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               $$ = s;
                 }
     | LONG      
     ;
//need revise!
terminal_types: SIGNED      
              | UNSIGNED    
              | TYPE_FLOAT  
              | TYPE_DOUBLE 
              | TYPE_VOID   
              ;
/*expression*/
//operators
//``variable": ``ident" or ``ident[expr]\[[expr]...\]"
//``literal": single signless integer / signless floating-point number / char / string literal
//``NULL": Equals to integer ``0"
expression_with_high_prec: IDENT    
    | IDENT array_remaining         { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + 1));
                                        strcpy(s, $1);
                                        strcat(s, $2);
                                        $$ = s;
                                    }
    | literal                      
    | EXP_NULL                          
    | '(' expr ')'                  { 
                                        char* s = malloc(sizeof(char) * (1 + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR) + 1));
                                        strcpy(s, L_BRACKET);
                                        strcat(s, S_EXPR);
                                        strcat(s, $2);
                                        strcat(s, E_EXPR);
                                        strcat(s, R_BRACKET);
                                        $$ = s;
                                    }
    ;
literal: INT                        {
                                        
                                        char* s = malloc(sizeof(char) * (strlen(int_to_string($1))));
                                        strcpy(s, int_to_string($1));
                                        $$ = s;
                                    }
       | FLOAT                      {
                                        
                                        char* s = malloc(sizeof(char) * (strlen(float_to_string($1))));
                                        strcpy(s, float_to_string($1));
                                        $$ = s;
                                    }
       | CHAR                       
       | STRING                     
       ;
suffix_expr: expression_with_high_prec
           | suffix_expr INCREMENT  { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)));
                                        strcpy(s, $1);
                                        strcat(s, $2);
                                        $$ = s;
                                    }
           | suffix_expr DECREMENT  { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)));
                                        strcpy(s, $1);
                                        strcat(s, $2);
                                        $$ = s;
                                    }
           | suffix_expr '(' ')'    { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 2));
                                        strcpy(s, $1);
                                        strcat(s, L_BRACKET);
                                        strcat(s, R_BRACKET);
                                        $$ = s;
                                    }
           | suffix_expr '(' arguments ')' { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3) + 1));
                                        strcpy(s, $1);
                                        strcat(s, L_BRACKET);
                                        strcat(s, $3);
                                        strcat(s, R_BRACKET);
                                        $$ = s;
                                    }
           
           ;

arguments: expr                     { 
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR)));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                    }
         | expr ',' arguments       { 
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + 1 + strlen($3)));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                        strcat(s, ",");
                                        strcat(s, $3);
                                        $$ = s;
                                    }
         ;

prefix_expr: suffix_expr
           | unary_op prefix_expr   { 
                                            char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)));
                                            strcpy(s, $1);
                                            strcat(s, $2);
                                            $$ = s;
                                    }
           | INCREMENT prefix_expr  { 
                                            char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)));
                                            strcpy(s, $1);
                                            strcat(s, $2);
                                            $$ = s;
                                    }
           | DECREMENT prefix_expr  { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)));
                                        strcpy(s, $1);
                                        strcat(s, $2);
                                        $$ = s;
                                    }
           | '(' type ')' prefix_expr   { 
                                        char* s = malloc(sizeof(char) * (1 + strlen($2) + 1 + strlen($4)));
                                        strcpy(s, L_BRACKET);
                                        strcat(s, $2);
                                        strcat(s, R_BRACKET);
                                        strcat(s, $4);
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
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3)));
                                        strcpy(s, $1);
                                        strcat(s, "*");
                                        strcat(s, $3);
                                        $$ = s;
                                    }
        | mul_expr '/' prefix_expr  { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3)));
                                        strcpy(s, $1);
                                        strcat(s, "/");
                                        strcat(s, $3);
                                        $$ = s;
                                    }
        | mul_expr '%' prefix_expr  { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3)));
                                        strcpy(s, $1);
                                        strcat(s, "%");
                                        strcat(s, $3);
                                        $$ = s;
                                    }
        ;
add_sub_expr: mul_expr
        | add_sub_expr '+' mul_expr { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3)));
                                        strcpy(s, $1);
                                        strcat(s, "+");
                                        strcat(s, $3);
                                        $$ = s;
                                    }
        | add_sub_expr '-' mul_expr { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3)));
                                        strcpy(s, $1);
                                        strcat(s, "-");
                                        strcat(s, $3);
                                        $$ = s;
                                    }
        ; 
shift_expr: add_sub_expr  
          | shift_expr RIGHT_SHIFT add_sub_expr { 
                                                    char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + strlen($3)));
                                                    strcpy(s, $1);
                                                    strcat(s, $2);
                                                    strcat(s, $3);
                                                    $$ = s;
                                                }
          | shift_expr LEFT_SHIFT add_sub_expr  { 
                                                    char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + strlen($3)));
                                                    strcpy(s, $1);
                                                    strcat(s, $2);
                                                    strcat(s, $3);
                                                    $$ = s;
                                                }
          ;
comparison_expr: shift_expr
               | comparison_expr LT shift_expr  { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + strlen($3)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               strcat(s, $3);
                               $$ = s;
                         }
               | comparison_expr LE shift_expr  { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + strlen($3)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               strcat(s, $3);
                               $$ = s;
                         }
               | comparison_expr GT shift_expr  { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + strlen($3)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               strcat(s, $3);
                               $$ = s;
                         }
               | comparison_expr GE shift_expr  { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + strlen($3)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               strcat(s, $3);
                               $$ = s;
                         }
               | comparison_expr EQ shift_expr  { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + strlen($3)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               strcat(s, $3);
                               $$ = s;
                         }
               | comparison_expr N_EQ shift_expr { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + strlen($3)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               strcat(s, $3);
                               $$ = s;
                         }
               ;
B_and_expr: comparison_expr
          | B_and_expr '&' comparison_expr  { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3)));
                                        strcpy(s, $1);
                                        strcat(s, "&");
                                        strcat(s, $3);
                                        $$ = s;
                                    }
          ;
B_xor_expr: B_and_expr
          | B_xor_expr '^' B_and_expr   { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3)));
                                        strcpy(s, $1);
                                        strcat(s, "^");
                                        strcat(s, $3);
                                        $$ = s;
                                    }
          ;
B_or_expr: B_xor_expr
         | B_or_expr '|' B_xor_expr { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3)));
                                        strcpy(s, $1);
                                        strcat(s, "|");
                                        strcat(s, $3);
                                        $$ = s;
                                    }
         ;
L_and_expr: B_or_expr
          | L_and_expr L_AND B_or_expr  { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + strlen($3)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               strcat(s, $3);
                               $$ = s;
                         }
          ;
L_or_expr: L_and_expr
         | L_or_expr L_OR L_and_expr    { 
                               char* s = malloc(sizeof(char) * (strlen($1) + strlen($2) + strlen($3)));
                               strcpy(s, $1);
                               strcat(s, $2);
                               strcat(s, $3);
                               $$ = s;
                         }
         ;
//right precedence: '=', assignment
expr: L_or_expr
    | L_or_expr '=' expr    { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR)));
                                        strcpy(s, $1);
                                        strcat(s, "=");
                                        strcpy(s, S_EXPR);
                                        strcat(s, $3);
                                        strcat(s, E_EXPR);
                                        $$ = s;
                                    }
    ;
/*statement*/
compound_stmt: '{' compound_stmt_content '}'    { 
                               char* s = malloc(sizeof(char) * (1 + strlen($2) + 1));
                               strcpy(s, L_PARENTHESIS);
                               strcat(s, $2);
                               strcat(s, R_PARENTHESIS);
                               $$ = s;
                            }
             | '{' '}'    { 
                               char* s = malloc(sizeof(char) * (1 + 1));
                               strcpy(s, L_PARENTHESIS);
                               strcat(s, R_PARENTHESIS);
                               $$ = s;
                            }
             ;
compound_stmt_content: statements   {
                                        char* s = malloc(sizeof(char) * (strlen(S_STMT) + strlen($1) + strlen(E_STMT)));
                                        strcpy(s, S_STMT);
                                        strcat(s, $1);
                                        strcat(s, E_STMT);
                                        $$ = s;
                                    }
                     | statements compound_stmt_content { 
                                        char* s = malloc(sizeof(char) * (strlen(S_STMT) + strlen($1) + strlen(E_STMT) + strlen($2)));
                                        strcpy(s, S_STMT);
                                        strcat(s, $1);
                                        strcat(s, E_STMT);
                                        strcat(s, $2);
                                        $$ = s;
                                    }
                     | variable_declarations
                     | variable_declarations compound_stmt_content  { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)));
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
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR) + 1));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                        strcat(s, ";");
                                        $$ = s;
                                    }
         ;
if_else_stmt: IF '(' expr ')' compound_stmt { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 1 + strlen(S_STMT) + strlen($5) + strlen(E_STMT)));
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
            | IF '(' expr ')' compound_stmt ELSE compound_stmt  { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 1 + strlen(S_STMT) + strlen($5) + strlen(E_STMT) + strlen($6) + strlen(S_STMT) + strlen($7) + strlen(E_STMT)));
                                        strcpy(s, $1);
                                        strcat(s, L_BRACKET);
                                        strcat(s, S_EXPR);
                                        strcat(s, $3);
                                        strcat(s, E_EXPR);
                                        strcat(s, R_BRACKET);
                                        strcat(s, S_STMT);
                                        strcat(s, $5);
                                        strcat(s, E_STMT);
                                        strcat(s, $6);
                                        strcat(s, S_STMT);
                                        strcat(s, $7);
                                        strcat(s, E_STMT);
                                        $$ = s;
                                    }
            ;
switch_stmt: SWITCH '(' expr ')' '{' switch_clauses '}' { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 1 + 1 + strlen($6) + 1));
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
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 1 + 1 + 1));
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
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen($2)));
                                        strcpy(s, $1);
                                        strcat(s, $2);
                                        $$ = s;
                                    }
              ;
switch_clause_content: CASE expr ':' statements    { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR) + 1 + strlen($4)));
                                        strcpy(s, $1);
                                        strcat(s, S_EXPR);
                                        strcat(s, $2);
                                        strcat(s, E_EXPR);
                                        strcat(s, ":");
                                        strcat(s, $4);
                                        $$ = s;
                                    }
                     | DEFAULT expr ':' statements { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR) + 1 + strlen($4)));
                                        strcpy(s, $1);
                                        strcat(s, S_EXPR);
                                        strcat(s, $2);
                                        strcat(s, E_EXPR);
                                        strcat(s, ":");
                                        strcat(s, $4);
                                        $$ = s;
                                    }
                     ;
while_stmt: WHILE '(' expr ')' statements   { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen(S_EXPR) + strlen($3) + strlen(E_EXPR) + 1 + strlen(S_STMT) + strlen($5) + strlen(E_STMT)));
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
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen(S_STMT) + strlen($2) + strlen(E_STMT) + strlen($3) + 1 + strlen(S_EXPR) + strlen($5) + strlen(E_EXPR) + 1 + 1));
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
                                        strcat(s, ";");
                                        $$ = s;
                                    }
          ;
for_stmt: FOR '(' for_content ';' for_content ';' for_content ')' statements { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1 + strlen($3) + 1 + strlen($5) + 1 + strlen($7) + 1 + strlen(S_STMT) + strlen($9) + strlen(E_STMT)));
                                        strcpy(s, $1);
                                        strcat(s, L_BRACKET);
                                        strcat(s, $3);
                                        strcat(s, ";");
                                        strcat(s, $5);
                                        strcat(s, ";");
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
                                        char* s = malloc(sizeof(char) * (strlen(S_EXPR) + strlen($1) + strlen(E_EXPR)));
                                        strcpy(s, S_EXPR);
                                        strcat(s, $1);
                                        strcat(s, E_EXPR);
                                    }
           ;
return_stmt: RETURN expr ';'        { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + strlen(S_EXPR) + strlen($2) + strlen(E_EXPR) + 1));
                                        strcpy(s, $1);
                                        strcpy(s, S_EXPR);
                                        strcat(s, $2);
                                        strcat(s, E_EXPR);
                                        strcat(s, ";");
                                        $$ = s;
                                    }
           | RETURN ';'             { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1));
                                        strcpy(s, $1);
                                        strcat(s, ";");
                                        $$ = s;
                                    }
           ;
break_stmt: BREAK ';'               { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1));
                                        strcpy(s, $1);
                                        strcat(s, ";");
                                        $$ = s;
                                    }
          ;
continue_stmt: CONTINUE ';'         { 
                                        char* s = malloc(sizeof(char) * (strlen($1) + 1));
                                        strcpy(s, $1);
                                        strcat(s, ";");
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