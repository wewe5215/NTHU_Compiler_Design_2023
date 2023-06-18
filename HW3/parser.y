%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <cstdio>
    #include <cassert>
    #include <iostream>
    #include <string>
    #include <vector>
    #include <initializer_list>
    #include "SymbolTable.h"
    #include "y.tab.h"
    FILE* codegen;
    int traceon = 0;
    

    

    
    
%}

%union{
    int intVal;
    double floatVal;
    char* strVal;
}

%token <strVal> FOR DO WHILE BREAK CONTINUE IF ELSE RETURN STRUCT SWITCH CASE DEFAULT
%token <strVal> TYPE_VOID TYPE_INT TYPE_DOUBLE TYPE_FLOAT TYPE_CHAR
%token <strVal> CONST SIGNED UNSIGNED SHORT LONG
%token <intVal> INT HIGH LOW
%token <strVal> IDENT STRING CHAR EXP_NULL DIGITALWRITE DELAY
%token <floatVal> FLOAT 
// operators
%token <strVal> INCREMENT DECREMENT 
%token <strVal> EQ N_EQ
%token <strVal> L_AND L_OR
%token <strVal> '(' ')' '[' ']' '!' '~' '{' '}'
%right <strVal> '='
%left <strVal> '+' '-' '*' '/' '%' '^' '&' '|' 
%left <strVal> LT LE GT GE 
%left <strVal> RIGHT_SHIFT LEFT_SHIFT
//start symbol
%start start_symbol 
%type <strVal> start_builder variable_declarations scalar_declaration array_declaration 
%type <strVal> type idents type_specifier 
/* %type <strVal>  scalar_initializearray_initialize*/ 
%type <strVal> function arrays declarator parameters for_content
%type <strVal> function_declarations 
%type <strVal> expr expression_with_high_prec literal suffix_expr arguments prefix_expr /* unary_op*/ mul_expr add_sub_expr 
%type <strVal> /* shift_expr*/ comparison_expr /* B_and_expr B_xor_expr B_or_expr L_and_expr L_or_expr*/ 
%type <strVal> compound_stmt compound_stmt_content statements expr_stmt if_else_stmt 
%type <strVal> /* switch_stmt*/ while_stmt for_stmt return_stmt break_stmt /* continue_stmt*/
/* %type <strVal>  switch_clauses switch_clause_content */ 
//types 

%%
start_symbol: start_builder
            | start_symbol start_builder
            ;
start_builder: function 
             ;
variable_declarations: scalar_declaration ';' 
                     | array_declaration ';' 
                     ;
scalar_declaration: type idents 
                  ;
type: type_specifier   
    | CONST type_specifier 
    ;
type_specifier: TYPE_INT
              | TYPE_VOID
              ;
idents: declarator            
      | declarator ',' idents
      ;

array_declaration: type arrays 
                 ;
arrays: IDENT '[' expr ']'   { 
                               if(traceon)printf("1\n");
                               $$ = install_symbol($1);
                               int idx = look_up_symbol($1);
                               table[idx].scope = cur_scope;
                               table[idx].offset = local_var_offset++;
                               
                            }
      | IDENT '[' expr ']' '=' expr  { 
                                        if(traceon)printf("2\n");
                                        $$ = install_symbol($1);    
                                        int idx = look_up_symbol($1);
                                        table[idx].scope = cur_scope;
                                        table[idx].offset = local_var_offset++;  
                                        fprintf(codegen, "  lw t0, %d(s0)\n", table[idx].offset * (-4) - 48);
                                        fprintf(codegen, "  addi sp, sp, -4\n");
                                        fprintf(codegen, "  sw t0, 0(sp)\n");

                                     }
      ;


function: function_declarations {                   
                                                    cur_scope++;
                                                    local_var_offset = 0;
                                                    set_scope_and_offset_of_param($1);
                                                    code_gen_func_header($1);
                                }
             compound_stmt  {
                if(traceon)printf("3\n");
                pop_up_symbol(cur_scope);
                cur_scope--;
                code_gen_at_end_of_function_body($1);
            }
            
        | function_declarations ';' 
        ;
declarator: '*' IDENT         { if(traceon)printf("4\n");
                                $$ = install_symbol($2);
                                int idx = look_up_symbol($2);
                                table[idx].scope = cur_scope;
                                table[idx].offset = local_var_offset++;
                              }
          | IDENT             {
                                if(traceon)printf("5\n");
                                $$ = install_symbol($1);
                                int idx = look_up_symbol($1);
                                table[idx].scope = cur_scope;
                                table[idx].offset = local_var_offset++;
                              }
          | '*' IDENT '=' expr { if(traceon)printf("6\n");
                                $$ = install_symbol($2);
                                int idx = look_up_symbol($2);
                                table[idx].scope = cur_scope;
                                table[idx].offset = local_var_offset++;
                                fprintf(codegen, "  lw t0, %d(s0)\n", table[idx].offset * (-4) - 48);
                                fprintf(codegen, "  addi sp, sp, -4\n");
                                fprintf(codegen, "  sw t0, 0(sp)\n");
                              }
          | IDENT  '=' expr   { if(traceon)printf("7\n");
                                $$ = install_symbol($1);
                                int idx = look_up_symbol($1);
                                table[idx].scope = cur_scope;
                                table[idx].offset = local_var_offset++;
                                fprintf(codegen, "  lw t0, %d(s0)\n", table[idx].offset * (-4) - 48);
                                fprintf(codegen, "  addi sp, sp, -4\n");
                                fprintf(codegen, "  sw t0, 0(sp)\n");
                              }
          ;
function_declarations: type IDENT '(' parameters ')' { 
                                                    // function may be defined first
                                                    if(traceon)printf("8\n");
                                                    if(look_up_symbol($2) == -1) $$ = install_symbol($2); 
                                                    $$ = $2;
                                                    
                                               }
                     | type IDENT '(' ')' {
                                                    if(traceon)printf("9\n");
                                                    if(look_up_symbol($2) == -1) $$ = install_symbol($2);
                                                    $$ = $2;
                                                    
                                                    
                     }
                     ;
parameters: parameters ',' type declarator  {if(traceon)printf("parameter!! one\n");}
          | type declarator                 {if(traceon)printf("parameter!! type declarator\n");}
          ;
/*expression*/
expression_with_high_prec: IDENT    {   if(traceon)printf("10\n");
                                        int idx = look_up_symbol($1);
                                        fprintf(codegen, "  lw t0, %d(s0)\n", table[idx].offset * (-4) - 48);
                                        fprintf(codegen, "  addi sp, sp, -4\n");
                                        fprintf(codegen, "  sw t0, 0(sp)\n");
                                        $$ = $1;
                                    }
    | IDENT '[' expr ']'            {   if(traceon)printf("11\n");
                                        int idx = look_up_symbol($1);
                                        fprintf(codegen, "  lw t0, %d(s0)\n", table[idx].offset * (-4) - 48);
                                        fprintf(codegen, "  addi sp, sp, -4\n");
                                        fprintf(codegen, "  sw t0, 0(sp)\n");
                                        $$ = $1;
                                    }
    | literal                       {if(traceon)printf("12\n");}
    | '(' expr ')'                  {   if(traceon)printf("13\n");
                                        $$ = $2;
                                    }
    ;
literal: INT                        {   if(traceon)printf("14\n");
                                        fprintf(codegen, "  li a0, %d\n", $1);
                                        /* fprintf(codegen, "  addi sp, sp, -4\n");
                                        fprintf(codegen, "  sw t0, 0(sp)\n"); */ 
                                    }  
       | HIGH                       {   if(traceon)printf("15\n");
                                        fprintf(codegen, "  li a1, %d\n", $1);
                                       /* fprintf(codegen, "  addi sp, sp, -4\n");
                                        fprintf(codegen, "  sw t0, 0(sp)\n");  */
                                    }
       | LOW                        {   if(traceon)printf("16\n");
                                        fprintf(codegen, "  li a1, %d\n", $1);
                                       /* fprintf(codegen, "  addi sp, sp, -4\n");
                                        fprintf(codegen, "  sw t0, 0(sp)\n");  */
                                    }
       ;
suffix_expr: expression_with_high_prec { if(traceon)printf("19\n");
                                        $$ = $1;
                                        }
           | suffix_expr '(' arguments ')' { 
                
                if(traceon)printf("20\n");
                $$ = $3;
            }
           | suffix_expr '(' ')' { 
                if(traceon)printf("21\n");
            }
           ;

arguments: expr {if(traceon)printf("22\n");}        
         | expr ',' arguments  {if(traceon)printf("23\n");}    
         ;

prefix_expr: suffix_expr {if(traceon)printf("24\n");}
           | '&' suffix_expr { if(traceon)printf("25\n");
                int idx = look_up_symbol($2);
                fprintf(codegen, "  addi t0, sp, %d\n", table[idx].offset * (-4) - 48);
                fprintf(codegen, "  addi sp, sp, -4\n");
                fprintf(codegen, "  sw t0, 0(sp)\n");
                $$ = NULL;
            }
           | '*' suffix_expr {
                if(traceon)printf("26\n");
            }
           | '-' suffix_expr {
                if(traceon)printf("27\n");
            }
           ;

//left precedence
mul_expr: prefix_expr
        | mul_expr '*' prefix_expr  {   if(traceon)printf("28\n");
                                        fprintf(codegen, "  lw t0, 0(sp)\n");
                                        fprintf(codegen, "  addi sp, sp, 4\n");
                                        fprintf(codegen, "  lw t1, 0(sp)\n");
                                        fprintf(codegen, "  addi sp, sp, 4\n");
                                        fprintf(codegen, "  mul t0, t0, t1\n");
                                        fprintf(codegen, "  addi sp, sp, -4\n");
                                        fprintf(codegen, "  sw t0, 0(sp)\n");
                                        $$ = NULL;
                                    }
        | mul_expr '/' prefix_expr  {   if(traceon)printf("29\n");
                                        fprintf(codegen, "  lw t0, 0(sp)\n");
                                        fprintf(codegen, "  addi sp, sp, 4\n");
                                        fprintf(codegen, "  lw t1, 0(sp)\n");
                                        fprintf(codegen, "  addi sp, sp, 4\n");
                                        fprintf(codegen, "  div t0, t0, t1\n");
                                        fprintf(codegen, "  addi sp, sp, -4\n");
                                        fprintf(codegen, "  sw t0, 0(sp)\n");
                                        $$ = NULL;
                                    }
        | mul_expr '%' prefix_expr  {   if(traceon)printf("30\n");
                                        fprintf(codegen, "  lw t0, 0(sp)\n");
                                        fprintf(codegen, "  addi sp, sp, 4\n");
                                        fprintf(codegen, "  lw t1, 0(sp)\n");
                                        fprintf(codegen, "  addi sp, sp, 4\n");
                                        fprintf(codegen, "  rem t0, t0, t1\n");
                                        fprintf(codegen, "  addi sp, sp, -4\n");
                                        fprintf(codegen, "  sw t0, 0(sp)\n");
                                        $$ = NULL;
                                    }
        ;
add_sub_expr: mul_expr
        | add_sub_expr '+' mul_expr {   if(traceon)printf("31\n");
                                        fprintf(codegen, "  lw t0, 0(sp)\n");
                                        fprintf(codegen, "  addi sp, sp, 4\n");
                                        fprintf(codegen, "  lw t1, 0(sp)\n");
                                        fprintf(codegen, "  addi sp, sp, 4\n");
                                        fprintf(codegen, "  add t0, t0, t1\n");
                                        fprintf(codegen, "  addi sp, sp, -4\n");
                                        fprintf(codegen, "  sw t0, 0(sp)\n");
                                        $$ = NULL;
                                    }
        | add_sub_expr '-' mul_expr {   if(traceon)printf("32\n");
                                        fprintf(codegen, "  lw t0, 0(sp)\n");
                                        fprintf(codegen, "  addi sp, sp, 4\n");
                                        fprintf(codegen, "  lw t1, 0(sp)\n");
                                        fprintf(codegen, "  addi sp, sp, 4\n");
                                        fprintf(codegen, "  sub t0, t0, t1\n");
                                        fprintf(codegen, "  addi sp, sp, -4\n");
                                        fprintf(codegen, "  sw t0, 0(sp)\n");
                                        $$ = NULL;
                                    }
        ; 

comparison_expr:add_sub_expr
               |
                comparison_expr LT add_sub_expr { 
                                if(traceon)printf("33\n");
                                fprintf(codegen, "  lw t0, 0(sp)\n");
                                fprintf(codegen, "  addi sp, sp, 4\n");
                                fprintf(codegen, "  lw t1, 0(sp)\n");
                                fprintf(codegen, "  addi sp, sp, 4\n");
                                fprintf(codegen, "  blt t0, t1, L1");
                         }
               
               | comparison_expr GE add_sub_expr  { 
                                if(traceon)printf("34\n");
                                fprintf(codegen, "  lw t0, 0(sp)\n");
                                fprintf(codegen, "  addi sp, sp, 4\n");
                                fprintf(codegen, "  lw t1, 0(sp)\n");
                                fprintf(codegen, "  addi sp, sp, 4\n");
                                fprintf(codegen, "  bge t0, t1, L1");
                         }
               | comparison_expr EQ add_sub_expr  { 
                                if(traceon)printf("35\n");
                                fprintf(codegen, "  lw t0, 0(sp)\n");
                                fprintf(codegen, "  addi sp, sp, 4\n");
                                fprintf(codegen, "  lw t1, 0(sp)\n");
                                fprintf(codegen, "  addi sp, sp, 4\n");
                                fprintf(codegen, "  beq t0, t1, L1");
                         }
               | comparison_expr N_EQ add_sub_expr { 
                                if(traceon)printf("36\n");
                                fprintf(codegen, "  lw t0, 0(sp)\n");
                                fprintf(codegen, "  addi sp, sp, 4\n");
                                fprintf(codegen, "  lw t1, 0(sp)\n");
                                fprintf(codegen, "  addi sp, sp, 4\n");
                                fprintf(codegen, "  bne t0, t1, L1");
                         }
               ;

// right precedence: '=', assignment
expr: comparison_expr
    | comparison_expr '=' expr      { 
                                        if(traceon)printf("37\n");
                                        fprintf(codegen, "  lw t0, 0(fp)\n");
                                        fprintf(codegen, "  addi sp, sp, 4\n");
                                        fprintf(codegen, "  lw t1, 0(fp)\n");
                                        fprintf(codegen, "  addi sp, sp, 4\n");
                                        fprintf(codegen, "  addi t0, t1, 0\n");
                                        fprintf(codegen, "  addi sp, sp, -4\n");
                                        fprintf(codegen, "  sw t0, 0(sp)\n");
                                        $$ = NULL;
                                    }
    ;

/* statement */
compound_stmt:  '{' compound_stmt_content  '}' { if(traceon)printf("38\n");
                                       }   
             | '{' '}'
             ;
compound_stmt_content: statements   {
                                        if(traceon)printf("39\n");
                                    }
                     | statements compound_stmt_content { 
                                        if(traceon)printf("40\n");
                                    }
                     | variable_declarations    {if(traceon)printf("41\n");}
                     | variable_declarations compound_stmt_content  { 
                                        if(traceon)printf("42\n");
                                    }
                     | function_declarations  {if(traceon)printf("43\n");}
                     | function_declarations compound_stmt_content  { 
                                        if(traceon)printf("44\n");
                                    }
                     ;
statements: DIGITALWRITE {
                            fprintf(codegen, "  addi sp, sp, -4\n");
                            fprintf(codegen, "  sw ra, 0(sp)\n"); 
                         } '(' expr ',' expr ')' ';' 
                         { 
                            if(traceon)printf("digital write\n"); 
                            fprintf(codegen, "  jal ra, digitalWrite\n");
                            fprintf(codegen, "  lw ra, 0(sp)\n");
                            fprintf(codegen, "  addi sp, sp, 4\n");
                            fprintf(codegen, "\n");
                        }
          | DELAY {
                    fprintf(codegen, "  addi sp, sp, -4\n");
                    fprintf(codegen, "  sw ra, 0(sp)\n"); 
                  } '(' expr ')' ';' 
                  {
                    if(traceon)printf("delay\n");
                    fprintf(codegen, "  jal ra, delay\n");
                    fprintf(codegen, "  lw ra, 0(sp)\n");
                    fprintf(codegen, "  addi sp, sp, 4\n");
                    fprintf(codegen, "\n");
                  }
          | expr_stmt {if(traceon)printf("46\n");}
          | if_else_stmt {if(traceon)printf("47\n");}  
          | while_stmt   {if(traceon)printf("48\n");}  
          | for_stmt     {if(traceon)printf("49\n");}
          | return_stmt  {if(traceon)printf("50\n");}
          | break_stmt   {if(traceon)printf("51\n");}
          | compound_stmt {if(traceon)printf("52\n");}
          
          ;
expr_stmt: expr ';'                { 
                                        if(traceon)printf("45\n");
                                    }
         ;
if_else_stmt: IF '(' expr ')' compound_stmt
            | IF '(' expr ')' compound_stmt ELSE compound_stmt
            ;


while_stmt: WHILE '(' expr ')' statements { 
                                        
                                    }
          | DO statements WHILE '(' expr ')' ';' { 
                                        
                                    }
          ;
for_stmt: FOR '(' for_content ';' for_content ';' for_content ')' statements { 
                                        
                                    }
        ;
for_content: /* empty */ {$$ = "";}
           | expr                   
           ;

return_stmt: RETURN expr ';'        { 
                                        
                                    }
           | RETURN ';'             { 
                                        
                                    }       
           ;
break_stmt: BREAK ';'               { 
                                        
                                    } 
          ;
%%
int main(void) {
    yyparse();
    printf("parse successfully.\n");
    return 0;
}

void yyerror(char * msg) {
    fprintf(stderr, "Error %s\n", msg);
    exit(1); 
}