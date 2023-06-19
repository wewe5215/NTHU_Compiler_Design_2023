%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    FILE* codegen;
    int traceon = 0;
    int local_var_offset = 0;
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
    #define MAX_TABLE_SIZE 5000
    typedef struct symbol_entry *PTR_SYMB;
    struct symbol_entry {
    char *name;
    int scope;
    int offset;
    int id;
    int variant;
    int type;
    int total_args;
    int total_locals;
    int mode;
    int functor_index;
    }  table[MAX_TABLE_SIZE];

    #define T_FUNCTION 1
    #define ARGUMENT_MODE   2
    #define LOCAL_MODE      4
    #define GLOBAL_MODE     8
    int cur_counter = 0;
    int cur_scope   = 1;
    char *copys(char* s){
        char* ret = malloc(sizeof(char) * (strlen(s) + 10));
        strcpy(ret, s);
        if(traceon)printf("input: %s\n", s);
        if(traceon)printf("return: %s\n", ret);
        return ret;
    }
    void init_symbol_table(){
        bzero(&table[0], sizeof(struct symbol_entry)*MAX_TABLE_SIZE);
    }   
    char * install_symbol(char *s){
        
        if (cur_counter >= MAX_TABLE_SIZE)
            perror("Symbol Table Full");
        else {
            table[cur_counter].scope = cur_scope;
            table[cur_counter].name = copys(s);
            
            if(traceon)printf("install symbol %s success!\n", table[cur_counter].name);
            if(traceon)printf("input: %s\n", s);
            cur_counter++;
        }
        return s;
    }
    int look_up_symbol(char *s){
        int i;

        if (cur_counter == 0) return -1;
        for (i=cur_counter-1;i>=0; i--)
        {
            if (!strcmp(s,table[i].name)) return i;
        }
        return -1;
    }
    void pop_up_symbol(int scope){
        int i;
    if (cur_counter==0) return;
    for (i=cur_counter-1;i>=0; i--)
        {
        if (table[i].scope !=scope) break;
        }
    if (i<0) cur_counter = 0;
    cur_counter = i+1;
    }
    void set_scope_and_offset_of_param(char *s){
        int i,j,index;
        int total_args;

        index = look_up_symbol(s);
        if (index<0) perror("Error in function header");
        else {
            table[index].type = T_FUNCTION;
            total_args = cur_counter -index -1;
            table[index].total_args=total_args;
            for (j=total_args, i=cur_counter-1;i>index; i--,j--)
                {
                table[i].scope= cur_scope;
                table[i].offset= j;
                table[i].mode  = ARGUMENT_MODE;
                table[i].functor_index  = index;
                }
        }
    }
    void code_gen_func_header(char *functor){
        fprintf(codegen, "%s:\n", functor);
        fprintf(codegen, "  // BEGIN PROLOGUE: codegen is the callee here, so we save callee-saved registers\n");
        fprintf(codegen, "  addi sp, sp, -52\n");
        fprintf(codegen, "  sw sp, 48(sp)\n");
        fprintf(codegen, "  sw s0, 44(sp)\n");
        fprintf(codegen, "  sw s1, 40(s0)\n");
        fprintf(codegen, "  sw s2, 36(s0)\n");
        fprintf(codegen, "  sw s3, 32(s0)\n");
        fprintf(codegen, "  sw s4, 28(s0)\n");
        fprintf(codegen, "  sw s5, 24(s0)\n");
        fprintf(codegen, "  sw s6, 20(s0)\n");
        fprintf(codegen, "  sw s7, 16(s0)\n");
        fprintf(codegen, "  sw s8, 12(s0)\n");
        fprintf(codegen, "  sw s9, 8(s0)\n");
        fprintf(codegen, "  sw s10, 4(s0)\n");
        fprintf(codegen, "  sw s11, 0(s0)\n");
        fprintf(codegen, "  addi s0, sp, 52 // set new frame\n");
        fprintf(codegen, "  // END PROLOGUE\n");
        fprintf(codegen, "  \n");
    }

    void code_gen_at_end_of_function_body(char *functor){
        int i;
        fprintf(codegen, "  \n");
        fprintf(codegen, "  // BEGIN EPILOGUE: restore callee-saved registers\n");
        fprintf(codegen, "  lw sp, 48(sp)\n");
        fprintf(codegen, "  lw s0, 44(sp)\n");
        fprintf(codegen, "  lw s1, 40(sp)\n");
        fprintf(codegen, "  lw s2, 36(sp)\n");
        fprintf(codegen, "  lw s3, 32(sp)\n");
        fprintf(codegen, "  lw s4, 28(sp)\n");
        fprintf(codegen, "  lw s5, 24(sp)\n");
        fprintf(codegen, "  lw s6, 20(sp)\n");
        fprintf(codegen, "  lw s7, 16(sp)\n");
        fprintf(codegen, "  lw s8, 12(sp)\n");
        fprintf(codegen, "  lw s9, 8(sp)\n");
        fprintf(codegen, "  lw s10, 4(sp)\n");
        fprintf(codegen, "  lw s11, 0(sp)\n");
        fprintf(codegen, "  addi sp, sp, 52\n");
        fprintf(codegen, "  // END EPILOGUE\n");
        fprintf(codegen, "  \n");
        fprintf(codegen, "  jalr zero, 0(ra) // return\n");
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
                                        fprintf(codegen, "\n");
                                        
                                    }  
       | HIGH                       {   if(traceon)printf("15\n");
                                        fprintf(codegen, "  li a1, %d\n", $1);
                                        /* fprintf(codegen, "  addi sp, sp, -4\n");
                                        fprintf(codegen, "  sw t0, 0(sp)\n"); */
                                        fprintf(codegen, "\n");
                                        
                                    }
       | LOW                        {   if(traceon)printf("16\n");
                                        fprintf(codegen, "  li a1, %d\n", $1);
                                        /* fprintf(codegen, "  addi sp, sp, -4\n");
                                        fprintf(codegen, "  sw t0, 0(sp)\n"); */
                                        fprintf(codegen, "\n");
                                        
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
                                        fprintf(codegen, "\n");
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
                                        fprintf(codegen, "\n");
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
                                        fprintf(codegen, "\n");
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
    codegen = fopen("codegen.S", "w");
    fprintf(codegen, ".global codegen\n");
    init_symbol_table();
    yyparse();
    printf("parse successfully.\n");
    fprintf(codegen, "\n");
    fclose(codegen);
    return 0;
}

void yyerror(char * msg) {
    fprintf(stderr, "Error %s\n", msg);
    exit(1); 
}