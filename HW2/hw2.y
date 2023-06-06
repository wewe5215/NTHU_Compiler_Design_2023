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

%%
start_symbol: start_builder
            | start_symbol start_builder
            ;
start_builder: variable_declarations
             | function
             ;
variable_declarations: scalar_declaration ';'
                     | array_declaration ';'
                     ;
scalar_declaration: type idents 
                  ;
type: type_specifier
    | type_specifier type
    | CONST type_specifier
    | CONST type_specifier type
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
idents: '*' IDENT
      | IDENT
      | '*' IDENT ',' idents
      | IDENT ',' idents
      | '*' IDENT scalar_initialize
      | IDENT scalar_initialize
      | '*' IDENT scalar_initialize ',' idents 
      | IDENT scalar_initialize ',' idents 
      ;
scalar_initialize: '=' expr;

array_declaration: type arrays 
                 ;
arrays: '*' IDENT array_remaining
      | IDENT array_remaining
      | '*' IDENT array_remaining ',' arrays
      | IDENT array_remaining ',' arrays
      | '*' IDENT array_remaining '=' array_initialize
      | IDENT array_remaining '=' array_initialize
      ;

array_remaining: '[' expr ']' 
               | '[' expr ']' array_remaining 
               ;
array_initialize:  expr  
                |  '{' array_contents '}' 
                ;
array_contents: array_initialize 
              | array_initialize ',' array_contents  
              ; 
function: function_declarations function_definitions 
        | function_declarations ';'
        ;
declarator: '*' IDENT         
          | IDENT             
          ;
function_declarations: type declarator '(' parameters ')'
                     | type declarator '(' ')'
                     ;
parameters: parameters ',' type declarator  
          | type declarator 
          ;
function_definitions: compound_stmt
                    ;
/*expression*/
expression_with_high_prec: IDENT    
    | IDENT array_remaining         
    | literal                      
    | EXP_NULL                          
    | '(' expr ')'                  
    ;
literal: INT                        
       | FLOAT                      
       | CHAR                       
       | STRING                     
       ;
suffix_expr: expression_with_high_prec
           | suffix_expr INCREMENT  
           | suffix_expr DECREMENT  
           | suffix_expr '(' ')'    
           | suffix_expr '(' arguments ')' 
           
           ;

arguments: expr                     
         | expr ',' arguments       
         ;

prefix_expr: suffix_expr
           | unary_op prefix_expr   
           | INCREMENT prefix_expr  
           | DECREMENT prefix_expr  
           | '(' type ')' prefix_expr 
           | '(' type '*' ')' prefix_expr  
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
        | mul_expr '*' prefix_expr  
        | mul_expr '/' prefix_expr  
        | mul_expr '%' prefix_expr  
        ;
add_sub_expr: mul_expr
        | add_sub_expr '+' mul_expr 
        | add_sub_expr '-' mul_expr 
        ; 
shift_expr: add_sub_expr  
          | shift_expr RIGHT_SHIFT add_sub_expr 
          | shift_expr LEFT_SHIFT add_sub_expr  
          ;
comparison_expr: shift_expr
               | comparison_expr LT shift_expr 
               | comparison_expr LE shift_expr 
               | comparison_expr GT shift_expr 
               | comparison_expr GE shift_expr  
               | comparison_expr EQ shift_expr  
               | comparison_expr N_EQ shift_expr 
               ;
B_and_expr: comparison_expr
          | B_and_expr '&' comparison_expr 
          ;
B_xor_expr: B_and_expr
          | B_xor_expr '^' B_and_expr   
          ;
B_or_expr: B_xor_expr
         | B_or_expr '|' B_xor_expr 
         ;
L_and_expr: B_or_expr
          | L_and_expr L_AND B_or_expr
          ;
L_or_expr: L_and_expr
         | L_or_expr L_OR L_and_expr  
         ;
//right precedence: '=', assignment
expr: L_or_expr
    | L_or_expr '=' expr    
    ;

/*statement*/
compound_stmt: '{' compound_stmt_content '}'   
             | '{' '}'    
             ;
compound_stmt_content: statements   
                     | statements compound_stmt_content 
                     | variable_declarations
                     | variable_declarations compound_stmt_content
                     | function_declarations  
                     | function_declarations compound_stmt_content
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
expr_stmt: expr ';'                
         ;
if_else_stmt: IF '(' expr ')' compound_stmt 
            | IF '(' expr ')' compound_stmt ELSE compound_stmt 
            ;
switch_stmt: SWITCH '(' expr ')' '{' switch_clauses '}' 
            | SWITCH '(' expr ')' '{' '}' 
            ;
switch_clauses: switch_clause_content
              | switch_clause_content switch_clauses  
              ;
switch_clause_content: CASE expr ':' statement_list    
                     | CASE expr ':'
                     | DEFAULT  ':'
                     | DEFAULT  ':' statement_list
                     ;
statement_list: statements 
              | statements statement_list
              ;
while_stmt: WHILE '(' expr ')' statements 
          | DO statements WHILE '(' expr ')' ';' 
          ;
for_stmt: FOR '(' expr ';' expr ';' expr ')' statements 
        | FOR '(' expr ';'      ';' expr ')' statements 
        | FOR '('      ';' expr ';' expr ')' statements 
        | FOR '(' expr ';' expr ';'      ')' statements 
        | FOR '('      ';'      ';' expr ')' statements 
        | FOR '(' expr ';'      ';'      ')' statements 
        | FOR '('      ';' expr ';'      ')' statements 
        | FOR '('      ';'      ';'      ')' statements 
        ;

return_stmt: RETURN expr ';'        
           | RETURN ';'             
           ;
break_stmt: BREAK ';'               
          ;
continue_stmt: CONTINUE ';'         
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