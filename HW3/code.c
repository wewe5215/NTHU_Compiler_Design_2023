#include <stdio.h>  
#include <stdlib.h>
#include <string.h>
#include "code.h"
extern FILE *codegen;
int cur_counter = 0;
int cur_scope   = 1;
char *copys(char* s){
    char* ret = malloc(sizeof(char) * (strlen(s) + 10));
    return ret;
}
void init_symbol_table(){
    memset(&table, '\0', sizeof(struct symbol_entry) * MAX_TABLE_SIZE);
}
char * install_symbol(char *s){
    if (cur_counter >= MAX_TABLE_SIZE)
        perror("Symbol Table Full");
    else {
        table[cur_counter].scope = cur_scope;
        table[cur_counter].name = copys(s);
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
void set_local_vars(char *functor){
    int i,j,index,index1;
    int total_locals;

    index = look_up_symbol(functor);
    index1 = index + table[index].total_args;
    total_locals = cur_counter -index1 -1;
    if (total_locals <0)
        perror("Error in number of local variables");
    table[index].total_locals = total_locals;
    for (j = total_locals, i = cur_counter-1;j>0; i--,j--)
    {
        table[i].scope = cur_scope;
        table[i].offset = j;
        table[i].mode = LOCAL_MODE;
        printf("set %s to scope %d, offset %d\n", table[i].name, table[i].scope, table[i].offset);
    }
}

void code_gen_func_header(char *functor){
    fprintf(codegen, "%s:\n", functor);
    fprintf(codegen, "  // BEGIN PROLOGUE: codegen is the callee here, so we save callee-saved registers\n");
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