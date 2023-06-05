/* The Definition Section will be copied to 
the top of generated C program. 
Include header files, declare variables*/
%{
    #include <stdio.h>
    #include <string.h>
    #include <stdbool.h>
    int lineCount = 0;
    char line_content[300];
    bool pragma_token = true;
    bool pragma_source = true;
    void append_text(char* text){
        strcat(line_content, text);
    }
    void print_token(char* type, char* content){
        if(pragma_token)
            printf("#%s:%s\n", type, content);
    }
    void print_line_inform(){
        lineCount++;
        if(pragma_source)
            printf("%d:%s", lineCount, line_content);
    }
    void clear_line(){
        memset(line_content, '\0', strlen(line_content));
    }

%}
/*The Rules Section is for writing regular expression to recognize tokens. When pattern is matched, then execute action[Regular expression rule] { The things you want to do; }*/
keywords   for|do|while|break|continue|if|else|return|struct|switch|case|default|void|int|double|float|char|const|signed|unsigned|short|long
macros     NULL|__COUNTER__|__LINE__|INT_MAX|INT_MIN|CHAR_MAX|CHAR_MIN|MAX|MIN
Identifiers [a-zA-Z_][0-9a-zA-Z_]*
/*to escape character : \* = {*}, \\ = {\}*/
operators \+|\-|\*|\/|\%|\+\+|\-\-|<|<=|>|>=|==|!=|=|&&|\|\||!|&|\|
punctuations :|;|,|\.|\[|\]|\(|\)|\{|\}
integer \+0|0|-0|[+][0-9]*|[0-9]*|[-][0-9]*
floating [\+-]?([0-9]+)\.[0-9]*|[\+-]?([0-9]*)\.[0-9]+
/* . = {a, b, c, d .....}*/
string \"(\\.|[^"\\])*\"
char '([^']|\\.|\\\\|\\\"|\\'|([\x20-\x7e]{-}[\\'"])|(\\[0-7]{1,3})|(\\x[a-fA-F0-9]+)|(\\u[a-fA-F0-9]{4})|(\\U[a-fA-F0-9]{8}))'
newline \n
%x SINGLE_LINE_COMMENT 
%x COMMENT
%%
<COMMENT>\/\/ {
    //printf("cmt//\n");
    append_text(yytext);
}
\/\/ {
    //printf("normal //\n");
    append_text(yytext);
    BEGIN SINGLE_LINE_COMMENT;
}
<SINGLE_LINE_COMMENT>\\\n {
    //printf("hi\n");
    append_text(yytext);
    print_line_inform();
    clear_line(); 
}
<SINGLE_LINE_COMMENT>. {
    append_text(yytext);
    
}
<SINGLE_LINE_COMMENT>\n {
    append_text(yytext);
    print_line_inform();
    clear_line();
    BEGIN 0;
}

"/*" {
append_text(yytext);
BEGIN COMMENT;
}
<COMMENT>"*/" {
append_text(yytext);
BEGIN 0;}
<COMMENT>. {
    append_text(yytext);
}
<COMMENT>\n {
    append_text(yytext);
    print_line_inform();
    clear_line();
}
#pragma[ \t]+source[ \t]+on {
    append_text(yytext);
    pragma_source = true;
}
#pragma[ \t]+source[ \t]+off {
    append_text(yytext);
    pragma_source = false;
}
#pragma[ \t]+token[ \t]+on {
    append_text(yytext);
    pragma_token = true;
}
#pragma[ \t]+token[ \t]+off {
    append_text(yytext);
    pragma_token = false;
}

{newline} {
    append_text(yytext);
    print_line_inform();
    clear_line();
}

{keywords} {
    append_text(yytext);
    print_token("key", yytext);
}

{macros} {
    append_text(yytext);
    print_token("macro", yytext);

}

{Identifiers} {
    append_text(yytext);
    print_token("id", yytext);

}
{operators} {
    append_text(yytext);
    print_token("op", yytext);

}
{punctuations} {
    append_text(yytext);
    print_token("punc", yytext);

}
{integer} {
    append_text(yytext);
    print_token("integer", yytext);

}
{floating} {
    append_text(yytext);
    print_token("float", yytext);

}
{string} {
    append_text(yytext);
    print_token("string", yytext);

}

{char} {
    append_text(yytext);
    print_token("char", yytext);

}



. {/*deal with blankspace and the other unknown characters*/
  append_text(yytext);
}
%%

/* The C Code Section will be copied to the bottom of generated C program */
int main(void){
    yylex();
    return 0;
}

