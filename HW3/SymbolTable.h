#include <cassert>
#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <tuple>
#include <algorithm>
std::ofstream AST, ASM;
//registers
std::vector<std::string> function_arguments{"a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"};
std::vector<std::string> caller_saved_reg{"ra", "t0", "t1", "t2", "t3", "t4", 
    "t5", "t6", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"};
std::vector<std::string> callee_saved_reg{"sp", "s0", "s1", "s2", "s3", "s4", 
    "s5", "s6", "s7", "s8", "s9", "s10", "s11"};
enum GrammarType {
    NO_SPECIFIC,
    SCALAR_DECL,
    ARRAY_DECL,
    FUN_DECL,
    FUN_DEF,
    EXPR,
    STMT
};

enum OP{
    ASSIGN,
    EQUAL,
    N_EQUAL,
    ADD,
    SUB,
    MUL,
    DIV,
    REM,
    OP_LT,
    OP_LE,
    OP_GT,
    OP_GE,
    ADDR,
    DE_REFERENCE
};

enum MODE_{
    LOCAL,
    GLOBAL,
    ARGUMENT
};
enum DataType{
    INT_,
    INT_PTR,
    INT_ARR
};
std::string Print_OP(OP op_name){
    switch (op_name)
    {
    case ASSIGN : return "assignment";
        break;
    case EQUAL : return "equal";
        break;
    case N_EQUAL : return "non-eq";
        break;
    case ADD : return "addition";
        break;
    case SUB : return "substraction";
        break;
    case MUL : return "multiply";
        break;
    case DIV : return "division";
        break;
    case REM : return "modulo";
        break;
    case OP_LT : return "less than";
        break;
    case OP_LE : return "less equal";
        break;
    case OP_GT : return "greater than";
        break;
    case OP_GE : return "greater equal";
        break;
    case ADDR : return "address";
        break;
    default: return "de-reference";
        break;
    }
}


struct Symbol_Table_Element{
    std::string name;
    int scope;
    int offset;
    MODE_ mode;
    DataType type;
};
struct Symbol_Table{
    std::vector<Symbol_Table_Element> table;
    
};
struct Node{
    char* token;
    GrammarType grammar;

};
struct Type;
struct Declaration;
struct FuncDecl;
struct FuncDefn;
struct Declare_list;   // int a, b, c;
struct ScalarDecl;
struct ArrayDecl;
struct Statement;
struct CompoundStmt;
struct ExpressionStatement;
struct IfStatement;
struct Do_While_Statement;  // do-while
struct WhileStatement; 
struct ForStatement;
struct BreakStatement;
struct ReturnStatement;
struct Expression;
struct UnaryExpression;
struct BinaryExpression;
struct CallExpression;
struct ArraySubscriptExpression;
struct Identifier;
struct Literal;


