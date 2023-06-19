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
    DE_REFERENCE,
    NEG
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
    int size;
    MODE_ mode;
    DataType type;
};
struct Symbol_Table{
    std::vector<Symbol_Table_Element> table;
    int frame_cnt;  // number of stuffs in current stack frame

    void set_frame_cnt(int size) { frame_cnt = size; }
    int push_stack(std::string name, int size, int scope) { 
        push(name, scope, size, LOCAL, INT_);
        return frame_cnt; 
    }
    void pop_stack(int size) { 
        if (table.back().size != size && "stack content incorrect!") {
            Symbol_Table_Element* nd = NULL;
            nd->scope = 7122;
        }
        frame_cnt -= size; 
        table.pop_back();
    }

    void push(std::string name, int scope, int size, MODE_ mode, DataType type) {
        frame_cnt += size;
        table.emplace_back((Symbol_Table_Element){name, scope, size, frame_cnt * 4, mode, type});
    }
    Symbol_Table_Element* lookup(std::string name) {
        std::vector<Symbol_Table_Element>::reverse_iterator it;
        for (it = table.rbegin(); it != table.rend(); it++) {
            if (it->name == name) break;
        }
        if (it == table.rend()) return nullptr;
        else return &(*it);
    }
    int clear_to_scope(int scope, bool fake_delete = false) {
        int remove_cnt = 0;
        std::vector<Symbol_Table_Element> tmp;
        while (table.size() && table.back().scope > scope) {
            frame_cnt -= table.back().size; 
            remove_cnt += table.back().size;
            tmp.emplace_back(table.back());
            table.pop_back();
        }
        std::reverse(tmp.begin(), tmp.end());
        if (fake_delete) {
            table.insert(table.end(), tmp.begin(), tmp.end());
            frame_cnt += remove_cnt;
        }
        return remove_cnt;
    }
    
};
struct Node;
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

struct Scope {
    int scope;

    Scope():scope(0) {}

    int enter() { return ++scope; }
    int leave() { return --scope; }
    int get_scope() { return scope; }
};

// Visitor Declaration

struct Visitor {
    // output file discriptor
    std::ofstream AST, ASM;

    // ast related member
    int ast_indent;

    void inc_indent() { ast_indent++; }
    void dec_indent() { ast_indent--; }
    std::string indent() { return std::string(ast_indent*2, ' '); }

    // codegen related member
    Symbol_Table symbol_table;
    Scope scope;
    void save_regs_on_stack(std::string whom, std::vector<std::string> &regs);
    void restore_regs_from_stack(std::string whom, std::vector<std::string> &regs);

    // codegen label maintainer
    std::string gen_label(std::string str, int idx) { char buff[30]; sprintf(buff, "_%d", idx); return str+buff; }

    int if_cnt;
    int new_if_label_set() { return ++if_cnt; }
    std::string label_else(int idx) { return gen_label("else", idx); }
    std::string label_end_if(int idx) { return gen_label("endif", idx); }

    int loop_cnt;
    int new_loop_label_set() { return ++loop_cnt; }
    /* loop_start: label where loop starts */
    std::string label_loop_start(int idx) { return gen_label("lstart", idx); }
    /* loop_continue: label where `continue` goes to */
    std::string label_loop_continue(int idx) { return gen_label("lcont", idx); }
    /* loop_cond: label where condition starts*/
    std::string label_loop_cond(int idx) { return gen_label("lcond", idx); }
    /* loop_end: label where `break` goes to */
    std::string label_loop_end(int idx) { return gen_label("lend", idx); }

    // maintain which label should `break` `continue` jump to
    // Note: will break when function is allowed to return in the middle of func body
    // (loop idx, loop scope)
    std::vector<std::tuple<int,int>> loop_stack;
    void enter_loop(int idx, int scope) { loop_stack.emplace_back(idx, scope); }
    void leave_loop() { loop_stack.pop_back(); }
    std::tuple<int,int> get_current_loop() { return loop_stack.empty()? std::make_tuple(-1, -1) : loop_stack.back(); }

    // currently in which function, used for return stmt
    FuncDefn *current_function;
    std::string label_function_end();  

    // constructor & visitor pattern
    Visitor():AST("ast.txt"), ASM("codegen.S"), ast_indent(0), if_cnt(0), loop_cnt(0), current_function(nullptr) {}

    void visit(Node &);
    void visit(TranslationUnit &);
    void visit(Declaration &);
    void visit(FuncDecl &);
    void visit(FuncDefn &);
    void visit(MultiDecl &);
    void visit(ScalarDecl &);
    void visit(ArrayDecl &);
    void visit(Statement &);
    void visit(CompoundStatement &);
    void visit(ExpressionStatement &);
    void visit(IfStatement &);
    void visit(Do_While_Statement &);
    void visit(WhileStatement &);
    void visit(ForStatement &);
    void visit(BreakStatement &);
    void visit(ReturnStatement &);
    void visit(Expression &);
    void visit(UnaryExpression &);
    void visit(BinaryExpression &);
    void visit(CallExpression &);
    void visit(ArraySubscriptExpression &);
    void visit(Identifier &);
    void visit(Literal &);
};

// AST Nodes Definition
    
enum ValueType { lvalue, rvalue, no_value };

std::string get_value_type_name(ValueType type);

struct CodegenDest {
    enum Dest { reg, mem, no_gen };
    Dest dest;
    ValueType value_type;
    std::string reg_name;
    int mem_offset;  // relative to stack pointer

    bool is_reg() { return dest == reg; }
    bool is_mem() { return dest == mem; }
    void set_reg(std::string _reg) { dest = reg, std::swap(_reg, reg_name); }
    void set_mem(int _offset) { dest = mem, mem_offset = _offset; }

    bool is_lvalue() { return value_type == lvalue; }
    bool is_rvalue() { return value_type == rvalue; }
    bool set_lvalue() { return value_type = lvalue; }
    bool set_rvalue() { return value_type = rvalue; }

    CodegenDest():dest(no_gen), value_type(no_value) {}
};

// The very base class

struct Node {
    char* token;
    GrammarType grammar;

    virtual void accept(Visitor &visitor) { visitor.visit(*this); }

    Node();
    Node(char*);    // construct from yytext
    virtual ~Node();
};

std::ostream& operator << (std::ostream&, Node&);

// Recursive list to array

template<typename T>
struct NodeList : public Node {
    std::vector<T> arr;

    void push(T elem) { arr.emplace_back(elem); }
};

// Type class

struct Type : public Node {
    DataType type;

    Type(DataType _type):type(_type) {}
    void add(Type *_type) {
        if (_type->type == INT_PTR) type = INT_PTR;
        if (_type->type == INT_ARR) type = INT_ARR;
    }
};



// Declaration base class

struct Declaration : public Node {
    Type *type; // scalar, atomic element of array, return type of function
    Expression *initializer;

    Declaration():type(nullptr), initializer(nullptr) {}

    Declaration(char *txt):Node(txt), type(nullptr), initializer(nullptr) {}
    virtual ~Declaration();

    void accept(Visitor &visitor) { visitor.visit(*this); }

    virtual void set_type(Type* _type) { 
        if (type == nullptr) type = _type;
        else { type->add(_type); delete _type; }
    }
    DataType get_data_type() { return type->type; }

    void set_initializer(Expression *init) { initializer = init; }
};

// Function Declaration

struct FuncDecl : public Declaration {
    std::vector<Declaration*> parameter_list;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    FuncDecl(char* str, NodeList<Declaration*> *_list = nullptr):Declaration(str) {
        if (_list) std::swap(_list->arr, parameter_list);
    }
    virtual ~FuncDecl();
};

struct FuncDefn : public Declaration {
    FuncDecl *func_decl;
    Statement *func_body;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    FuncDefn(Type* _type, FuncDecl *decl, Statement *body);
    ~FuncDefn();
};

// variable declaration

struct MultiDecl: public Declaration {
    std::vector<Declaration*> decl_list;

    void set_type(Type *_type) {
        for (auto decl : decl_list) {
            Type *t = new Type(*_type);
            decl->Declaration::set_type(t);
        }
        delete _type;
    }

    void accept(Visitor &visitor) { visitor.visit(*this); }
    MultiDecl(NodeList<Declaration*> *_list) {
        std::swap(_list->arr, decl_list);
    }
    ~MultiDecl();
};

struct ScalarDecl : public Declaration {

    void accept(Visitor &visitor) { visitor.visit(*this); }
    ScalarDecl(char* str):Declaration(str) {}
};


struct ArrayDecl : public Declaration {
    int array_size;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    ArrayDecl(char* str, int _size):Declaration(str), array_size(_size) {}
};

// Statement Base Class

struct Statement : public Node {
    bool is_func_body;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    Statement():is_func_body(false){}
    virtual ~Statement() {}
};

struct CompoundStatement : public Statement {
    std::vector<Node*> stmt_decl_list;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    CompoundStatement(NodeList<Node*> *_list) {
        if (_list) std::swap(_list->arr, stmt_decl_list);
    }
    ~CompoundStatement();
};

// If Statement (including if-else)
// Note: there is no else-if in spec, forget about it

struct IfStatement : public Statement {
    Expression *cond;
    Statement *if_body, *else_body;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    IfStatement(Expression *_cond, Statement *_if, Statement *_else = nullptr) : cond(_cond), if_body(_if), else_body(_else) {}
    ~IfStatement();
};

// Do While Statement

struct Do_While_Statement : public Statement {
    Expression *cond;
    Statement *body;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    Do_While_Statement(Expression *_cond, Statement *_body) : cond(_cond), body(_body) {}
    ~Do_While_Statement();
};

// While Statement

struct WhileStatement : public Statement {
    Expression *cond;
    Statement *body;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    WhileStatement(Expression *_cond, Statement *_body) : cond(_cond), body(_body) {}
    ~WhileStatement();
};

// For Statement

struct ForStatement : public Statement {
    Expression *initialize, *condition, *increment;
    Statement *body;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    ForStatement(Expression *init, Expression *cond , Expression *inc, Statement *_body)
        : initialize(init), condition(cond), increment(inc), body(_body) {}
    ~ForStatement();
};

// Expression Statement

struct ExpressionStatement : public Statement {
    Expression *expr;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    ExpressionStatement(Expression *_expr):expr(_expr) {}
    ~ExpressionStatement();
};

// Break Statement

struct BreakStatement : public Statement {
    void accept(Visitor &visitor) { visitor.visit(*this); }
};

struct ReturnStatement : public Statement {
    Expression *expr;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    ReturnStatement(Expression *_expr = nullptr):expr(_expr) {}
    ~ReturnStatement();
};

// Expression

struct Expression : public Node {
    CodegenDest dest;
    DataType return_type;

    Expression():return_type(INT_) {}

    void accept(Visitor &visitor) { visitor.visit(*this); }
    virtual ~Expression() {}

    void set_save_to_reg(std::string reg) { dest.set_reg(reg); }
    void set_save_to_mem(int offset) { dest.set_mem(offset); }
    void set_gen_lvalue() { dest.set_lvalue(); }
    void set_gen_rvalue() { dest.set_rvalue(); }
};

struct UnaryExpression : public Expression {
    OP op;
    Expression *expr;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    UnaryExpression(OP _op, Expression *_expr):op(_op), expr(_expr) {}
    virtual ~UnaryExpression();
};

struct BinaryExpression : public Expression { 
    OP op;
    Expression *lhs, *rhs;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    BinaryExpression(OP _op, Expression *_lhs, Expression *_rhs):op(_op), lhs(_lhs), rhs(_rhs) {}
    virtual ~BinaryExpression(); 
};

struct CallExpression : public UnaryExpression {
    std::vector<Expression*> argument_list;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    void set_argument_list(NodeList<Expression*> *nl) { std::swap(nl->arr, argument_list); }

    CallExpression(Expression *_expr, NodeList<Expression*> *nl = nullptr):UnaryExpression(op_call, _expr) {
        if (nl != nullptr) set_argument_list(nl); 
    }
    ~CallExpression();
};

struct ArraySubscriptExpression : public UnaryExpression {
    Expression *subscript;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    ArraySubscriptExpression(Expression *arr, Expression *sub):UnaryExpression(op_subscript, arr), subscript(sub) {}
    ~ArraySubscriptExpression();
};

struct Identifier : public Expression {
    void accept(Visitor &visitor) { visitor.visit(*this); }
    Identifier(char *str);
};

struct Literal : public Expression { 
    void accept(Visitor &visitor) { visitor.visit(*this); }
    Literal(char *str);
};



