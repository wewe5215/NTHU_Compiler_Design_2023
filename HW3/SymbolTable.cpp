#include <string>
#include <iostream>
#include <cstring>
#include <cassert>
#include "SymbolTable.h"




std::string get_type_name(DataType type) {
    switch (type) {
    case T_INT: return "int";
    case T_PTR: return "int*";
    case T_ARR: return "int[]";
    }
    std::cerr << "unrecognized data type: " << static_cast<int>(type) << std::endl;
    assert (false && "invalid data type");
    return "unreachable";
}

std::string get_value_type_name(ValueType type) {
    switch (type) {
    case lvalue: return "lvalue";
    case rvalue: return "rvalue";
    case no_value: return "no_value";
    }
    std::cerr << "unrecognized value type: " << static_cast<int>(type) << std::endl;
    assert (false && "invalid value type");
    return "unreachable";
}

void Visitor::save_regs_on_stack(std::string whom, std::vector<std::string> &regs) {
    ASM << "  // " << whom << " saves registers >>>" << std::endl;
    ASM << "  addi sp, sp, " << -WORD_SIZE * (int)regs.size() << std::endl;
    for (size_t i = 0; i < regs.size(); i++) {
        auto &reg = regs[i];
        ASM << "  sd " << reg << ", " << WORD_SIZE * i << "(sp)" << std::endl;
    }
    ASM << "  // <<<" << std::endl;
}

void Visitor::restore_regs_from_stack(std::string whom, std::vector<std::string> &regs) {
    ASM << "  // " << whom << " restores registers >>>" << std::endl;
    for (size_t i = 0; i < regs.size(); i++) {
        auto &reg = regs[i];
        ASM << "  ld " << reg << ", " << 8 * i << "(sp)" << std::endl;
    }
    ASM << "  addi sp, sp, " << 8 * (int)regs.size() << std::endl;
    ASM << "  // <<<" << std::endl;
}

std::string Visitor::label_function_end() {
    std::string label = current_function->func_decl->token;
    label += "_end";
    return label;
}

// Node Class

Node::Node(){
    token = NULL;
    grammar = NO_SPECIFIC;
}

Node::Node(char *yytext){
    int token_len = strlen(yytext) + 10; // note the '\0'
    token = new char[token_len];
    grammar = NO_SPECIFIC;
    memcpy(token, yytext, token_len);
}

Node::~Node() {
    if (token != NULL) delete[] token;
}

std::ostream &operator << (std::ostream &out, Node &nd) {
    if (nd.token != NULL) {
        out << nd.token;
    }
    return out;
}

TranslationUnit::~TranslationUnit() {
    for (auto x : decl_func_list) delete x;
}

Declaration::~Declaration() {
    delete type;
    if (initializer) delete initializer;
}

FuncDecl::~FuncDecl() {
    for (auto x : parameter_list) delete x;
}

MultiDecl::~MultiDecl() {
    for (auto x : decl_list) delete x;
}

FuncDefn::FuncDefn(Type* _type, FuncDecl *decl, Statement *body):func_decl(decl), func_body(body) { 
    func_decl->set_type(_type); 
    func_body->is_func_body = true;
}

FuncDefn::~FuncDefn() {
    delete func_decl;
    delete func_body;
}

CompoundStatement::~CompoundStatement() {
    for (auto x : stmt_decl_list) delete x;
}

IfStatement::~IfStatement() {
    delete cond;
    delete if_body;
    if (else_body) delete else_body;
}

Do_While_Statement::~Do_While_Statement() {
    delete cond;
    delete body;
}

WhileStatement::~WhileStatement() {
    delete cond;
    delete body;
}

ForStatement::~ForStatement() {
    delete initialize;
    delete condition;
    delete increment;
    delete body;
}

ExpressionStatement::~ExpressionStatement() {
    delete expr;
}

ReturnStatement::~ReturnStatement() {
    if (expr) delete expr;
}

UnaryExpression::~UnaryExpression() {
    delete expr;
}

BinaryExpression::~BinaryExpression() {
    delete lhs;
    delete rhs;
}

CallExpression::~CallExpression() {
    for (auto x : argument_list) delete x;
}

ArraySubscriptExpression::~ArraySubscriptExpression() {
    delete subscript;
}

Identifier::Identifier(char *str) {
    token = new char[strlen(str)+1];
    memcpy(token, str, strlen(str)+1);
}

Literal::Literal(char *str) {
    token = new char[strlen(str)+1];
    memcpy(token, str, strlen(str)+1);
}

// Vistor (AST)

void Visitor::visit(Node &node) {
    AST << indent() << "<Node>" << std::endl;
}

void Visitor::visit(TranslationUnit &unit) {
    AST << indent() << "<Translation Unit>" << std::endl;

    inc_indent();
    for (auto x : unit.decl_func_list) {
        x->accept(*this);
    }
    dec_indent();
}

void Visitor::visit(Declaration &decl) {
    
}

void Visitor::visit(FuncDecl &decl) {
    
}

void Visitor::visit(FuncDefn &defn) {
    

    current_function = &defn;

    // function header in asm
    ASM << ".global " << defn.func_decl->token << std::endl;
    ASM << defn.func_decl->token << ":" << std::endl;

    // store callee preserved registers
    save_regs_on_stack("callee", callee_saved_reg);
    symbol_table.push_stack(callee_saved_reg.size(), scope.get_scope());
    // set new frame
    ASM << "  addi s0, sp, 104" << std::endl;

    // save parameters as local variables
    int arg_n = defn.func_decl->parameter_list.size();
    ASM << "  addi sp, sp, " << -arg_n*8 << std::endl;
    for (int i = 0; i < arg_n; i++) {
        Declaration *decl = defn.func_decl->parameter_list[i];
        symbol_table.push(decl->token, scope.get_scope(), 1, LOCAL, decl->get_data_type());
        Symbol *sym = symbol_table.lookup(decl->token);
        ASM << "  sd " << function_arguments[i] << ", " << -sym->offset << "(fp)" << std::endl;
    }

    inc_indent();
    defn.func_body->accept(*this);
    dec_indent();

    // release parameters (local variables)
    ASM << "  addi sp, sp, " << arg_n*8 << std::endl;
    for (int i = 0; i < arg_n; i++) symbol_table.pop_stack(1);


    int removed_local_cnt = symbol_table.clear_to_scope(scope.get_scope());
    ASM << "  // clear local variable" << std::endl;
    ASM << "  addi sp, sp, " << removed_local_cnt * 8 << std::endl;

    // restore callee preserved registers
    ASM << "  // release local variables" << std::endl;
    ASM << "  addi sp, sp, " << 8 * (symbol_table.frame_cnt - callee_preserved_registers.size()) << std::endl;

    restore_regs_from_stack("callee", callee_preserved_registers);
    symbol_table.pop_stack(callee_preserved_registers.size());

    current_function = nullptr;

    // return
    ASM << "  jalr x0, 0(ra)" << std::endl;
}

void Visitor::visit(MultiDecl &decl) {
    // no codegen :)
    // no ast :)
    for (auto x : decl.decl_list) x->accept(*this);
}

void Visitor::visit(ScalarDecl &decl) {
    // push into symbol table
    symbol_table.push(decl.token, scope.get_scope(), 1, LOCAL, decl.get_data_type());
    Symbol *sym = symbol_table.lookup(decl.token);
    assert(sym != nullptr && "symbol not found");

    // codegen: simply grow the stack
    ASM << "  addi sp, sp, " << -8 << std::endl;

    // initializer
    inc_indent();
    if (decl.initializer != nullptr) {
        decl.initializer->set_save_to_mem(sym->offset);
        decl.initializer->set_gen_rvalue();
        decl.initializer->accept(*this); 
    }
    dec_indent();
}

void Visitor::visit(ArrayDecl &decl) {
    // push into symbol table
    // Note: Spec doesn't support multi-level-pointer, so array is always `int arr[X]`
    symbol_table.push(decl.token, scope.get_scope(), decl.array_size, LOCAL, INT_ARR); 
    Symbol *sym = symbol_table.lookup(decl.token);
    assert(sym != nullptr && "symbol not found");

    // codegen: simply grow the stack
    ASM << "  addi sp, sp, " << -decl.array_size * 8 << std::endl;

    // note: there is no array initializer in the test cases, forget about them
}

void Visitor::visit(Statement &stmt) {
    
}

void Visitor::visit(CompoundStatement &stmt) {
    

    int backup = scope.get_scope();

    inc_indent();
    scope.enter();
    for (auto c : stmt.stmt_decl_list) c->accept(*this);
    dec_indent();
    int recovered_scope = scope.leave();

    int removed_local_cnt = symbol_table.clear_to_scope(recovered_scope);
    ASM << "  // clear local variable" << std::endl;
    ASM << "  addi sp, sp, " << removed_local_cnt * 8 << std::endl;

    if (stmt.is_func_body) {
        ASM << label_function_end() << ":" << std::endl;
    }
}

void Visitor::visit(IfStatement &if_stmt) {
    

    ASM << "  // IfStatement >>>" << std::endl;

    int if_idx = new_if_label_set();

    // allocate temp for cond
    int cond_offset = symbol_table.push_stack(1, scope.get_scope()) * WORD_SIZE;
    if_stmt.cond->set_save_to_mem(cond_offset);
    if_stmt.cond->set_gen_rvalue();
    ASM << "  addi sp, sp, " << -WORD_SIZE << std::endl;

    // fall-through implementation

    // traverse child (cond)
    inc_indent();
    if_stmt.cond->accept(*this);

    ASM << "  ld t0, " << -cond_offset << "(fp)" << std::endl; // t0 = cond
    ASM << "  beqz t0, " << label_else(if_idx) << std::endl;  // !cond then goto else

    // traverse child (if_body)
    if_stmt.if_body->accept(*this);
    if (if_stmt.else_body) ASM << "  j " << label_end_if(if_idx) << std::endl;

    ASM << label_else(if_idx) << ":" << std::endl;

    // traverse child (else_body)
    if (if_stmt.else_body) if_stmt.else_body->accept(*this);

    ASM << label_end_if(if_idx) << ":" << std::endl;

    // traverse end
    dec_indent();

    // release temp
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << WORD_SIZE << std::endl;

    ASM << "  // <<< IfStatement" << std::endl;
}

void Visitor::visit(Do_While_Statement &do_stmt) {
    

    ASM << "  // DoStatement >>>" << std::endl;

    int loop_idx = new_loop_label_set();
    enter_loop(loop_idx, scope.get_scope());

    // allocate temp for cond
    int cond_offset = symbol_table.push_stack(1, scope.get_scope()) * WORD_SIZE;
    do_stmt.cond->set_save_to_mem(cond_offset);
    do_stmt.cond->set_gen_rvalue();
    ASM << "  addi sp, sp, " << -WORD_SIZE << std::endl;

    // traverse child
    inc_indent();

    ASM << label_loop_start(loop_idx) << ":" << std::endl;
    do_stmt.body->accept(*this);

    ASM << label_loop_continue(loop_idx) << ":" << std::endl;
    do_stmt.cond->accept(*this);
    
    ASM << "  ld t0, " << -cond_offset << "(fp)" << std::endl;
    ASM << "  bnez t0, " << label_loop_start(loop_idx) << std::endl;

    dec_indent();

    leave_loop();
    ASM << label_loop_end(loop_idx) << ":" << std::endl;

    // release temp
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << WORD_SIZE << std::endl;

    ASM << "  // <<< DoStatement" << std::endl;
}

void Visitor::visit(WhileStatement &while_stmt) {
    

    ASM << "  // WhileStatement >>>" << std::endl;

    int loop_idx = new_loop_label_set();
    enter_loop(loop_idx, scope.get_scope());

    // allocate temp for cond
    int cond_offset = symbol_table.push_stack(1, scope.get_scope()) * WORD_SIZE;
    while_stmt.cond->set_save_to_mem(cond_offset);
    while_stmt.cond->set_gen_rvalue();
    ASM << "  addi sp, sp, " << -WORD_SIZE << std::endl;

    // fall-through implementation

    inc_indent();

    ASM << label_loop_continue(loop_idx) << ":" << std::endl;
    while_stmt.cond->accept(*this);
    ASM << "  ld t0, " << -cond_offset << "(fp)" << std::endl;
    ASM << "  beqz t0, " << label_loop_end(loop_idx) << std::endl;

    while_stmt.body->accept(*this);
    ASM << "  j " << label_loop_continue(loop_idx) << std::endl;
    
    dec_indent();

    leave_loop();
    ASM << label_loop_end(loop_idx) << ":" << std::endl;

    // release temp
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << WORD_SIZE << std::endl;

    ASM << "  // <<< WhileStatement" << std::endl;
}

void Visitor::visit(ForStatement &for_stmt) {
    ASM << "  // ForStatement >>>" << std::endl;

    int loop_idx = new_loop_label_set();
    enter_loop(loop_idx, scope.get_scope());

    // allocate temp for cond
    int cond_offset = symbol_table.push_stack(1, scope.get_scope()) * WORD_SIZE;
    for_stmt.condition->set_save_to_mem(cond_offset);
    for_stmt.condition->set_gen_rvalue();
    ASM << "  addi sp, sp, " << -WORD_SIZE << std::endl;

    // fall-through implementation

    // traverse child
    inc_indent();

    for_stmt.initialize->accept(*this);

    ASM << label_loop_cond(loop_idx) << ":" << std::endl;
    for_stmt.condition->accept(*this);
    ASM << "  ld t0, " << -cond_offset << "(fp)" << std::endl;
    ASM << "  beqz t0, " << label_loop_end(loop_idx) << std::endl;

    for_stmt.body->accept(*this);

    ASM << label_loop_continue(loop_idx) << ":" << std::endl;
    for_stmt.increment->accept(*this);
    ASM << "  j " << label_loop_cond(loop_idx) << std::endl;

    dec_indent();

    leave_loop();

    ASM << label_loop_end(loop_idx) << ":" << std::endl;

    // release temp
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << WORD_SIZE << std::endl;

    ASM << "  // <<< ForStatement" << std::endl;
}

void Visitor::visit(ExpressionStatement &expr_stmt) {
    AST << indent() << "<Expression Statement>";
    AST << "[scope = " << scope.get_scope() << "]";
    AST << std::endl;

    inc_indent();
    expr_stmt.expr->accept(*this);
    dec_indent();
}

void Visitor::visit(BreakStatement &break_stmt) {

    ASM << "  // BreakStatement >>>" << std::endl;

    auto [loop_idx, loop_scope] = loop_stack.back();
    int released_cnt = symbol_table.clear_to_scope(loop_scope, true);  
    ASM << "  addi sp, sp, " << released_cnt * 8 << std::endl;

    ASM << "  j " << label_loop_end(loop_idx) << std::endl;

    ASM << "  // <<< BreakStatement" << std::endl;

}

void Visitor::visit(ReturnStatement &return_stmt) {

    ASM << "  // ReturnStatement >>>" << std::endl;

    // allocate temp
    int tmp_offset = symbol_table.push_stack(1, scope.get_scope()) * 8;
    return_stmt.expr->set_save_to_mem(tmp_offset);
    return_stmt.expr->set_gen_rvalue();
    ASM << "  addi sp, sp, " << -8 << std::endl;
    
    inc_indent();
    if (return_stmt.expr) return_stmt.expr->accept(*this);
    dec_indent();

    ASM << "  ld a0, " << -tmp_offset << "(fp)" << std::endl;

    // release temp
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << 8 << std::endl;

    int released_cnt = symbol_table.clear_to_scope(0, true);  
    ASM << "  // return release cnt = " << std::to_string(released_cnt) << std::endl;
    ASM << "  addi sp, sp, " << released_cnt * WORD_SIZE << std::endl;

    /*
    // restore callee preserved registers
    ASM << "  // release local variables" << std::endl;
    ASM << "  addi sp, sp, " << WORD_SIZE * (symbol_table.frame_cnt - callee_preserved_registers.size()) << std::endl;
    */

    ASM << "  j " << label_function_end() << std::endl;
    ASM << "  // <<< ReturnStatement" << std::endl;
}

void Visitor::visit(Expression &expr) {
    AST << indent() << "<Expression>" << std::endl;
}

void Visitor::visit(UnaryExpression &expr) {
    

    ASM << "  // UnaryExpression >>>" << std::endl;

    // allocate temp
    int tmp_offset = symbol_table.push_stack(1, scope.get_scope()) * WORD_SIZE;
    expr.expr->set_save_to_mem(tmp_offset);
    ASM << "  addi sp, sp, " << -WORD_SIZE << std::endl;

    // set value type
    if (expr.op == ADDR) {
        expr.expr->set_gen_lvalue();
    } else {
        expr.expr->set_gen_rvalue();
    }

    // traverse child
    inc_indent();
    expr.expr->accept(*this);
    dec_indent();

    // codegen
    // load result into register
    ASM << "  ld t0, " << -tmp_offset << "(fp)" << std::endl;

    // operator codegen
    switch (expr.op) {
    case ADDR: break;  // t0 is already address of the variable
    case DE_REFERENCE: break;  // t0 is now the address of the variable that is pointed to
    default: 
        NEG: ASM << "  sub t0, x0, t0" << std::endl;
    }

    // Note that op_addr, op_neg must return rvalue
    // Cast op_deref if required
    if (expr.op == DE_REFERENCE && expr.dest.is_rvalue()) {
        ASM << "  ld t0, 0(t0)" << std::endl;
    }

    // return value
    if (expr.dest.is_reg()) {
        ASM << "  addi " << expr.dest.reg_name << ", t0, 0" << std::endl;
    } else if (expr.dest.is_mem()) {
        ASM << "  sd t0, " << -expr.dest.mem_offset << "(fp)" << std::endl;
    }

    // return type
    switch (expr.op) {
    case op_addr: expr.return_type = T_PTR; break;
    case op_deref: expr.return_type = T_INT; break;  // Since only single-level-pointer is suported
    case op_neg: expr.return_type = T_INT; break;
    default: 
        std::cerr << "unsupported operator " << get_op_name(expr.op) << std::endl; 
        assert(false && "unsupported unary operator (return type unknown)");
    }


    // release temp
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << WORD_SIZE << std::endl;

    ASM << "  // <<< UnaryExpression" << std::endl;
}

void Visitor::visit(BinaryExpression &expr) {
    ASM << "  // Binary Expression >>>" << std::endl;

    // allocate temp
    int lhs_offset = symbol_table.push_stack(1, scope.get_scope()) * 8;
    int rhs_offset = symbol_table.push_stack(1, scope.get_scope()) * 8; 
    expr.lhs->set_save_to_mem(lhs_offset);
    expr.rhs->set_save_to_mem(rhs_offset);
    ASM << "  addi sp, sp, " << -2*8 << std::endl;

    // set child lvalue, rvalue
    if (expr.op == ASSIGN) {
        expr.lhs->set_gen_lvalue();
        expr.rhs->set_gen_rvalue();
    } else {
        expr.lhs->set_gen_rvalue();
        expr.rhs->set_gen_rvalue();
    }

    // traverse to child
    inc_indent();
    expr.lhs->accept(*this);
    expr.rhs->accept(*this);
    dec_indent();
    
    // return type
    switch (expr.op) {
    case ADD: case SUB:
        expr.return_type = (expr.lhs->return_type == INT_PTR || expr.rhs->return_type == INT_PTR? INT_PTR : INT_);
        break;
    case MUL: case DIV: case OP_LT: case EQUAL: case NEG:
        expr.return_type = INT_;
        break;
    default:
        expr.return_type = expr.lhs->return_type;
        break;
    }


    // load lhs, rhs result into registers
    ASM << "  ld t0, " << -lhs_offset << "(fp)" << std::endl;
    ASM << "  ld t1, " << -rhs_offset << "(fp)" << std::endl;

    // operator codegen
    if (expr.op == ASSIGN) {
        ASM << "  sd t1, 0(t0)" << std::endl;

        // cast to rvalue if needed
        if (expr.dest.is_rvalue()) ASM << "  addi t0, t1, 0" << std::endl;
    } else if (expr.return_type == INT_) {  // arithmetic
        switch (expr.op) {
        case op_add: ASM << "  add t0, t0, t1" << std::endl; break;
        case op_sub: ASM << "  sub t0, t0, t1" << std::endl; break;
        case op_mul: ASM << "  mul t0, t0, t1" << std::endl; break;
        case op_div: ASM << "  div t0, t0, t1" << std::endl; break;
        case op_lt: ASM << "  slt t0, t0, t1" << std::endl; break;
        case op_eq: 
            ASM << "  sub t0, t0, t1" << std::endl;
            ASM << "  seqz t0, t0" << std::endl;
            break;
        case op_neq:
            ASM << "  sub t0, t0, t1" << std::endl;
            ASM << "  snez t0, t0" << std::endl;
            break;
        default: 
            std::cerr << "unsupported operator " << get_op_name(expr.op) << std::endl; 
            assert(false && "unsupported binary operator (codegen)");
        }

        // Note: arithmetic operator should always be rvalue

    } else {  // pointer int + -
        std::string ptr_reg = (expr.lhs->return_type == T_PTR? "t0" : "t1");
        std::string int_reg = (expr.lhs->return_type == T_INT? "t0" : "t1");

        ASM << "  slli " << int_reg << ", " << int_reg << ", " << LG_WORD_SIZE << std::endl;
        
        // Note that arr is on higher address, while arr+n is on lower address
        switch (expr.op) {
        case op_add: ASM << "  add t0, " << ptr_reg << ", " << int_reg << std::endl; break;
        case op_sub: ASM << "  sub t0, " << ptr_reg << ", " << int_reg << std::endl; break;
        default: 
            std::cerr << "unsupported operator " << get_op_name(expr.op) << std::endl; 
            assert(false && "unsupported binary operator (codegen)");
        }

    }

    // return value
    if (expr.dest.is_reg()) {
        ASM << "  addi " << expr.dest.reg_name << ", t0, 0" << std::endl;
    } else if (expr.dest.is_mem()) {
        ASM << "  sd t0, " << -expr.dest.mem_offset << "(fp)" << std::endl;
    }

    // release temp
    symbol_table.pop_stack(1);
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << 2*8 << std::endl;

    ASM << "  // <<< BinaryExpression" << std::endl;
}

void Visitor::visit(CallExpression &expr) {
    // Note: function always returns int in test case
    // forget about function returning pointer
    expr.return_type = T_INT;

    ASM << "  // CallExpression >>>" << std::endl;

    // allocate temp
    int args_n = expr.argument_list.size();
    int args_offset = symbol_table.push_stack(args_n, scope.get_scope()) * 8;
    ASM << "  addi sp, sp, " << -args_n*8 << std::endl;

    inc_indent();
    for (int i = 0; i < args_n; i++) {
        auto &arg = expr.argument_list[i];
        arg->set_save_to_mem(args_offset + i * 8); 
        arg->set_gen_rvalue(); 
        arg->accept(*this);
    }
    expr.expr->accept(*this);

    // load arguments from stack to reg
    // note that args_n is always <= 7 in test case, forget about case where there are many args
    for (int i = 0; i < args_n; i++) {
        ASM << "  ld " << function_arguments[i] << ", " << -(args_offset + i * 8) << "(fp)" << std::endl;
    }

    // store caller preserved registers
    save_regs_on_stack("caller", caller_saved_reg);

    ASM << "  jal ra, " << expr.expr->token << std::endl;

    // return value, originally stored in a0
    if (expr.dest.is_reg()) {
        ASM << "  addi " << expr.dest.reg_name << ", a0, 0" << std::endl;
    } else if (expr.dest.is_mem()) {
        ASM << "  sd a0, " << -expr.dest.mem_offset << "(fp)" << std::endl;
    }

    // restore caller preserved registers
    restore_regs_from_stack("caller", caller_saved_reg);

    // release temp
    symbol_table.pop_stack(args_n);
    ASM << "  addi sp, sp, " << args_n*WORD_SIZE << std::endl;

    ASM << "  // <<< CallExpression" << std::endl;

    dec_indent();
}

void Visitor::visit(ArraySubscriptExpression &expr) {
    // Note that there is no pointer array in test case, forget about it
    expr.return_type = INT_; 


    ASM << "  // ArraySubscriptExpression >>>" << std::endl;

    // allocate temp
    int expr_offset = symbol_table.push_stack(1, scope.get_scope()) * 8;
    int subscript_offset = symbol_table.push_stack(1, scope.get_scope()) * 8; 
    expr.expr->set_save_to_mem(expr_offset);
    expr.subscript->set_save_to_mem(subscript_offset);
    ASM << "  addi sp, sp, " << -2*8 << std::endl;

    expr.expr->set_gen_rvalue();
    expr.subscript->set_gen_rvalue();

    // traverse to child
    inc_indent();
    expr.expr->accept(*this);
    expr.subscript->accept(*this);
    dec_indent();


    // load into register
    ASM << "  ld t0, " << -expr_offset << "(fp)" << std::endl;
    ASM << "  ld t1, " << -subscript_offset << "(fp)" << std::endl;

    // operator codegen
    ASM << "  slli t1, t1, " << 3 << std::endl;
    ASM << "  add t0, t0, t1" << std::endl;

    // cast to rvalue if needed
    if (expr.dest.is_rvalue()) ASM << "  ld t0, 0(t0)" << std::endl;

    // return value
    if (expr.dest.is_reg()) {
        ASM << "  addi " << expr.dest.reg_name << ", t0, 0" << std::endl;
    } else if (expr.dest.is_mem()) {
        ASM << "  sd t0, " << -expr.dest.mem_offset << "(fp)" << std::endl;
    }

    // release temp
    symbol_table.pop_stack(1);
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << 2*8 << std::endl;

    ASM << "  // <<< ArraySubscriptExpression" << std::endl;
}

void Visitor::visit(Identifier &id) {
    Symbol_Table_Element *sym = symbol_table.lookup(id.token);
    if (sym == nullptr) return;  // probably it is id of function
    id.return_type = (sym->type == INT_ARR? INT_PTR : sym->type);


    int offset = sym->offset;

    ASM << "  // Identifier >>>" << std::endl;

    // lvalue or rvalue
    if (sym->type == INT_ARR) {
        // array type can only be r_value, return base address
        ASM << "  addi t0, fp, " << -offset << std::endl;
    } else if (id.dest.is_lvalue()) {
        ASM << "  addi t0, fp, " << -offset << std::endl;
    } else if (id.dest.is_rvalue()) {
        ASM << "  ld t0, " << -offset << "(fp)" << std::endl;
    }

    // register or mem
    if (id.dest.is_reg()) {
        ASM << "  addi " << id.dest.reg_name << ", t0, 0" << std::endl;
    } else if (id.dest.is_mem()) {
        ASM << "  sd t0, " << -id.dest.mem_offset << "(fp)" << std::endl;
    }

    ASM << "  // <<< Identifier" << std::endl;
}

void Visitor::visit(Literal &lit) {
    lit.return_type = INT_;

    ASM << "  // Literal >>>" << std::endl;

    // always rvalue
    if (lit.dest.is_reg()) {
        ASM << "  li " << lit.dest.reg_name << ", " << lit.token << std::endl;
    } else if (lit.dest.is_mem()) {
        ASM << "  li t0, " << lit.token << std::endl;
        ASM << "  sd t0, " << -lit.dest.mem_offset << "(fp)" << std::endl;
    }

    ASM << "  // <<< Literal" << std::endl;
}