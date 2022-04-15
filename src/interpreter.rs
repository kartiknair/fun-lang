use std::{collections::HashMap, fmt::Debug};

use crate::{ast, token};

#[derive(Clone)]
struct FunObj<'a> {
    fun_lit: ast::FunLit,
    call: fn(&mut Interpreter<'a>, &ast::FunLit, Vec<Value<'a>>),
}

impl<'a> Debug for FunObj<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunObj")
            .field("fun_lit", &self.fun_lit)
            .field("call", &"<function pointer>".to_string())
            .finish()
    }
}

impl<'a> FunObj<'a> {
    fn call(&self, interpreter: &mut Interpreter<'a>, args: Vec<Value<'a>>) {
        (self.call)(interpreter, &self.fun_lit, args)
    }
}

#[derive(Debug, Clone)]
enum Value<'a> {
    Null,
    True,
    False,

    Number(f64),
    String(String),

    Fun(FunObj<'a>),
    Arr(Vec<Value<'a>>),
    Obj(HashMap<String, Value<'a>>),
}

impl<'a> Value<'a> {
    fn is_truthy(&self) -> bool {
        match self {
            Value::Null => false,
            Value::True => true,
            Value::False => false,
            Value::Number(number) => *number != 0.0,
            Value::String(string) => !string.is_empty(),
            Value::Fun(_) => true,
            Value::Arr(vec) => !vec.is_empty(),
            Value::Obj(obj) => !obj.is_empty(),
        }
    }
}

impl<'a> PartialEq for Value<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(left_num), Self::Number(right_num)) => left_num == right_num,
            (Self::String(left_string), Self::String(right_string)) => left_string == right_string,
            (Self::Fun(_), Self::Fun(_)) => false, // functions are never equal
            (Self::Arr(left_vec), Self::Arr(right_vec)) => {
                if left_vec.len() != right_vec.len() {
                    return false;
                }

                let mut all_same = true;
                for (i, left_el) in left_vec.iter().enumerate() {
                    if &right_vec[i] == left_el {
                        all_same = false;
                        break;
                    }
                }

                all_same
            }
            (Self::Obj(left_map), Self::Obj(right_map)) => {
                if left_map.len() != right_map.len() {
                    return false;
                }

                let mut all_same = true;
                for (left_key, left_value) in left_map.iter() {
                    if let Some(right_value) = right_map.get(left_key) {
                        if left_value != right_value {
                            all_same = false;
                            break;
                        }
                    } else {
                        all_same = false;
                        break;
                    }
                }

                all_same
            }
            _ => false, // since only values that have the same type can be equal
        }
    }
}

impl<'a> Eq for Value<'a> {}

impl<'a> ToString for Value<'a> {
    fn to_string(&self) -> String {
        match self {
            Value::Null => "null".to_string(),
            Value::True => "true".to_string(),
            Value::False => "false".to_string(),
            Value::Number(number) => format!("{}", number),
            Value::String(string) => string.clone(),
            Value::Fun(_) => "<function>".to_string(),
            Value::Arr(values) => {
                format!(
                    "[{}]",
                    values
                        .iter()
                        .map(|value| value.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Value::Obj(map) => {
                format!(
                    "{{{}}}",
                    map.iter()
                        .map(|(key, value)| format!("\"{}\": {}", key, value.to_string()))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

#[derive(Debug, Clone)]
struct ScopeStack<'a> {
    stack: Vec<HashMap<String, Value<'a>>>,
}

impl<'a> ScopeStack<'a> {
    fn new() -> Self {
        Self { stack: Vec::new() }
    }

    fn nest(&mut self) {
        self.stack.push(HashMap::new());
    }

    fn unnest(&mut self) {
        self.stack.pop();
    }

    fn insert(&mut self, name: String, value: Value<'a>) {
        if let Some(current_scope) = self.stack.last_mut() {
            current_scope.insert(name, value);
        } else {
            let mut initial_scope = HashMap::new();
            initial_scope.insert(name, value);
            self.stack.push(initial_scope);
        }
    }

    fn get(&self, name: &str) -> Option<&Value<'a>> {
        for scope in self.stack.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }

        return None;
    }

    fn get_mut(&mut self, name: &str) -> Option<&mut Value<'a>> {
        for scope in self.stack.iter_mut().rev() {
            if let Some(value) = scope.get_mut(name) {
                return Some(value);
            }
        }

        return None;
    }
}

#[derive(Debug, Clone)]
struct CallStackFrame {
    return_adr: usize,
}

struct Interpreter<'a> {
    file: &'a ast::File,
    namespace: ScopeStack<'a>,

    expr_pc: usize,
    return_value: Option<Value<'a>>,
    call_stack: Vec<CallStackFrame>,
}

impl<'a> Interpreter<'a> {
    fn lvalue_reference(&mut self, expr: &ast::Expr) -> Option<&mut Value<'a>> {
        match &expr.kind {
            ast::ExprKind::Var(var_expr) => self
                .namespace
                .get_mut(&self.file.lexeme(&var_expr.ident.span)),

            ast::ExprKind::Idx(idx_expr) => {
                let idx = self.interpret_expr(&*idx_expr.idx);
                let target = if let Some(value) = self.lvalue_reference(&*idx_expr.target) {
                    value
                } else {
                    return None;
                };
                if let Value::Arr(target) = target {
                    if let Value::Number(idx) = idx {
                        let int_part = idx.trunc();
                        if int_part != idx {
                            None
                        } else {
                            target.get_mut(int_part as usize)
                        }
                    } else {
                        None
                    }
                } else if let Value::Obj(target) = target {
                    if let Value::String(idx) = idx {
                        target.get_mut(&idx)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            ast::ExprKind::Binary(binary_expr) => {
                let key = if let ast::ExprKind::Var(var_expr) = &binary_expr.right.kind {
                    self.file.lexeme(&var_expr.ident.span)
                } else {
                    return None;
                };
                let obj = if let Some(value) = self.lvalue_reference(&*binary_expr.left) {
                    value
                } else {
                    return None;
                };

                if let Value::Obj(obj) = obj {
                    obj.get_mut(&key)
                } else {
                    None
                }
            }
            _ => unimplemented!(),
        }
    }

    fn interpret_expr(&mut self, expr: &ast::Expr) -> Value<'a> {
        match &expr.kind {
            ast::ExprKind::FunLit(fun_lit) => {
                let call = |interpreter: &mut Self, fun_lit: &ast::FunLit, args: Vec<Value<'a>>| {
                    interpreter.call_stack.push(CallStackFrame {
                        return_adr: interpreter.expr_pc,
                    });

                    let fun_arg_names = fun_lit
                        .parameters
                        .iter()
                        .map(|token| interpreter.file.lexeme(&token.span))
                        .collect::<Vec<_>>();

                    interpreter.namespace.nest();

                    for (i, arg) in args.iter().enumerate() {
                        interpreter
                            .namespace
                            .insert(fun_arg_names[i].to_string(), arg.clone());
                    }

                    interpreter.return_value = Some(interpreter.interpret_expr(&*fun_lit.body));
                    if let Some(function_call_frame) = interpreter.call_stack.pop() {
                        // Meaning an early return has not occured
                        interpreter.expr_pc = function_call_frame.return_adr;
                        interpreter.namespace.unnest();
                    }
                };

                Value::Fun(FunObj {
                    fun_lit: fun_lit.clone(),
                    call,
                })
            }
            ast::ExprKind::VarDecl(var_decl) => {
                let init_value = self.interpret_expr(&*var_decl.init);
                self.namespace.insert(
                    self.file.lexeme(&var_decl.ident.span).to_string(),
                    init_value,
                );

                self.namespace
                    .get(&self.file.lexeme(&var_decl.ident.span))
                    .unwrap()
                    .clone()
            }
            ast::ExprKind::WhileStmt(while_stmt) => {
                let mut cond = self.interpret_expr(&*while_stmt.condition);
                while cond.is_truthy() {
                    self.interpret_expr(&ast::Expr {
                        kind: while_stmt.block.clone().into(),
                        span: 0..0,
                    });
                    cond = self.interpret_expr(&*while_stmt.condition);
                }

                Value::Null
            }
            ast::ExprKind::ReturnStmt(ret_stmt) => {
                self.return_value = Some(self.interpret_expr(&ret_stmt.value));
                self.expr_pc = self.call_stack.pop().unwrap().return_adr;
                self.namespace.unnest();
                Value::Null
            }

            ast::ExprKind::If(if_stmt) => {
                let cond = self.interpret_expr(&*if_stmt.condition);
                if cond.is_truthy() {
                    self.interpret_expr(&ast::Expr {
                        kind: if_stmt.if_block.clone().into(),
                        span: 0..0,
                    })
                } else {
                    for elif_stmt in &if_stmt.elif_stmts {
                        let elif_cond = self.interpret_expr(&elif_stmt.0);
                        if elif_cond.is_truthy() {
                            return self.interpret_expr(&ast::Expr {
                                kind: elif_stmt.1.clone().into(),
                                span: 0..0,
                            });
                        }
                    }

                    if let Some(else_block) = &if_stmt.else_block {
                        self.interpret_expr(&ast::Expr {
                            kind: else_block.clone().into(),
                            span: 0..0,
                        })
                    } else {
                        Value::Null
                    }
                }
            }
            ast::ExprKind::Block(block) => {
                let mut block_result = None;
                for (i, expr) in block.stmts.iter().enumerate() {
                    let evaluated = self.interpret_expr(expr);
                    if i == block.stmts.len() - 1 {
                        block_result = Some(evaluated)
                    }
                }

                if let Some(value) = block_result {
                    value
                } else {
                    Value::Null
                }
            }

            ast::ExprKind::Unary(unary_expr) => {
                let value = self.interpret_expr(&*unary_expr.expr);
                match unary_expr.op.kind {
                    token::TokenKind::Minus => {
                        if let Value::Number(number) = value {
                            Value::Number(-number)
                        } else {
                            Value::Null
                        }
                    }
                    token::TokenKind::Bang => {
                        if value.is_truthy() {
                            Value::False
                        } else {
                            Value::True
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            ast::ExprKind::Binary(binary_expr) => {
                if binary_expr.op.kind == token::TokenKind::Equal {
                    let new_value = self.interpret_expr(&*binary_expr.right);
                    let lvalue = self.lvalue_reference(&*binary_expr.left);
                    return if let Some(lvalue) = lvalue {
                        *lvalue = new_value.clone();
                        new_value
                    } else {
                        Value::Null
                    };
                }

                let left_value = self.interpret_expr(&*binary_expr.left);
                match binary_expr.op.kind {
                    token::TokenKind::Dot => {
                        if let Value::Obj(obj) = left_value {
                            if let ast::ExprKind::Var(var_expr) = &binary_expr.right.kind {
                                let field_name = self.file.lexeme(&var_expr.ident.span);
                                if let Some(value) = obj.get(&field_name) {
                                    value.clone()
                                } else {
                                    Value::Null
                                }
                            } else {
                                Value::Null
                            }
                        } else {
                            Value::Null
                        }
                    }

                    token::TokenKind::Plus => {
                        if let Value::Number(left_number) = &left_value {
                            let right_value = self.interpret_expr(&*binary_expr.right);
                            if let Value::Number(right_number) = &right_value {
                                Value::Number(left_number + right_number)
                            } else {
                                // number + anything that's not a number == null
                                Value::Null
                            }
                        } else if let Value::String(left_string) = &left_value {
                            let right_value = self.interpret_expr(&*binary_expr.right);
                            // implicitly convert the right side to a string
                            Value::String(left_string.clone() + &right_value.to_string())
                        } else {
                            Value::Null
                        }
                    }
                    token::TokenKind::Minus
                    | token::TokenKind::Star
                    | token::TokenKind::Slash
                    | token::TokenKind::Percent
                    | token::TokenKind::Lesser
                    | token::TokenKind::Greater
                    | token::TokenKind::LesserEqual
                    | token::TokenKind::GreaterEqual => {
                        // only number operators
                        if let Value::Number(left_number) = &left_value {
                            let right_value = self.interpret_expr(&*binary_expr.right);
                            if let Value::Number(right_number) = &right_value {
                                match binary_expr.op.kind {
                                    token::TokenKind::Lesser => {
                                        if left_number < right_number {
                                            Value::True
                                        } else {
                                            Value::False
                                        }
                                    }
                                    token::TokenKind::Greater => {
                                        if left_number > right_number {
                                            Value::True
                                        } else {
                                            Value::False
                                        }
                                    }
                                    token::TokenKind::LesserEqual => {
                                        if left_number <= right_number {
                                            Value::True
                                        } else {
                                            Value::False
                                        }
                                    }
                                    token::TokenKind::GreaterEqual => {
                                        if left_number >= right_number {
                                            Value::True
                                        } else {
                                            Value::False
                                        }
                                    }

                                    token::TokenKind::Minus => {
                                        Value::Number(left_number - right_number)
                                    }
                                    token::TokenKind::Star => {
                                        Value::Number(left_number * right_number)
                                    }
                                    token::TokenKind::Slash => {
                                        Value::Number(left_number / right_number)
                                    }
                                    token::TokenKind::Percent => {
                                        Value::Number(left_number % right_number)
                                    }
                                    _ => unreachable!(),
                                }
                            } else {
                                // number op anything that's not a number == null
                                Value::Null
                            }
                        } else {
                            Value::Null
                        }
                    }

                    token::TokenKind::EqualEqual => {
                        let right_value = self.interpret_expr(&*binary_expr.right);
                        if left_value == right_value {
                            Value::True
                        } else {
                            Value::False
                        }
                    }
                    token::TokenKind::BangEqual => {
                        let right_value = self.interpret_expr(&*binary_expr.right);
                        if left_value != right_value {
                            Value::True
                        } else {
                            Value::False
                        }
                    }

                    token::TokenKind::AndAnd => {
                        if left_value.is_truthy() {
                            let right_value = self.interpret_expr(&*binary_expr.right);
                            if right_value.is_truthy() {
                                Value::True
                            } else {
                                Value::False
                            }
                        } else {
                            Value::False
                        }
                    }
                    token::TokenKind::OrOr => {
                        let right_value = self.interpret_expr(&*binary_expr.right);
                        if left_value.is_truthy() || right_value.is_truthy() {
                            Value::True
                        } else {
                            Value::False
                        }
                    }

                    _ => unimplemented!(),
                }
            }
            ast::ExprKind::Idx(idx_expr) => {
                let target_value = self.interpret_expr(&*idx_expr.target);
                let idx_value = self.interpret_expr(&*idx_expr.idx);

                if let Value::Arr(vec) = target_value {
                    if let Value::Number(idx) = idx_value {
                        let int_part = idx.trunc();
                        if int_part != idx {
                            Value::Null
                        } else {
                            vec[int_part as usize].clone()
                        }
                    } else {
                        Value::Null
                    }
                } else if let Value::Obj(obj) = target_value {
                    if let Value::String(idx) = idx_value {
                        obj.get(&idx).unwrap_or(&Value::Null).clone()
                    } else {
                        Value::Null
                    }
                } else {
                    Value::Null
                }
            }
            ast::ExprKind::Var(var_expr) => {
                let var_name = self.file.lexeme(&var_expr.ident.span);
                if let Some(value) = self.namespace.get(&var_name) {
                    value.clone()
                } else {
                    Value::Null
                }
            }
            ast::ExprKind::Call(call_expr) => {
                let callee = self.interpret_expr(&*call_expr.callee);

                if let Value::Fun(fun_obj) = callee {
                    let interpretd_args = call_expr
                        .args
                        .iter()
                        .map(|expr| self.interpret_expr(expr))
                        .collect::<Vec<_>>();

                    fun_obj.call(self, interpretd_args);
                    self.return_value.as_ref().unwrap().clone()
                } else {
                    Value::Null
                }
            }
            ast::ExprKind::ObjectLit(obj_lit) => {
                let mut obj_map = HashMap::new();
                for (token, value) in &obj_lit.inits {
                    let key_name = if token.kind == token::TokenKind::String {
                        let string_with_quotes = self.file.lexeme(&token.span);
                        string_with_quotes[1..string_with_quotes.len() - 1].to_string()
                    } else if token.kind == token::TokenKind::Ident {
                        self.file.lexeme(&token.span)
                    } else {
                        "".to_string()
                    };
                    obj_map.insert(key_name, self.interpret_expr(value));
                }

                Value::Obj(obj_map)
            }
            ast::ExprKind::ArrLit(arr_lit) => {
                let evaluated_elements = arr_lit
                    .elements
                    .iter()
                    .map(|expr| self.interpret_expr(expr))
                    .collect::<Vec<_>>();

                Value::Arr(evaluated_elements)
            }
            ast::ExprKind::Lit(lit) => match &lit.token.kind {
                token::TokenKind::Int => {
                    let string_value_of_int = self.file.lexeme(&lit.token.span).to_string();
                    let int_value = string_value_of_int.parse::<i64>().unwrap();
                    Value::Number(int_value as f64)
                }
                token::TokenKind::Float => {
                    let string_value_of_float = self.file.lexeme(&lit.token.span).to_string();
                    Value::Number(string_value_of_float.parse::<f64>().unwrap())
                }
                token::TokenKind::String => {
                    let string_value = self.file.lexeme(&lit.token.span).to_string();
                    Value::String(string_value)
                }
                token::TokenKind::Null => Value::Null,
                token::TokenKind::True => Value::True,
                token::TokenKind::False => Value::False,
                _ => unreachable!(),
            },
        }
    }

    fn interpret(&mut self) {
        self.namespace.insert(
            "println".to_string(),
            Value::Fun(FunObj {
                fun_lit: ast::FunLit {
                    parameters: vec![],
                    body: Box::new(ast::Expr {
                        kind: ast::Block { stmts: vec![] }.into(),
                        span: 0..0,
                    }),
                },
                call: |interpreter: &mut Self, _: &ast::FunLit, args: Vec<Value<'a>>| {
                    interpreter.call_stack.push(CallStackFrame {
                        return_adr: interpreter.expr_pc,
                    });

                    println!("{}", args[0].to_string());

                    interpreter.return_value = Some(Value::Null);
                    interpreter.expr_pc = interpreter.call_stack.pop().unwrap().return_adr;
                },
            }),
        );

        self.expr_pc = 0;
        while let Some(expr) = self.file.exprs.get(self.expr_pc) {
            self.interpret_expr(&expr);
            self.expr_pc += 1;
        }

        self.namespace.unnest();
    }
}

pub fn interpret(file: &ast::File) {
    let mut interpreter = Interpreter {
        file,
        namespace: ScopeStack::new(),

        expr_pc: 0,
        return_value: None,
        call_stack: Vec::new(),
    };
    interpreter.interpret();
}
