use rand::Rng;
use std::{collections::HashMap, fmt::Debug, fs, process::exit};
use tiny_http::{Response, Server};
use unicode_segmentation::UnicodeSegmentation;

use crate::{ast, lexer, parser, token};

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

    Err(Box<Value<'a>>),
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
            Value::Err(_) => false,
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
            Value::Err(value) => format!("error<{}>", value.to_string()),
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
    id: usize,
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
                            let int_part = int_part as usize;
                            if int_part >= target.len() {
                                for _ in target.len() - 1..int_part {
                                    target.push(Value::Null);
                                }
                            }
                            target.get_mut(int_part as usize)
                        }
                    } else {
                        None
                    }
                } else if let Value::Obj(target) = target {
                    if let Value::String(idx) = idx {
                        if !target.contains_key(&idx) {
                            target.insert(idx.clone(), Value::Null);
                        }
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

    fn interpret_call(&mut self, call_expr: &ast::CallExpr, panic_on_err: bool) -> Value<'a> {
        let callee = self.interpret_expr(&*call_expr.callee);

        if let Value::Fun(fun_obj) = callee {
            let interpreted_args = call_expr
                .args
                .iter()
                .map(|expr| self.interpret_expr(expr))
                .collect::<Vec<_>>();

            fun_obj.call(self, interpreted_args);

            if panic_on_err {
                if let Some(return_value) = &self.return_value {
                    if let Value::Err(err) = return_value {
                        self.panic(&*err);
                    }
                }
            }

            self.return_value.as_ref().unwrap().clone()
        } else {
            Value::Null
        }
    }

    fn panic(&self, value: &Value) -> ! {
        println!("{}", value.to_string());
        exit(1);
    }

    fn interpret_expr(&mut self, expr: &ast::Expr) -> Value<'a> {
        match &expr.kind {
            ast::ExprKind::FunLit(fun_lit) => {
                let call = |interpreter: &mut Self, fun_lit: &ast::FunLit, args: Vec<Value<'a>>| {
                    let return_adr = interpreter.expr_pc;
                    let call_frame_id = interpreter.call_stack.len();
                    interpreter.call_stack.push(CallStackFrame {
                        id: call_frame_id,
                        return_adr,
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

                    if let ast::ExprKind::Block(block_expr) = &fun_lit.body.kind {
                        for (i, expr) in block_expr.stmts.iter().enumerate() {
                            let value = interpreter.interpret_expr(expr);
                            if let Some(last_call_frame) = interpreter.call_stack.last() {
                                if last_call_frame.id != call_frame_id {
                                    // Early return just happened
                                    break;
                                } else if i == block_expr.stmts.len() - 1 {
                                    interpreter.return_value = Some(value);
                                    // Explicit return has not occured
                                    interpreter.expr_pc = return_adr;
                                    interpreter.namespace.unnest();
                                }
                            } else {
                                break;
                            }
                        }
                    } else {
                        interpreter.return_value = Some(interpreter.interpret_expr(&*fun_lit.body));
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
            ast::ExprKind::ForStmt(for_stmt) => {
                let iterable = self.interpret_expr(&*for_stmt.iterable);
                match &iterable {
                    Value::String(string) => {
                        for grapheme in string.graphemes(true) {
                            self.namespace.nest();
                            self.namespace.insert(
                                self.file.lexeme(&for_stmt.ident.span),
                                Value::String(grapheme.to_string()),
                            );
                            self.interpret_expr(&ast::Expr {
                                kind: for_stmt.block.clone().into(),
                                span: 0..0,
                            });
                            self.namespace.unnest();
                        }
                    }
                    Value::Arr(arr) => {
                        for el in arr {
                            self.namespace.nest();
                            self.namespace
                                .insert(self.file.lexeme(&for_stmt.ident.span), el.clone());
                            self.interpret_expr(&ast::Expr {
                                kind: for_stmt.block.clone().into(),
                                span: 0..0,
                            });
                            self.namespace.unnest();
                        }
                    }
                    Value::Obj(obj) => {
                        for el in obj {
                            self.namespace.nest();
                            self.namespace.insert(
                                self.file.lexeme(&for_stmt.ident.span),
                                Value::Arr(vec![Value::String(el.0.clone()), el.1.clone()]),
                            );
                            self.interpret_expr(&ast::Expr {
                                kind: for_stmt.block.clone().into(),
                                span: 0..0,
                            });
                            self.namespace.unnest();
                        }
                    }
                    _ => {}
                }

                Value::Null
            }
            ast::ExprKind::ReturnStmt(ret_stmt) => {
                self.return_value = Some(self.interpret_expr(&ret_stmt.value));
                // dbg!(&self.return_value);
                if ret_stmt.err {
                    self.return_value = Some(Value::Err(Box::new(
                        self.return_value.as_ref().unwrap().clone(),
                    )));
                }
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
            ast::ExprKind::Call(call_expr) => self.interpret_call(call_expr, true),
            ast::ExprKind::Catch(catch_expr) => {
                // Error values can only be generated by a function call
                if let ast::ExprKind::Call(call_expr) = &catch_expr.target.kind {
                    let ret_value = self.interpret_call(call_expr, false);
                    if let Value::Err(err_value) = ret_value {
                        let err_callback = self.interpret_expr(&ast::Expr {
                            kind: ast::ExprKind::FunLit(catch_expr.callback.clone()),
                            span: 0..0,
                        });
                        if let Value::Fun(fun_obj) = err_callback {
                            fun_obj.call(self, vec![*err_value.clone()]);
                            if let Value::Err(err) = self.return_value.as_ref().unwrap() {
                                self.panic(&*err);
                            }
                            self.return_value.as_ref().unwrap().clone()
                        } else {
                            unreachable!()
                        }
                    } else {
                        ret_value
                    }
                } else {
                    Value::Null
                }
            }
            ast::ExprKind::ObjectLit(obj_lit) => {
                let mut obj_map = HashMap::new();
                for (token, value) in &obj_lit.inits {
                    let key_name = if token.kind == token::TokenKind::String {
                        self.file.lexeme(&token.span).to_string()
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
        let builtins = [
            // len(iterable: array | object | string): number
            (
                "len".to_string(),
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
                            id: interpreter.call_stack.len(),
                            return_adr: interpreter.expr_pc,
                        });

                        interpreter.return_value = Some(if let Some(value) = args.get(0) {
                            match value {
                                Value::String(string) => Value::Number(
                                    string.graphemes(true).collect::<Vec<_>>().len() as f64,
                                ),
                                Value::Arr(arr) => Value::Number(arr.len() as f64),
                                Value::Obj(obj) => Value::Number(obj.len() as f64),
                                _ => Value::Err(Box::new(Value::String(
                                    "argument must be array or object".to_string(),
                                ))),
                            }
                        } else {
                            Value::Err(Box::new(Value::String("expected argument".to_string())))
                        });

                        interpreter.expr_pc = interpreter.call_stack.pop().unwrap().return_adr;
                    },
                }),
            ),
            // enumerate(iterable: array | object | string): array
            (
                "enumerate".to_string(),
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
                            id: interpreter.call_stack.len(),
                            return_adr: interpreter.expr_pc,
                        });

                        interpreter.return_value = Some(if let Some(value) = args.get(0) {
                            match value {
                                Value::String(string) => Value::Arr(
                                    string
                                        .graphemes(true)
                                        .enumerate()
                                        .map(|(i, string)| {
                                            Value::Arr(vec![
                                                Value::Number(i as f64),
                                                Value::String(string.to_string()),
                                            ])
                                        })
                                        .collect(),
                                ),
                                Value::Arr(arr) => Value::Arr(
                                    arr.iter()
                                        .enumerate()
                                        .map(|(i, value)| {
                                            Value::Arr(vec![Value::Number(i as f64), value.clone()])
                                        })
                                        .collect(),
                                ),
                                Value::Obj(obj) => Value::Arr(
                                    obj.iter()
                                        .enumerate()
                                        .map(|(i, value)| {
                                            Value::Arr(vec![
                                                Value::Number(i as f64),
                                                Value::Arr(vec![
                                                    Value::String(value.0.clone()),
                                                    value.1.clone(),
                                                ]),
                                            ])
                                        })
                                        .collect(),
                                ),
                                _ => Value::Err(Box::new(Value::String(
                                    "argument must be array or object".to_string(),
                                ))),
                            }
                        } else {
                            Value::Err(Box::new(Value::String("expected argument".to_string())))
                        });

                        interpreter.expr_pc = interpreter.call_stack.pop().unwrap().return_adr;
                    },
                }),
            ),
            // keys(obj: object): array
            (
                "keys".to_string(),
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
                            id: interpreter.call_stack.len(),
                            return_adr: interpreter.expr_pc,
                        });

                        interpreter.return_value = Some(if let Some(first_arg) = args.get(0) {
                            if let Value::Obj(obj) = first_arg {
                                Value::Arr(
                                    obj.keys()
                                        .cloned()
                                        .map(|string| Value::String(string))
                                        .collect(),
                                )
                            } else {
                                Value::Err(Box::new(Value::String(
                                    "first argument must be object".to_string(),
                                )))
                            }
                        } else {
                            Value::Err(Box::new(Value::String(
                                "expected first argument".to_string(),
                            )))
                        });

                        interpreter.expr_pc = interpreter.call_stack.pop().unwrap().return_adr;
                    },
                }),
            ),
            // values(obj: object): array
            (
                "values".to_string(),
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
                            id: interpreter.call_stack.len(),
                            return_adr: interpreter.expr_pc,
                        });

                        interpreter.return_value = Some(if let Some(first_arg) = args.get(0) {
                            if let Value::Obj(obj) = first_arg {
                                Value::Arr(obj.values().cloned().collect())
                            } else {
                                Value::Err(Box::new(Value::String(
                                    "first argument must be object".to_string(),
                                )))
                            }
                        } else {
                            Value::Err(Box::new(Value::String(
                                "expected first argument".to_string(),
                            )))
                        });

                        interpreter.expr_pc = interpreter.call_stack.pop().unwrap().return_adr;
                    },
                }),
            ),
            // type(value: any): string
            (
                "type".to_string(),
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
                            id: interpreter.call_stack.len(),
                            return_adr: interpreter.expr_pc,
                        });

                        interpreter.return_value = Some(Value::String(
                            match args[0] {
                                Value::Null => "null",
                                Value::True | Value::False => "boolean",
                                Value::Number(_) => "number",
                                Value::String(_) => "string",
                                Value::Err(_) => "error",
                                Value::Fun(_) => "function",
                                Value::Arr(_) => "array",
                                Value::Obj(_) => "object",
                            }
                            .to_string(),
                        ));

                        interpreter.expr_pc = interpreter.call_stack.pop().unwrap().return_adr;
                    },
                }),
            ),
            // eval(code: string)
            (
                "eval".to_string(),
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
                            id: interpreter.call_stack.len(),
                            return_adr: interpreter.expr_pc,
                        });

                        if let Value::String(code) = &args[0] {
                            let mut file = ast::File {
                                source: code.chars().collect(),
                                exprs: vec![],
                            };

                            let mut tokenizer = lexer::Lexer::from_chars(file.source.clone());
                            match tokenizer.lex() {
                                Ok(tokens) => {
                                    file.source = tokenizer.source;
                                    match parser::parse(&tokens) {
                                        Ok(exprs) => {
                                            file.exprs = exprs;
                                            interpret(&file);
                                            // TODO: get final expr value and return it
                                        }
                                        Err(_) => interpreter.return_value = Some(Value::Null),
                                    };
                                }
                                Err(_) => interpreter.return_value = Some(Value::Null),
                            };
                        }

                        interpreter.return_value = Some(Value::Null);
                        interpreter.expr_pc = interpreter.call_stack.pop().unwrap().return_adr;
                    },
                }),
            ),
            // print(value: any)
            (
                "print".to_string(),
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
                            id: interpreter.call_stack.len(),
                            return_adr: interpreter.expr_pc,
                        });

                        println!("{}", args[0].to_string());

                        interpreter.return_value = Some(Value::Null);
                        interpreter.expr_pc = interpreter.call_stack.pop().unwrap().return_adr;
                    },
                }),
            ),
            // read_file(path: string): string
            (
                "read_file".to_string(),
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
                            id: interpreter.call_stack.len(),
                            return_adr: interpreter.expr_pc,
                        });

                        if let Value::String(path) = &args[0] {
                            interpreter.return_value = Some(match fs::read_to_string(path) {
                                Ok(contents) => Value::String(contents),
                                Err(err) => Value::Err(Box::new(Value::String(err.to_string()))),
                            });
                        } else {
                            interpreter.return_value = Some(Value::Err(Box::new(Value::String(
                                "path must be string".to_string(),
                            ))));
                        }

                        interpreter.expr_pc = interpreter.call_stack.pop().unwrap().return_adr;
                    },
                }),
            ),
            // write_file(path: string, contents: string)
            (
                "write_file".to_string(),
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
                            id: interpreter.call_stack.len(),
                            return_adr: interpreter.expr_pc,
                        });

                        if let Value::String(path) = &args[0] {
                            if let Value::String(contents) = &args[1] {
                                interpreter.return_value =
                                    Some(if let Err(err) = fs::write(path, contents) {
                                        Value::Err(Box::new(Value::String(err.to_string())))
                                    } else {
                                        Value::Null
                                    });
                            } else {
                                interpreter.return_value = Some(Value::Err(Box::new(
                                    Value::String("contents must be string".to_string()),
                                )));
                            }
                        } else {
                            interpreter.return_value = Some(Value::Err(Box::new(Value::String(
                                "path must be string".to_string(),
                            ))));
                        }

                        interpreter.expr_pc = interpreter.call_stack.pop().unwrap().return_adr;
                    },
                }),
            ),
            // serve(callback: (request: object) -> string, options: object)
            (
                "serve".to_string(),
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
                            id: interpreter.call_stack.len(),
                            return_adr: interpreter.expr_pc,
                        });

                        let options = if let Some(options) = args.get(1) {
                            if let Value::Obj(options) = options {
                                Some(options)
                            } else {
                                None
                            }
                        } else {
                            None
                        };

                        match Server::http(&format!(
                            "0.0.0.0:{}",
                            if let Some(options) = options {
                                if let Some(port_value) = options.get("port") {
                                    if let Value::Number(port) = port_value {
                                        *port as u64
                                    } else {
                                        8080
                                    }
                                } else {
                                    8080
                                }
                            } else {
                                8080
                            }
                        )) {
                            Ok(server) => {
                                for request in server.incoming_requests() {
                                    let mut request_obj = HashMap::new();
                                    request_obj.insert(
                                        "method".to_string(),
                                        Value::String(
                                            match request.method() {
                                                tiny_http::Method::Get => "GET",
                                                tiny_http::Method::Head => "HEAD",
                                                tiny_http::Method::Post => "POST",
                                                tiny_http::Method::Put => "PUT",
                                                tiny_http::Method::Delete => "DELETE",
                                                tiny_http::Method::Connect => "CONNECT",
                                                tiny_http::Method::Options => "OPTIONS",
                                                tiny_http::Method::Trace => "TRACE",
                                                tiny_http::Method::Patch => "PATCH",
                                                tiny_http::Method::NonStandard(_) => todo!(),
                                            }
                                            .to_string(),
                                        ),
                                    );
                                    request_obj.insert(
                                        "url".to_string(),
                                        Value::String(request.url().to_string()),
                                    );
                                    request_obj.insert(
                                        "headers".to_string(),
                                        Value::Arr(
                                            request
                                                .headers()
                                                .iter()
                                                .map(|header| {
                                                    let mut obj = HashMap::new();
                                                    obj.insert(
                                                        header.field.as_str().to_string(),
                                                        Value::String(header.value.to_string()),
                                                    );
                                                    Value::Obj(obj)
                                                })
                                                .collect(),
                                        ),
                                    );

                                    if let Value::Fun(callback) = &args[0] {
                                        callback.call(interpreter, vec![Value::Obj(request_obj)]);
                                        if let Some(Value::String(ref response_value)) =
                                            interpreter.return_value
                                        {
                                            let response =
                                                Response::from_string(response_value.to_string());

                                            if let Err(err) = request.respond(response) {
                                                interpreter.return_value = Some(Value::Err(
                                                    Box::new(Value::String(err.to_string())),
                                                ));
                                                break;
                                            };
                                        }
                                    }
                                }
                            }
                            Err(err) => {
                                interpreter.return_value =
                                    Some(Value::Err(Box::new(Value::String(err.to_string()))));
                            }
                        }

                        interpreter.expr_pc = interpreter.call_stack.pop().unwrap().return_adr;
                    },
                }),
            ),
            // rand(): number
            (
                "rand".to_string(),
                Value::Fun(FunObj {
                    fun_lit: ast::FunLit {
                        parameters: vec![],
                        body: Box::new(ast::Expr {
                            kind: ast::Block { stmts: vec![] }.into(),
                            span: 0..0,
                        }),
                    },
                    call: |interpreter: &mut Self, _: &ast::FunLit, _args: Vec<Value<'a>>| {
                        interpreter.call_stack.push(CallStackFrame {
                            id: interpreter.call_stack.len(),
                            return_adr: interpreter.expr_pc,
                        });

                        let mut rng = rand::thread_rng();
                        interpreter.return_value = Some(Value::Number(rng.gen::<f64>()));

                        interpreter.expr_pc = interpreter.call_stack.pop().unwrap().return_adr;
                    },
                }),
            ),
        ];

        for (name, builtin) in builtins {
            self.namespace.insert(name, builtin)
        }

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
