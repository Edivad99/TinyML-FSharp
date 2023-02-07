(*
* TinyML
* Eval.fs: evaluator
*)

module TinyML.Eval

open Ast

let rec eval_expr (env : value env) (e : expr) : value =
    match e with
    | Lit lit -> VLit lit

    | Var x ->
        let _, v = List.find (fun (y, _) -> x = y) env
        v

    | Lambda (x, _, e) -> Closure (env, x, e)

    | App (e1, e2) ->
        let v1 = eval_expr env e1
        let v2 = eval_expr env e2
        match v1 with
        | Closure (env1, x, e) -> eval_expr ((x, v2) :: env1) e
        | RecClosure (env1, f, x, e) -> eval_expr ((x, v2) :: (f, v1) :: env1) e
        | _ -> unexpected_error "eval_expr: non-closure in left side of application: %s" (pretty_value v1)
        
    | IfThenElse (e1, e2, None) ->
        let v1 = eval_expr env e1
        match v1 with
        | VLit (LBool true) -> eval_expr env e2
        | VLit (LBool false) -> VLit LUnit
        | _ -> unexpected_error "eval_expr: non-boolean in if guard: %s" (pretty_value v1)
        

    | IfThenElse (e1, e2, Some e3) ->
        let v1 = eval_expr env e1
        eval_expr env (match v1 with
                       | VLit (LBool true) -> e2
                       | VLit (LBool false) -> e3
                       | _ -> unexpected_error "eval_expr: non-boolean in if guard: %s" (pretty_value v1)
                       )

    | Let (x, _, e1, e2) -> 
        let v1 = eval_expr env e1
        eval_expr ((x, v1) :: env) e2

    // TODO: test this is ok or fix it
    | LetRec (f, _, e1, e2) -> 
        let v1 = eval_expr env e1
        match v1 with
        | Closure (venv1, x, e) -> eval_expr ((f, RecClosure (venv1, f, x, e)) :: env) e2
        | _ -> unexpected_error "eval_expr: expected closure in rec binding but got: %s" (pretty_value v1)
    // TODO: implement other binary ops
    | Tuple es -> VTuple (List.map (fun e -> eval_expr env e) es)
    | BinOp (e1, "+", e2) -> arithmetic_op(+) (+) env e1 e2
    | BinOp (e1, "-", e2) -> arithmetic_op (-) (-) env e1 e2
    | BinOp (e1, "*", e2) -> arithmetic_op ( * ) ( * ) env e1 e2
    | BinOp (e1, "/", e2) -> arithmetic_op (/) (/) env e1 e2
    | BinOp (e1, "%", e2) -> arithmetic_op (%) (%) env e1 e2
    | BinOp (e1, ">", e2) -> bool_op_num (>) (>) env e1 e2
    | BinOp (e1, ">=", e2) -> bool_op_num (>=) (>=) env e1 e2
    | BinOp (e1, "<", e2) -> bool_op_num (<) (<) env e1 e2
    | BinOp (e1, "<=", e2) -> bool_op_num (<=) (<=) env e1 e2
    | BinOp (e1, "=", e2) -> bool_op_equal_num (=) env e1 e2
    | BinOp (e1, "<>", e2) -> bool_op_equal_num (<>) env e1 e2
    | BinOp (e1, "and", e2) -> bool_op (&&) env e1 e2
    | BinOp (e1, "or", e2) -> bool_op (||) env e1 e2
    | UnOp ("not", e) -> un_op_not (not) env e

    | _ -> unexpected_error "eval_expr: unsupported expression: %s [AST: %A]" (pretty_expr e) e

// Sometimes functions are mutually recursive, meaning that calls form a circle, where one function calls
// another which in turn calls the first, with any number of calls in between.
// You must define such functions together in one let binding, using the and keyword to link them together.
and arithmetic_op op_int op_float env e1 e2 =
    let v1 = eval_expr env e1
    let v2 = eval_expr env e2
    match v1, v2 with
    | VLit (LInt x), VLit (LInt y) -> VLit (LInt (op_int x y))
    | VLit (LFloat x), VLit (LFloat y) -> VLit (LFloat (op_float x y))
    | VLit (LInt x), VLit (LFloat y) -> VLit (LFloat (op_float (float x) y))
    | VLit (LFloat x), VLit (LInt y) -> VLit (LFloat (op_float x (float y)))
    | _ -> unexpected_error "eval_expr: illegal operands in binary operator (+): %s + %s" (pretty_value v1) (pretty_value v2)

and bool_op_num op_int op_float env e1 e2 =
    let v1 = eval_expr env e1
    let v2 = eval_expr env e2
    match v1, v2 with
    | VLit (LInt x), VLit (LInt y) -> VLit (LBool (op_int x y))
    | VLit (LFloat x), VLit (LFloat y) -> VLit (LBool (op_float x y))
    | VLit (LInt x), VLit (LFloat y) -> VLit (LBool (op_float (float x) y))
    | VLit (LFloat x), VLit (LInt y) -> VLit (LBool (op_float x (float y)))
    | _ -> unexpected_error $"eval_expr: illegal operands in binary operator (+): {pretty_value v1} + {pretty_value v2}"

and bool_op_equal_num op env e1 e2 =
    let v1 = eval_expr env e1
    let v2 = eval_expr env e2
    match v1, v2 with
    | VLit (LInt x), VLit (LInt y) -> VLit (LBool (op x y))
    | _ -> unexpected_error $"eval_expr: The =/<> operator works only on int"

and bool_op op env e1 e2 =
    let v1 = eval_expr env e1
    let v2 = eval_expr env e2
    match v1, v2 with
    | VLit (LBool x), VLit (LBool y) -> VLit (LBool (op x y))
    | _ -> unexpected_error $"eval_expr: The and/or operator works only on bool"

and un_op_not op env e =
    let v = eval_expr env e
    match v with
    | VLit (LBool x) -> VLit (LBool (op x))
    | _ -> unexpected_error "eval_expr: The not operator works only on bool"
