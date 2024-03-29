﻿(*
* TinyML
* Typing.fs: typing algorithms
*)

module TinyML.Typing

open Ast
open System

let type_error fmt = throw_formatted TypeError fmt

type subst = (tyvar * ty) list

// TODO implement this
let rec apply_subst (s : subst) (t : ty): ty =
    match t with
    | TyName _ -> t
    | TyArrow (t1, t2) -> TyArrow (apply_subst s t1, apply_subst s t2)
    | TyVar tv ->
        match List.tryFind (fun (tv1, _) -> tv1 = tv) s with
        | Some (_, t1) -> t1
        | None -> t
    | TyTuple ts -> TyTuple (List.map (apply_subst s) ts)

// TODO implement this
let compose_subst (s1 : subst) (s2 : subst) : subst =
    s1
    |> List.map (fun (y, x) -> (y, apply_subst s2 x))
    |> fun s1_applied -> List.append s1_applied s2
    |> List.distinctBy fst

let compose_multiple_subst ([<ParamArray>] subst) =
    List.fold (fun s1 s2 -> compose_subst s1 s2) [] subst

// TODO implement this
let rec unify (t1 : ty) (t2 : ty) : subst =
    match (t1, t2) with
    | TyName s1, TyName s2 when s1 = s2 -> []
    | TyVar tv, t
    | t, TyVar tv -> [tv, t]

    | TyArrow (t1, t2), TyArrow (t3, t4) ->
        let s = unify t1 t3
        s @ (unify (apply_subst s t2) (apply_subst s t4))
    | TyTuple ts1, TyTuple ts2 when List.length ts1 = List.length ts2 ->
        List.fold (fun s (t1, t2) -> compose_subst s (unify t1 t2)) [] (List.zip ts1 ts2)

    | _ -> type_error "cannot unify types %O and %O" t1 t2

let rec freevars_ty (t : ty) : tyvar Set =
    match t with
    | TyName _ -> Set.empty
    | TyVar tv -> Set.singleton tv
    | TyArrow (t1, t2) -> Set.union (freevars_ty t1) (freevars_ty t2)
    | TyTuple ts -> List.fold (fun set t -> Set.union set (freevars_ty t)) Set.empty ts

let freevars_scheme (Forall (tvs, t)) =
    Set.difference (freevars_ty t) (tvs)

let freevars_scheme_env env =
    List.fold (fun r (_, sch) -> Set.union r (freevars_scheme sch)) Set.empty env

let generalization (env : scheme env) (t : ty) : scheme =
    let tvs = Set.difference (freevars_ty t) (freevars_scheme_env env)
    Forall (tvs, t)

let apply_subst_to_env (subst : subst) (env : scheme env) : scheme env =
    List.map (fun (name, Forall (tvs, t)) ->
        (name, Forall (tvs, apply_subst subst t))
    ) env

// type inference

let gamma0 = [
    ("+", TyArrow (TyInt, TyArrow (TyInt, TyInt)))
    ("-", TyArrow (TyInt, TyArrow (TyInt, TyInt)))
    ("*", TyArrow (TyInt, TyArrow (TyInt, TyInt)))
    ("/", TyArrow (TyInt, TyArrow (TyInt, TyInt)))
    ("%", TyArrow (TyInt, TyArrow (TyInt, TyInt)))
    ("<", TyArrow (TyInt, TyArrow (TyInt, TyBool)))
    ("<=", TyArrow (TyInt, TyArrow (TyInt, TyBool)))
    (">", TyArrow (TyInt, TyArrow (TyInt, TyBool)))
    (">=", TyArrow (TyInt, TyArrow (TyInt, TyBool)))
    ("=", TyArrow (TyInt, TyArrow (TyInt, TyBool)))
    ("<>", TyArrow (TyInt, TyArrow (TyInt, TyBool)))

    ("and", TyArrow (TyBool, TyArrow(TyBool, TyBool)))
    ("or", TyArrow (TyBool, TyArrow(TyBool, TyBool)))

    ("not", TyArrow (TyBool, TyBool))
]

let gamma0_scheme_env =
    gamma0
    |> List.map (fun (name, t) -> (name, Forall (Set.empty, t)))

let mutable private var_counter = 0

// Used for testing
let reset_var_counter () = var_counter <- 0

let fresh_variable () =
    var_counter <- var_counter + 1
    //printfn $"New fresh variable of type {var_counter}"
    TyVar (var_counter)

let inst (Forall (tvs, t)) : ty =
    let free_vars = freevars_ty t
    let vars_to_be_refresh = Set.intersect free_vars tvs
    let sub =
        vars_to_be_refresh
        |> Set.toList
        |> List.map (fun v -> (v, (fresh_variable())))
    apply_subst sub t

// TODO for exam
let rec typeinfer_expr (env : scheme env) (e : expr) : ty * subst =
    match e with
    | Lit (LInt _) -> TyInt, []
    | Lit (LFloat _) -> TyFloat, []
    | Lit (LString _) -> TyString, []
    | Lit (LChar _) -> TyChar, []
    | Lit (LBool _) -> TyBool, []
    | Lit LUnit -> TyUnit, []

    | Var x -> 
        match List.tryFind(fun (var_name, _) -> var_name = x) env with
        | Some (_, schema) -> inst schema, []
        | _ -> type_error $"typeinfer_expr: variable {x} is not defined in the environment"

    | Lambda (x, tyo, e) ->
        let fresh_var = fresh_variable ()
        let te, s1 = typeinfer_expr ((x, Forall(Set.empty, fresh_var)) :: env) e
        let t1 = apply_subst s1 fresh_var

        let unify_with_user_type =
            match tyo with
            | Some t -> unify t1 t
            | None -> []

        let t2 = apply_subst unify_with_user_type t1
        TyArrow (t2, te), compose_subst unify_with_user_type s1

    | App (e1, e2) ->
        let t1, s1 = typeinfer_expr env e1
        let env = apply_subst_to_env s1 env
        let t2, s2 = typeinfer_expr env e2

        let fresh_var = fresh_variable ()
        let s3 = unify (apply_subst s2 t1) (TyArrow(t2, fresh_var))
        let res = compose_multiple_subst [s3; s2; s1]

        apply_subst res fresh_var, res

    | Let (x, tyo, e1, e2) ->
        let t1, s1 = typeinfer_expr env e1
        let sch = generalization env (apply_subst s1 t1)
        let env2 = apply_subst_to_env s1 ((x, sch) :: env)
        let t2, s2 = typeinfer_expr env2 e2
        match tyo with
        | Some (t2_user) when t2_user <> t2 -> type_error $"the expected type of this expression is {pretty_ty t2_user} but the actual one is {pretty_ty t2}"
        | _ -> t2, compose_subst s2 s1

    | LetRec (f, tyo, e1, e2) ->
        let fresh_var = fresh_variable ()
        let t1, s1 = typeinfer_expr ((f, Forall (Set.empty, fresh_var)) :: env) e1
        match tyo with
        | Some (t1_user) when t1_user <> t1 -> type_error $"let rec type mismatch the expected type of this expression is {pretty_ty t1_user} but the actual one is {pretty_ty t1}"
        | _ ->
            let sch = generalization env t1

            let local_env = apply_subst_to_env s1 env
            let t2, s2 = typeinfer_expr ((f, sch) :: local_env) e2
            t2, s2

    | IfThenElse (e1, e2, e3o) ->
        let t1, s1 = typeinfer_expr env e1
        let s2 = unify t1 TyBool

        let s3 = compose_subst s2 s1
        let env = apply_subst_to_env s3 env

        let t2, s4 = typeinfer_expr env e2
        let s5 = compose_subst s4 s3

        match e3o with
        | None ->
            if t2 <> TyUnit then
                type_error $"if-then without else requires unit type on then branch, but got {pretty_ty t2}"
            else
                TyUnit, s5
        | Some e3 ->
            let env = apply_subst_to_env s5 env
            let t3, s6 = typeinfer_expr env e3


            let s7 = compose_subst s6 s5
            let s8 = unify (apply_subst s7 t2) (apply_subst s7 t3)
            let t = apply_subst s8 t2
            t, compose_subst s8 s7

    | Tuple es ->
        es
        |> List.fold (fun ((tl, s) : ty list * subst) (e : expr) ->
            let local_env = apply_subst_to_env s env
            let ti, si = typeinfer_expr local_env e
            List.append tl [ti], compose_subst s si
        ) ([], [])
        |> fun (tl, s) -> TyTuple tl, s

    | BinOp (e1, op, e2) when List.exists (fun (x, _) -> x = op) env ->
        typeinfer_expr env (App (App (Var op, e1), e2))

    | UnOp (op, e) when List.exists (fun (x, _) -> x = op) env ->
        typeinfer_expr env (App (Var op, e))

    | _ -> failwithf "not implemented"

// type checker
let rec typecheck_expr (env : ty env) (e : expr) : ty =
    match e with
    | Lit (LInt _) -> TyInt
    | Lit (LFloat _) -> TyFloat
    | Lit (LString _) -> TyString
    | Lit (LChar _) -> TyChar
    | Lit (LBool _) -> TyBool
    | Lit LUnit -> TyUnit

    | Var x ->
        let _, t = List.find (fun (y, _) -> x = y) env
        t

    | Lambda (x, None, e) -> unexpected_error "typecheck_expr: unannotated lambda is not supported"

    | Lambda (x, Some t1, e) ->
        let t2 = typecheck_expr ((x, t1) :: env) e
        TyArrow (t1, t2)

    | App (e1, e2) ->
        let t1 = typecheck_expr env e1
        let t2 = typecheck_expr env e2
        match t1 with
        | TyArrow (l, r) ->
            if l = t2 then r 
            else type_error "wrong application: argument type %s does not match function domain %s" (pretty_ty t2) (pretty_ty l)
        | _ -> type_error "expecting a function on left side of application, but got %s" (pretty_ty t1)

    | Let (x, tyo, e1, e2) ->
        let t1 = typecheck_expr env e1
        match tyo with
        | None -> ()
        | Some t -> if t <> t1 then type_error "type annotation in let binding of %s is wrong: exptected %s, but got %s" x (pretty_ty t) (pretty_ty t1)
        typecheck_expr ((x, t1) :: env) e2

    | IfThenElse (e1, e2, e3o) ->
        let t1 = typecheck_expr env e1
        if t1 <> TyBool then type_error "if condition must be a bool, but got a %s" (pretty_ty t1)
        let t2 = typecheck_expr env e2
        match e3o with
        | None ->
            if t2 <> TyUnit then type_error "if-then without else requires unit type on then branch, but got %s" (pretty_ty t2)
            TyUnit
        | Some e3 ->
            let t3 = typecheck_expr env e3
            if t2 <> t3 then type_error "type mismatch in if-then-else: then branch has type %s and is different from else branch type %s" (pretty_ty t2) (pretty_ty t3)
            t2

    | Tuple es ->
        TyTuple (List.map (typecheck_expr env) es)

    | LetRec (f, None, e1, e2) ->
        unexpected_error "typecheck_expr: unannotated let rec is not supported"

    | LetRec (f, Some tf, e1, e2) ->
        let env0 = (f, tf) :: env
        let t1 = typecheck_expr env0 e1
        match t1 with
        | TyArrow _ -> ()
        | _ -> type_error "let rec is restricted to functions, but got type %s" (pretty_ty t1)
        if t1 <> tf then type_error "let rec type mismatch: expected %s, but got %s" (pretty_ty tf) (pretty_ty t1)
        typecheck_expr env0 e2

    | BinOp (e1, ("+" | "-" | "/" | "%" | "*" as op), e2) ->
        let t1 = typecheck_expr env e1
        let t2 = typecheck_expr env e2
        match t1, t2 with
        | TyInt, TyInt -> TyInt
        | TyFloat, TyFloat -> TyFloat
        | TyInt, TyFloat
        | TyFloat, TyInt -> TyFloat
        | _ -> type_error "binary operator expects two int operands, but got %s %s %s" (pretty_ty t1) op (pretty_ty t2)
        
    | BinOp (e1, ("<" | "<=" | ">" | ">=" | "=" | "<>" as op), e2) ->
        let t1 = typecheck_expr env e1
        let t2 = typecheck_expr env e2
        match t1, t2 with
        | TyInt, TyInt -> ()
        | _ -> type_error "binary operator expects two numeric operands, but got %s %s %s" (pretty_ty t1) op (pretty_ty t2)
        TyBool

    | BinOp (e1, ("and" | "or" as op), e2) ->
        let t1 = typecheck_expr env e1
        let t2 = typecheck_expr env e2
        match t1, t2 with
        | TyBool, TyBool -> ()
        | _ -> type_error "binary operator expects two bools operands, but got %s %s %s" (pretty_ty t1) op (pretty_ty t2)
        TyBool

    | BinOp (_, op, _) -> unexpected_error "typecheck_expr: unsupported binary operator (%s)" op

    | UnOp ("not", e) ->
        let t = typecheck_expr env e
        if t <> TyBool then type_error "unary not expects a bool operand, but got %s" (pretty_ty t)
        TyBool
            
    | UnOp ("-", e) ->
        let t = typecheck_expr env e
        match t with
        | TyInt -> TyInt
        | TyFloat -> TyFloat
        | _ -> type_error "unary negation expects a numeric operand, but got %s" (pretty_ty t)

    | UnOp (op, _) -> unexpected_error "typecheck_expr: unsupported unary operator (%s)" op

    | _ -> unexpected_error "typecheck_expr: unsupported expression: %s [AST: %A]" (pretty_expr e) e
