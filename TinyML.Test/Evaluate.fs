namespace TinyML.Test

open TinyML
open Ast

module Evaluate =

    let mutable tenv = []
    let mutable venv = []

    let evaluate line = 
        let x, (t, v) =
            match Main.parse_from_string line "LINE" Parser.interactive with 
            | IExpr e ->
                "it", Main.interpret_expr tenv venv e

            | IBinding (_, x, _, _ as b) ->
                let t, v = Main.interpret_expr tenv venv (LetIn (b, Var x))
                let tmp = Forall(Set.empty, t)
                tenv <- (x, tmp) :: tenv
                venv <- (x, v) :: venv
                x, (t, v)
        x, pretty_ty t, pretty_value v

