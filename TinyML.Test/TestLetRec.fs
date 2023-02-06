namespace TinyML.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open TinyML

[<TestClass>]
type TestLetRec () =

    [<TestInitialize>]
    member _.TestInitialize () =
        Evaluate.tenv <- []
        Evaluate.venv <- []
        Typing.reset_var_counter()

    [<TestMethod>]
    member _.TestLetRec () =
        let variable_name, ty, v = Evaluate.evaluate $"let rec f x = if x < 5 then 5 else f x + 1;;"
        Assert.AreEqual("f", variable_name)
        Assert.AreEqual("int -> int", ty)
        Assert.AreEqual("<|[];f;x;if x < 5 then 5 else f x + 1|>", v)

    [<TestMethod>]
    member _.TestLetRecFibonacci () =
        let variable_name, ty, v = Evaluate.evaluate $"""
            let rec fib x =
                if x < 3 then 1
                else fib (x - 1) + fib (x - 2);;
        """
        Assert.AreEqual("fib", variable_name)
        Assert.AreEqual("int -> int", ty)
        Assert.AreEqual("<|[];fib;x;if x < 3 then 1 else fib x - 1 + fib x - 2|>", v)

    [<TestMethod>]
    member _.RecursivePolymorphicFunctionAppliedMultipleOnATuple2 () =
        let variable_name, ty, v = Evaluate.evaluate $"""
            let z = 2 in
            let rec f x y = x (y + 1)
                in (
                    f (fun x -> x) 5,
                    f,
                    f (fun (x :int) -> if (x - 1) > 0 then x/z else x*z) -5,
                    f (fun (x :int) -> if x > 1 then ()) -5,
                    f (fun x -> "ciao"),
                    f
                );;
        """
        Assert.AreEqual("it", variable_name)
        Assert.AreEqual("(int, int -> '9 -> int -> '9, int, unit, int -> string, int -> '21 -> int -> '21)", ty)
        Assert.AreEqual("<|[z=2];f;x;fun y -> x y + 1|>", v)

    [<TestMethod>]
    member _.RecursivePolymorphicFunctionAppliedMultipleTimesLetBindings () =
        let variable_name, ty, v = Evaluate.evaluate $"""
            let z = 2 in
            let rec f x y = 
                x (y + 1)
            in let g = f (fun x -> x) 5
            in let w = f
            in let z = f (fun x -> x/z)
            in let s = f (fun x -> "ciao")
            in let a = f
            in (g, w, z, s, a);;
        """
        Assert.AreEqual("it", variable_name)
        Assert.AreEqual("(int, int -> '17 -> int -> '17, int -> int, int -> string, int -> '18 -> int -> '18)", ty)
        Assert.AreEqual("<|[z=2];f;x;fun y -> x y + 1|>", v)

    [<TestMethod>]
    member _.RecursiveFunctionCountStep () =
        let _ = Evaluate.evaluate $"""let rec f x y = if x > 0 then f (x - 1) (y + 1) else y;;"""
        let variable_name, ty, v = Evaluate.evaluate $"f 4 0;;"
        Assert.AreEqual("it", variable_name)
        Assert.AreEqual("int", ty)
        Assert.AreEqual("4", v)
