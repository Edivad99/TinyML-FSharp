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

