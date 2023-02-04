namespace TinyML.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open TinyML

[<TestClass>]
type TestLambda () =

    [<TestInitialize>]
    member _.TestInitialize () =
        Evaluate.tenv <- []
        Evaluate.venv <- []
        Typing.reset_var_counter()

    [<TestMethod>]
    member _.TestBaseLambda () =
        let variable_name, ty, v = Evaluate.evaluate $"let f x = x + 1;;"
        Assert.AreEqual("f", variable_name)
        Assert.AreEqual("int -> int", ty)
        Assert.AreEqual("<|[];x;x + 1|>", v)

    [<TestMethod>]
    member _.TestLambdaId () =
        let variable_name, ty, v = Evaluate.evaluate $"let id x = x;;"
        Assert.AreEqual("id", variable_name)
        Assert.AreEqual("'2 -> '2", ty)
        Assert.AreEqual("<|[];x;x|>", v)

    [<TestMethod>]
    member _.TestLambdaIf () =
        let variable_name, ty, v = Evaluate.evaluate $"let f x = if x then 1 else -1;;"
        Assert.AreEqual("f", variable_name)
        Assert.AreEqual("bool -> int", ty)
        Assert.AreEqual("<|[];x;if x then 1 else -1|>", v)
