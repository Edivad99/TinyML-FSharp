
namespace TinyML.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestLambda () =

    [<TestInitialize>]
    member _.TestInitialize () =
        Evaluate.tenv <- []
        Evaluate.venv <- []

    [<TestMethod>]
    member _.TestBaseLambda () =
        let variable_name, ty, v = Evaluate.evaluate $"let f x = x + 1;;"
        Assert.AreEqual("f", variable_name)
        Assert.AreEqual("int -> int", ty)
        Assert.AreEqual("<|[];x;x + 1|>", v)
