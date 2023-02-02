namespace TinyML.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestBinOp () =

    [<TestInitialize>]
    member _.TestInitialize () =
        Evaluate.tenv <- []
        Evaluate.venv <- []

    [<TestMethod>]
    [<DataRow("5 + 3", "8")>]
    [<DataRow("5 - 3", "2")>]
    [<DataRow("5 * 3", "15")>]
    [<DataRow("5 / 3", "1")>]
    [<DataRow("5 % 3", "2")>]
    member _.TestGenericBinOp (expr: string, result) =
        let variable_name, ty, v = Evaluate.evaluate $"{expr};;"
        Assert.AreEqual("it", variable_name)
        Assert.AreEqual("int", ty)
        Assert.AreEqual(result, v)

    [<TestMethod>]
    member _.TestBinOpWithVariable () =
        let _ = Evaluate.evaluate $"let a = 2;;"
        let variable_name, ty, v = Evaluate.evaluate $"let b = a * 10;;"
        Assert.AreEqual("b", variable_name)
        Assert.AreEqual("int", ty)
        Assert.AreEqual("20", v)