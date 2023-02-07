namespace TinyML.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open TinyML

[<TestClass>]
type TestBinOp () =

    [<TestInitialize>]
    member _.TestInitialize () =
        Evaluate.reset_environment()

    [<TestMethod>]
    [<DataRow("5 + 3", "8")>]
    [<DataRow("5 - 3", "2")>]
    [<DataRow("5 * 3", "15")>]
    [<DataRow("5 / 3", "1")>]
    [<DataRow("5 % 3", "2")>]
    member _.TestIntBinOp (expr: string, result) =
        let variable_name, ty, v = Evaluate.evaluate $"{expr};;"
        Assert.AreEqual("it", variable_name)
        Assert.AreEqual("int", ty)
        Assert.AreEqual(result, v)

    [<TestMethod>]
    [<DataRow("5 < 3", "false")>]
    [<DataRow("5 <= 3", "false")>]
    [<DataRow("5 <= 5", "true")>]
    [<DataRow("5 > 3", "true")>]
    [<DataRow("5 >= 3", "true")>]
    [<DataRow("5 >= 5", "true")>]
    [<DataRow("5 = 3", "false")>]
    [<DataRow("5 = 5", "true")>]
    [<DataRow("5 <> 3", "true")>]
    [<DataRow("5 <> 5", "false")>]
    member _.TestBoolBinOp (expr: string, result) =
        let variable_name, ty, v = Evaluate.evaluate $"{expr};;"
        Assert.AreEqual("it", variable_name)
        Assert.AreEqual("bool", ty)
        Assert.AreEqual(result, v)

    [<TestMethod>]
    member _.TestBinOpWithVariable () =
        let _ = Evaluate.evaluate $"let a = 2;;"
        let variable_name, ty, v = Evaluate.evaluate $"let b = a * 10;;"
        Assert.AreEqual("b", variable_name)
        Assert.AreEqual("int", ty)
        Assert.AreEqual("20", v)