namespace TinyML.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestUnOp () =

    [<TestInitialize>]
    member _.TestInitialize () =
        Evaluate.reset_environment()

    [<TestMethod>]
    [<DataRow("not true", "false")>]
    [<DataRow("not false", "true")>]
    [<DataRow("not (not true)", "true")>]
    [<DataRow("not (true and false)", "true")>]
    member _.TestIntBinOp (expr: string, result) =
        let variable_name, ty, v = Evaluate.evaluate $"{expr};;"
        Assert.AreEqual("it", variable_name)
        Assert.AreEqual("bool", ty)
        Assert.AreEqual(result, v)
