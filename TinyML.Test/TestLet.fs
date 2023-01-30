namespace TinyML.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestLet () =

    [<TestMethod>]
    [<DataRow("int", "2")>]
    [<DataRow("float", "0.5")>]
    [<DataRow("string", "\"ciao\"")>]
    [<DataRow("char", "'c'")>]
    [<DataRow("bool", "true")>]
    [<DataRow("unit", "()")>]
    member _.TestGenericLet (t, r) =
        let variable_name, ty, v = Evaluate.evaluate $"let x = {r};;"
        Assert.AreEqual("x", variable_name)
        Assert.AreEqual(t, ty)
        Assert.AreEqual(v, r)
