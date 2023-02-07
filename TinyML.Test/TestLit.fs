namespace TinyML.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open TinyML

[<TestClass>]
type TestLit () =

    [<TestInitialize>]
    member _.TestInitialize () =
        Evaluate.reset_environment()


    [<TestMethod>]
    [<DataRow("int", "2")>]
    [<DataRow("float", "0.5")>]
    [<DataRow("string", "\"ciao\"")>]
    [<DataRow("char", "'c'")>]
    [<DataRow("bool", "true")>]
    [<DataRow("unit", "()")>]
    member _.TestGenericLit (t, r) =
        let variable_name, ty, v = Evaluate.evaluate $"{r};;"
        Assert.AreEqual("it", variable_name)
        Assert.AreEqual(t, ty)
        Assert.AreEqual(r, v)
