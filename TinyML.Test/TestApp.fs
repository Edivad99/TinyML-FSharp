namespace TinyML.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open TinyML

[<TestClass>]
type TestApp () =

    [<TestInitialize>]
    member _.TestInitialize () =
        Evaluate.tenv <- []
        Evaluate.venv <- []
        Typing.reset_var_counter()

    [<TestMethod>]
    [<DataRow("int", "2")>]
    [<DataRow("float", "0.5")>]
    member _.TestIdApp (t, value) =
        let _ = Evaluate.evaluate "let id x = x;;"
        let variable_name, ty, v = Evaluate.evaluate $"id {value};;"
        Assert.AreEqual("it", variable_name)
        Assert.AreEqual(t, ty)
        Assert.AreEqual(value, v)

