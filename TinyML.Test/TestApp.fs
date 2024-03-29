﻿namespace TinyML.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestApp () =

    [<TestInitialize>]
    member _.TestInitialize () =
        Evaluate.reset_environment()


    [<TestMethod>]
    [<DataRow("int", "2")>]
    [<DataRow("float", "0.5")>]
    member _.TestIdApp (t, value) =
        let _ = Evaluate.evaluate "let id x = x;;"
        let variable_name, ty, v = Evaluate.evaluate $"id {value};;"
        Assert.AreEqual("it", variable_name)
        Assert.AreEqual(t, ty)
        Assert.AreEqual(value, v)

