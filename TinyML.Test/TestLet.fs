namespace TinyML.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestLet () =

    [<TestMethod>]
    member _.TestLetInt () =
        let t, l, r = "int", "x", "2"
        let variable_name, ty, v = Evaluate.evaluate $"let {l} = {r};;"
        Assert.AreEqual(l, variable_name)
        Assert.AreEqual(t, ty)
        Assert.AreEqual(v, r);

    [<TestMethod>]
    member _.TestLetFloat () =
        let t, l, r = "float", "x", "0.5"
        let variable_name, ty, v = Evaluate.evaluate $"let {l} = {r};;"
        Assert.AreEqual(l, variable_name)
        Assert.AreEqual(t, ty)
        Assert.AreEqual(v, r);

    [<TestMethod>]
    member _.TestLetString () =
        let t, l, r = "string", "x", "\"ciao\""
        let variable_name, ty, v = Evaluate.evaluate $"let {l} = {r};;"
        Assert.AreEqual(l, variable_name)
        Assert.AreEqual(t, ty)
        Assert.AreEqual(v, r);

    [<TestMethod>]
    member _.TestLetChar () =
        let t, l, r = "char", "x", "c"
        let variable_name, ty, v = Evaluate.evaluate $"let {l} = '{r}';;"
        Assert.AreEqual(l, variable_name)
        Assert.AreEqual(t, ty)
        Assert.AreEqual(v, r);

    [<TestMethod>]
    member _.TestLetBool () =
        let t, l, r = "bool", "x", "true"
        let variable_name, ty, v = Evaluate.evaluate $"let {l} = {r};;"
        Assert.AreEqual(l, variable_name)
        Assert.AreEqual(t, ty)
        Assert.AreEqual(v, r);

    [<TestMethod>]
    member _.TestLetUnit () =
        let t, l, r = "unit", "x", "()"
        let variable_name, ty, v = Evaluate.evaluate $"let {l} = {r};;"
        Assert.AreEqual(l, variable_name)
        Assert.AreEqual(t, ty)
        Assert.AreEqual(v, r);
