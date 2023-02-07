namespace TinyML.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open TinyML

[<TestClass>]
type TestTuple () =

    [<TestInitialize>]
    member _.TestInitialize () =
        Evaluate.reset_environment()


    [<TestMethod>]
    [<DataRow("5", "3", "(int, int)")>]
    [<DataRow("5", "3.1", "(int, float)")>]
    member _.TestTuple (l: string, r: string, t: string) =
        let variable_name, ty, v = Evaluate.evaluate $"({l},{r});;"

        Assert.AreEqual("it", variable_name)
        Assert.AreEqual(t, ty)
        Assert.AreEqual($"{l}, {r}", v)

