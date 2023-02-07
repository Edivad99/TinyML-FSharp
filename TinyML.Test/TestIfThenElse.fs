namespace TinyML.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestIfThenElse () =

    [<TestInitialize>]
    member _.TestInitialize () =
        Evaluate.reset_environment()


    [<TestMethod>]
    member _.TestIfThenElse () =
        let variable_name, ty, v = Evaluate.evaluate $"if 5 > 3 then 5 else 3;;"
        Assert.AreEqual("it", variable_name)
        Assert.AreEqual("int", ty)
        Assert.AreEqual("5", v)
