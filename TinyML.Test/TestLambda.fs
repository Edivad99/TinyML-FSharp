namespace TinyML.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestLambda () =

    [<TestInitialize>]
    member _.TestInitialize () =
        Evaluate.reset_environment()


    [<TestMethod>]
    member _.TestBaseLambda () =
        let variable_name, ty, v = Evaluate.evaluate $"let f x = x + 1;;"
        Assert.AreEqual("f", variable_name)
        Assert.AreEqual("int -> int", ty)
        Assert.AreEqual("<|[];x;x + 1|>", v)

    [<TestMethod>]
    member _.TestLambdaId () =
        let variable_name, ty, v = Evaluate.evaluate $"let id x = x;;"
        Assert.AreEqual("id", variable_name)
        Assert.AreEqual("'a -> 'a", ty)
        Assert.AreEqual("<|[];x;x|>", v)

    [<TestMethod>]
    member _.TestLambdaIf () =
        let variable_name, ty, v = Evaluate.evaluate $"let f x = if x then 1 else -1;;"
        Assert.AreEqual("f", variable_name)
        Assert.AreEqual("bool -> int", ty)
        Assert.AreEqual("<|[];x;if x then 1 else -1|>", v)

    [<TestMethod>]
    member _.TestLambdaIf2Args () =
        let variable_name, ty, v = Evaluate.evaluate $"let f x y = if true then x else y;;"
        Assert.AreEqual("f", variable_name)
        Assert.AreEqual("'a -> 'a -> 'a", ty)
        Assert.AreEqual("<|[];x;fun y -> if true then x else y|>", v)

    [<TestMethod>]
    member _.TestLambdaIf2ArgsWithTuple () =
        let variable_name, ty, v = Evaluate.evaluate $"let f x y = ((if true then x else y), x + 1);;"
        Assert.AreEqual("f", variable_name)
        Assert.AreEqual("int -> int -> (int, int)", ty)
        Assert.AreEqual("<|[];x;fun y -> (if true then x else y, x + 1)|>", v)

    [<TestMethod>]
    member _.TestLambdaHighOrderFullPolymorphic () =
        let variable_name, ty, v = Evaluate.evaluate $"""
            let f x y = x y
            in (f (fun x -> x), f (fun x -> x) 1, f (fun x -> x) "ciao");;
        """
        Assert.AreEqual("it", variable_name)
        Assert.AreEqual("('a -> 'a, int, string)", ty)
        Assert.AreEqual("<|[x=<|[f=<|[];x;fun y -> x y|>];x;x|>];y;x y|>, 1, \"ciao\"", v)

    [<TestMethod>]
    member _.TestLambdaHighOrderWhichReceivesPolymorphicFunction () =
        let variable_name, ty, v = Evaluate.evaluate $"""
            let f = fun x -> 
                fun y ->
                    if x > 0
                    then y x
                    else y (x + 1)
            in f 5 (fun x -> x);;
        """
        Assert.AreEqual("it", variable_name)
        Assert.AreEqual("int", ty)
        Assert.AreEqual("5", v)

    [<TestMethod>]
    member _.TestLambdaReturningClusureArgsShadowed () =
        let variable_name, ty, v = Evaluate.evaluate $"""
            let g = fun x -> ( x + 1, let x = "ciao" in x , fun x -> x )
            in g 5;;
        """
        Assert.AreEqual("it", variable_name)
        Assert.AreEqual("(int, string, 'a -> 'a)", ty)
        Assert.AreEqual("6, \"ciao\", <|[x=5];x;x|>", v)

    [<TestMethod>]
    member _.PolymorphicHighOrderFunctionMoreTimesTuple () =
        let variable_name, ty, v = Evaluate.evaluate $"""
            let f = fun x ->
                fun y ->
                    if x > 0
                    then y x
                    else y (x + 1)
            in (f, f (-3) (fun x -> x), f (-5) (fun x -> x + 1), f);;
        """
        Assert.AreEqual("it", variable_name)
        Assert.AreEqual("(int -> int -> 'a -> 'a, int, int, int -> int -> 'b -> 'b)", ty)
        Assert.AreEqual("<|[];x;fun y -> if x > 0 then y x else y x + 1|>, -2, -3, <|[];x;fun y -> if x > 0 then y x else y x + 1|>", v)
