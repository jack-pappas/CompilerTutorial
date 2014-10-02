(*  Untyped arithmetic expressions.
    Exercises:
      * Fix the broken tests (which will likely involve modifying the eval function).
*)

module Arithmetic

type Term =
    | True
    | False
    | Zero
    | Prev of Term
    | Next of Term
    | IsZero of Term
    | IfThenElse of Term * Term * Term

/// Determines if a term is a numeric value.
let rec isNumeric (term : Term) : bool =
    match term with
    | Zero ->
        true
    | Prev tm
    | Next tm ->
        isNumeric tm
    | _ ->
        false

/// Evaluate/simplify a term.
let rec eval (term : Term) =
    match term with
    | IfThenElse (True _, t2, t3) ->
        t2

    | IfThenElse (False _, t2, t3) ->
        t3

    | IfThenElse (t1, t2, t3) ->
        let t1' = eval t1
        IfThenElse (t1', t2, t3)

    | Next tm ->
        Next (eval tm)

    | Prev Zero ->
        Zero

    | Prev (Next nv1) when isNumeric nv1 ->
        nv1

    | Prev tm ->
        Prev (eval tm)

    | IsZero Zero ->
        True

    | IsZero (Next nv1) when isNumeric nv1 ->
        False

    | IsZero tm ->
        IsZero (eval tm)

    | _ -> 
        failwithf "Can't evaluate the term: %A" term


open NUnit.Framework

[<TestFixture>]
module Tests =
    [<Test>]
    let ``Zero is numeric`` () : unit =
        Assert.IsTrue (isNumeric Zero)

    [<Test>]
    let ``True is not numeric`` () : unit =
        Assert.IsFalse (isNumeric True)

    // Why doesn't this test pass like we expect?
    // Can you fix it by modifying 'eval'?
    [<Test>]
    let ``Next/Prev cancel each other out`` () : unit =
        let count = 10
        let mutable term = Zero
        for i = 1 to count do
            term <- Next term
        for i = 1 to count do
            term <- Prev term

        // Evaluating the term should return Zero.
        let result = eval term
        Assert.AreEqual (Zero, result)

    // Why doesn't this test pass like we expect?
    // Can you fix it by modifying 'eval'?
    [<Test>]
    let ``IfThenElse with nontrivial condition`` () : unit =
        let term =
            IfThenElse (IsZero (Next (Next Zero)), Zero, Next Zero)

        // Evaluate the term and check the result.
        let result = eval term
        Assert.AreEqual (Next Zero, result)
