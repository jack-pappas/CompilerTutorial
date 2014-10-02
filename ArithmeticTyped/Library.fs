(*  Typed arithmetic expressions.
    Exercises:
      * Terms in this language have a type: type Type = Bool | Num
      * Modify the 'eval' function so it also performs type-checking and returns
        an error message if it encounters a typing error.
      * Did your "typing" function handle the error cases? Try using the 'choice' workflow again to cleanly handle errors.
      * Assuming your type-checking function handles both the recursion over the tree _and_
        computing the type for each case in the tree:
        Are these two pieces of functionality _necessarily_ intertwined?
        If so, why? If not, show how they could be separated to improve modularity.
*)

module Arithmetic

/// The type of a term.
type Type = Bool | Num

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
let rec eval (term : Term) : Term =
    match term with
    | IfThenElse (True _, tm, _)
    | IfThenElse (False _, _, tm) ->
        tm

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
//open FsCheck

[<TestFixture>]
module Tests =
    [<Test>]
    let ``typeof IsZero`` () : unit =
        Assert.Fail "Test needs to be implemented."
//        let ty, value = eval <| IsZero (Next (Next Zero))
//        Assert.AreEqual (Bool, ty)
//        Assert.AreEqual (False, value)

    //
    // TODO : Implement additional tests.
    //


