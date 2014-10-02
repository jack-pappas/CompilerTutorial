(*  Untyped arithmetic expressions + source info.
    Exercises:
      * A few simple exercises to check how source information is preserved during evaluation.
        You'll need to modify the 'eval' function to preserve source information as best as you can
        so the users of your language will be able to match up error messages to the code causing the errors.
      * You'll need to decide how to combine source info when simplifying terms.
      * Challenge: Modify 'eval' to use the 'choice' workflow from ExtCore so we can handle errors without raising an exception
      * Advanced: Modify 'eval' so if one or more errors occur during evaluation, _all_ errors are returned.
*)

module Arithmetic

// Starting position in the input string.
type SourceInfo = int

type Term =
    | True of SourceInfo
    | False of SourceInfo
    | Zero of SourceInfo
    | Prev of SourceInfo * Term
    | Next of SourceInfo * Term
    | IsZero of SourceInfo * Term
    | IfThenElse of SourceInfo * Term * Term * Term

/// Determines if a term is a numeric value.
let rec isNumeric (term : Term) : bool =
    match term with
    | Zero _ ->
        true
    | Prev (_, tm)
    | Next (_, tm) ->
        isNumeric tm
    | _ ->
        false

/// Evaluate/simplify a term.
let rec eval (term : Term) =
    match term with
    | IfThenElse (info, True _, t2, t3) ->
        t2

    | IfThenElse (info, False _, t2, t3) ->
        t3

    | IfThenElse (info, t1, t2, t3) ->
        let t1' = eval t1
        IfThenElse (info, t1', t2, t3)

    | Next (info, tm) ->
        Next (info, eval tm)

    | Prev (info1, Zero info2) ->
        Zero info2

    | Prev (_, Next (_, nv1)) when isNumeric nv1 ->
        nv1

    | Prev (info, tm) ->
        Prev (info, eval tm)

    | IsZero (info1, Zero info2) ->
        True info1

    | IsZero (info1, Next (info2, nv1)) when isNumeric nv1 ->
        False info1

    | IsZero (info, tm) ->
        IsZero (info, eval tm)

    | _ -> 
        failwithf "Can't evaluate the term: %A" term


open NUnit.Framework
//open FsCheck

[<TestFixture>]
module Tests =
    [<Test>]
    let ``earliest source info preserved`` () : unit =
        let result = eval <| IsZero (17, Next (28, Zero 35))
        Assert.AreEqual (False 17, result)

    //
    // TODO : Implement additional tests.
    //
