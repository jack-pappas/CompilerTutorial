(*  Arithmetic expressions with variables and variable bindings.
    Exercises:
      * Start by implementing an evaluation function; use the 'choice' workflow from the start to make sure errors are cleanly handled.
        For now, don't handle the Var case -- your eval function should just return an error if it sees one of these cases.
      * Now that your eval function works for the arithmetic operations, what modifications need to be made for 'eval' to support variables?
      * For example, try creating a new, non-recursive 'eval' function which takes an additional parameter, (env : Map<_,_>);
        put your existing eval function inside it, and apply the term argument from the outer 'eval' function to the inner eval function.
        Your inner eval func now has "access" to the 'env' argument of the outer eval func. Use it to implement evaluation of Var cases.
      * If your code broke on the second test -- how could the code be modified so the 'eval' function doesn't break?
      * Now, let's back up to before we added the 'outer' eval function (so we have our original eval function).
        Can we modify this function so it works the same as it did with the wrapper? If so, how?
      * Uncomment the Let case in Term. Make the necessary modifications to the eval func to implement support for the Let case.
      * Does your new code correctly handle scoping? Or do variable bindings escape their scopes?
        If they do, try modifying your code to handle this correctly.
      * Everything we've written so far works well, but doesn't "scale" in terms of adding new operations.
        Easy enough to add a few more, but what about 100 more?
        How can we modify the definition of Term and the 'eval' function so we can add new operations on-the-fly?
*)

module Arithmetic

open ExtCore.Control

type Term<'VarId> =
    | Num of bigint

    | Negate of Term<'VarId>
    | Add of Term<'VarId> * Term<'VarId>
    | Sub of Term<'VarId> * Term<'VarId>
    | Mul of Term<'VarId> * Term<'VarId>
    | Div of Term<'VarId> * Term<'VarId>

    /// A variable.
    | Var of 'VarId

    /// If the condition evaluates to non-zero, take the 'then' branch;
    /// otherwise, take the 'else' branch.
    /// if ... then ... else ...
    | IfThenElse of Term<'VarId> * Term<'VarId> * Term<'VarId>

    /// Let-binding of a variable.
    /// let ... = ... in ...
    | Let of 'VarId * Term<'VarId> * Term<'VarId>


/// Matches the zero value.
let (|Zero|_|) (value : bigint) =
    if value.IsZero then Some () else None

/// Evaluate/simplify a term.
let rec eval (term : Term<'VarId>) : Choice<Term<'VarId>, _> =
    choice {
    match term with
    | Num _ ->
        return term

    //
    // TODO
    //

    | _ -> 
        let msg = sprintf "Can't evaluate the term: %A" term
        return! Choice.error msg
    }


open NUnit.Framework
//open FsCheck

[<TestFixture>]
module Tests =
    [<Test>]
    let ``2+2 = 4`` () : unit =
        let result = eval (Add (Num 2I, Num 2I))
        Assert.AreEqual (Num 4I, result)

    //
    // TODO : Implement additional tests.
    //

