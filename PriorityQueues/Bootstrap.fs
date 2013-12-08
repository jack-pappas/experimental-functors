module PriorityQueues.Bootstrapped


//
[<CustomEquality; CustomComparison>]
type BootstrappedQueueRoot<'T, 'Elem
    when 'Elem : comparison
    and 'T : equality> = Root of 'Elem * 'T
with
    override this.GetHashCode () =
        match this with
        | Root (x, _) ->
            x.GetHashCode()

    override this.Equals (other : obj) =
        match other with
        | :? BootstrappedQueueRoot<'T, 'Elem> as other' ->
            BootstrappedQueueRoot<'T, 'Elem>.Equals (this, other')
        | _ ->
            false

    static member private Equals (x : BootstrappedQueueRoot<'T, 'Elem>, y : BootstrappedQueueRoot<'T, 'Elem>) =
        match x, y with
        | Root (x1, q1), Root (x2, q2) ->
            x1 = x2 && q1 = q2

    static member private Compare (x : BootstrappedQueueRoot<'T, 'Elem>, y : BootstrappedQueueRoot<'T, 'Elem>) =
        match x, y with
        | Root (x1, q1), Root (x2, q2) ->
            compare x1 x2

    interface System.IEquatable<BootstrappedQueueRoot<'T, 'Elem>> with
        member this.Equals other =
            BootstrappedQueueRoot<_,_>.Equals (this, other)

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? BootstrappedQueueRoot<'T, 'Elem> as other' ->
                BootstrappedQueueRoot<'T, 'Elem>.Compare (this, other')
            | _ ->
                invalidArg "other" "The object cannot be compared to this instance because the object is not an instance of the same type."

    interface System.IComparable<BootstrappedQueueRoot<'T, 'Elem>> with
        member this.CompareTo other =
            BootstrappedQueueRoot<_,_>.Compare (this, other)


//[<NoEquality>]
[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type BootstrappedQueue<'T, 'Elem
    when 'Elem : comparison
    and 'T : equality> =
    | Empty
    // RootedQueue
    | NonEmpty of BootstrappedQueueRoot<'T, 'Elem>


let empty<'T, 'Elem
    when 'Elem : comparison and 'T : equality> =
    BootstrappedQueue<'T, 'Elem>.Empty

let isEmpty = function
    | Empty -> true
    | NonEmpty _ -> false

(*  None of these implementations of 'insert' satisfy the F# 3.0 compiler, which gives a variety of reasons
    (i.e., error messages) why it won't accept them. *)
(*

let rec insert (x, xs) =
    meld (NonEmpty (Root (x, PriorityQueueFunctor<'T,_,_>.Empty)), xs)
    
let rec insert (x, xs) =
    meld (NonEmpty (Root (x, PriorityQueueFunctor<'T, _, IPriorityQueueModule<_,_>>.Empty)), xs)
    
let rec insert<'T, 'Elem, 'PriorityQueueModule
    when 'Elem : comparison
    and 'T : equality
    and 'PriorityQueueModule :> IPriorityQueueModule<'T, 'Elem>
    and 'PriorityQueueModule : (new : unit -> 'PriorityQueueModule)> (x, xs) =
    meld (NonEmpty (Root (x, PriorityQueueFunctor<'T, _, 'PriorityQueueModule>.Empty)), xs)
    
let rec insert<'T, 'Elem, 'PriorityQueueModule
    when 'Elem : comparison
    and 'T : equality
    and 'PriorityQueueModule :> IPriorityQueueModule<'T, 'Elem>
    and 'PriorityQueueModule : (new : unit -> 'PriorityQueueModule)> (x, xs) =
    meld (NonEmpty (Root (x, PriorityQueueFunctor<_,_,'PriorityQueueModule>.Empty)), xs)
*)

(*
and meld = function
    | Empty, xs
    | xs, Empty -> xs
    | NonEmpty (Root (x1, q1) as r1), NonEmpty (Root (x2, q2) as r2) ->
        if x1 <= x2 then
            NonEmpty (Root (x1, PriorityQueueFunctor<'T, _, 'PriorityQueueModule>.Insert (r2, q1)))
        else
            NonEmpty (Root (x2, PriorityQueueFunctor<'T, _, 'PriorityQueueModule>.Insert (r1, q2)))

exception EMPTY

let findMin = function
    | Empty -> raise EMPTY
    | NonEmpty (Root (x, q)) -> x

let deleteMin = function
    | Empty -> raise EMPTY
    | NonEmpty (Root (x, q)) ->
        if PriorityQueueFunctor<'T, _, 'PriorityQueueModule>.IsEmpty q then
            BootstrappedQueue<_,_>.Empty
        else
            let (Root (y, q1)) = PriorityQueueFunctor<'T, _, 'PriorityQueueModule>.FindMin q
            let q2 = PriorityQueueFunctor<'T, _, 'PriorityQueueModule>.DeleteMin q
            NonEmpty (Root (y, PriorityQueueFunctor<'T, _, 'PriorityQueueModule>.Meld (q1, q2)))
*)
