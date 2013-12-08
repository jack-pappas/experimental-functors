module PriorityQueues.BootstrappedSkewBinomialQueue

open PriorityQueues

//
[<CustomEquality; CustomComparison>]
type BootstrappedQueueRoot<'Root, 'T
    when 'Root : comparison
    and 'T : comparison> = Root of 'Root * SkewBinomial.Forest<'T>
with
    override this.GetHashCode () =
        match this with
        | Root (x, _) ->
            x.GetHashCode()

    override this.Equals (other : obj) =
        match other with
        | :? BootstrappedQueueRoot<'Root, 'T> as other' ->
            BootstrappedQueueRoot<'Root, 'T>.Equals (this, other')
        | _ ->
            false

    static member private Equals (x : BootstrappedQueueRoot<'Root, 'T>, y : BootstrappedQueueRoot<'Root, 'T>) =
        match x, y with
        | Root (x1, q1), Root (x2, q2) ->
            x1 = x2 && q1 = q2

    static member private Compare (x : BootstrappedQueueRoot<'Root, 'T>, y : BootstrappedQueueRoot<'Root, 'T>) =
        match x, y with
        | Root (x1, q1), Root (x2, q2) ->
            compare x1 x2

    interface System.IEquatable<BootstrappedQueueRoot<'Root, 'T>> with
        member this.Equals other =
            BootstrappedQueueRoot<_,_>.Equals (this, other)

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? BootstrappedQueueRoot<'Root, 'T> as other' ->
                BootstrappedQueueRoot<'Root, 'T>.Compare (this, other')
            | _ ->
                invalidArg "other" "The object cannot be compared to this instance because the object is not an instance of the same type."

    interface System.IComparable<BootstrappedQueueRoot<'Root, 'T>> with
        member this.CompareTo other =
            BootstrappedQueueRoot<_,_>.Compare (this, other)

(*
//[<NoEquality>]
[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type BootstrappedQueue<'Root, 'T
    when 'Root : comparison
    and 'T : comparison> =
    | Empty
    // RootedQueue
    | NonEmpty of BootstrappedQueueRoot<'Root, 'T>


let empty<'Root, 'T
    when 'Root : comparison and 'T : comparison> =
    BootstrappedQueue<'Root, 'T>.Empty

let isEmpty = function
    | Empty -> true
    | NonEmpty _ -> false

let inline private unsafeCoerce value =
    unbox <| box value

let rec insert (x, xs) =
    meld (NonEmpty (Root (x, SkewBinomial.empty)), xs)

and meld (x, y) =
    match x, y with
    | Empty, xs
    | xs, Empty -> xs
    | NonEmpty (Root (x1, q1) as r1), NonEmpty (Root (x2, q2) as r2) ->
        if x1 <= x2 then
            NonEmpty (Root (x1, SkewBinomial.insert (unsafeCoerce r2, q1)))
        else
            NonEmpty (Root (x2, SkewBinomial.insert (unsafeCoerce r1, q2)))

exception EMPTY

let findMin (queue : BootstrappedQueue<'Elem, 'T>) =
    match queue with
    | Empty ->
        raise EMPTY
    | NonEmpty (Root (x, q)) -> x

let deleteMin (queue : BootstrappedQueue<'Elem, 'T>) : BootstrappedQueue<'Elem, 'T> =
    match queue with
    | Empty ->
        raise EMPTY
    | NonEmpty (Root (x, q)) ->
        if SkewBinomial.isEmpty q then
            BootstrappedQueue<_,_>.Empty
        else
            let (Root (y, q1)) = SkewBinomial.findMin (unsafeCoerce q)
            let q2 = SkewBinomial.deleteMin q
            NonEmpty (Root (y, SkewBinomial.meld (q1, unsafeCoerce q2)))

//
[<Sealed>]
type BootstrappedSkewBinomialQueueModule<'Elem, 'T when 'T : comparison and 'Elem : comparison> () =
    interface IPriorityQueueModule<BootstrappedQueue<'Elem, 'T>, 'Elem> with
        member __.Empty
            with get () = empty<'Elem, 'T>

        member __.IsEmpty (queue : BootstrappedQueue<'Elem, 'T>) =
            isEmpty queue

        member __.Insert (value : 'Elem, ts : BootstrappedQueue<'Elem, 'T>) : BootstrappedQueue<'Elem, 'T> =
            insert (value, ts)

        member __.Meld (queue1 : BootstrappedQueue<'Elem, 'T>, queue2 : BootstrappedQueue<'Elem, 'T>) : BootstrappedQueue<'Elem, 'T> =
            meld (queue1, queue2)

        member __.FindMin (ts : BootstrappedQueue<'Elem, 'T>) : 'Elem =
            findMin ts

        member __.DeleteMin (ts : BootstrappedQueue<'Elem, 'T>) : BootstrappedQueue<'Elem, 'T> =
            deleteMin ts

*)