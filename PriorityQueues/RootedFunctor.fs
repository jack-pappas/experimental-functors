module PriorityQueues.Rooted

[<NoEquality; NoComparison>]
[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type RootedQueue<'T, 'Elem when 'Elem : comparison> =
    | Empty
    | Root of 'Elem * 'T

let empty<'T, 'Elem when 'Elem : comparison> : RootedQueue<'T, 'Elem> = Empty

let isEmpty (queue : RootedQueue<'T, 'Elem>) =
    match queue with
    | Empty -> true
    | Root (_,_) -> false

let insert<'T, 'Elem, 'PriorityQueueModule
    when 'Elem : comparison
    and 'PriorityQueueModule :> IPriorityQueueModule<'T, 'Elem>
    and 'PriorityQueueModule : (new : unit -> 'PriorityQueueModule)> (y, queue) =
    match queue with
    | Empty ->
        Root (y, PriorityQueueFunctor<'T, 'Elem, 'PriorityQueueModule>.Empty)
    | Root (x, q) ->
        if y <= x then
            Root (y, PriorityQueueFunctor<'T, 'Elem, 'PriorityQueueModule>.Insert (x, q))
        else
            Root (x, PriorityQueueFunctor<'T, 'Elem, 'PriorityQueueModule>.Insert (y, q))

let meld<'T, 'Elem, 'PriorityQueueModule
    when 'Elem : comparison
    and 'PriorityQueueModule :> IPriorityQueueModule<'T, 'Elem>
    and 'PriorityQueueModule : (new : unit -> 'PriorityQueueModule)> = function
    | Empty, rq
    | rq, Empty -> rq
    | Root (x1, q1), Root (x2, q2) ->
        if x1 <= x2 then
            Root (x1, PriorityQueueFunctor<'T, 'Elem, 'PriorityQueueModule>.Insert (x2,
                    PriorityQueueFunctor<'T, 'Elem, 'PriorityQueueModule>.Meld (q1, q2)))
        else
            Root (x2, PriorityQueueFunctor<'T, 'Elem, 'PriorityQueueModule>.Insert (x1,
                    PriorityQueueFunctor<'T, 'Elem, 'PriorityQueueModule>.Meld (q1, q2)))

exception EMPTY

let findMin (queue : RootedQueue<'T, 'Elem>) =
    match queue with
    | Empty -> raise EMPTY
    | Root (x, q) -> x

let deleteMin<'T, 'Elem, 'PriorityQueueModule
    when 'Elem : comparison
    and 'PriorityQueueModule :> IPriorityQueueModule<'T, 'Elem>
    and 'PriorityQueueModule : (new : unit -> 'PriorityQueueModule)> (queue : RootedQueue<'T, 'Elem>) =
    match queue with
    | Empty -> raise EMPTY
    | Root (x, q) ->
        if PriorityQueueFunctor<'T, 'Elem, 'PriorityQueueModule>.IsEmpty q then Empty
        else
            Root (PriorityQueueFunctor<'T, 'Elem, 'PriorityQueueModule>.FindMin q,
                    PriorityQueueFunctor<'T, 'Elem, 'PriorityQueueModule>.DeleteMin q)

//
[<Sealed>]
type RootedQueueModule<'T, 'Elem, 'PriorityQueueModule
    when 'Elem : comparison
    and 'PriorityQueueModule :> IPriorityQueueModule<'T, 'Elem>
    and 'PriorityQueueModule : (new : unit -> 'PriorityQueueModule)> () =
    interface IPriorityQueueModule<RootedQueue<'T, 'Elem>, 'Elem> with
        member __.Empty
            with get () = empty<'T, 'Elem>

        member __.IsEmpty (queue : RootedQueue<'T, 'Elem>) =
            isEmpty queue

        member __.Insert (value : 'Elem, ts : RootedQueue<'T, 'Elem>) : RootedQueue<'T, 'Elem> =
            insert<'T, 'Elem, 'PriorityQueueModule> (value, ts)

        member __.Meld (queue1 : RootedQueue<'T, 'Elem>, queue2 : RootedQueue<'T, 'Elem>) : RootedQueue<'T, 'Elem> =
            meld<'T, 'Elem, 'PriorityQueueModule> (queue1, queue2)

        member __.FindMin (ts : RootedQueue<'T, 'Elem>) : 'Elem =
            findMin ts

        member __.DeleteMin (ts : RootedQueue<'T, 'Elem>) : RootedQueue<'T, 'Elem> =
            deleteMin<'T, 'Elem, 'PriorityQueueModule> ts
