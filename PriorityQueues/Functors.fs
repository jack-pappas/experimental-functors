namespace PriorityQueues

//
[<Sealed; AbstractClass>]
type PriorityQueueFunctor<'T, 'Elem, 'PriorityQueueModule
    when 'Elem : comparison
    and 'PriorityQueueModule :> IPriorityQueueModule<'T, 'Elem>
    and 'PriorityQueueModule : (new : unit -> 'PriorityQueueModule)> () =

    //
    static let moduleInstance = new 'PriorityQueueModule ()

    //
    static member Empty
        with get () = moduleInstance.Empty

    //
    static member IsEmpty (queue) =
        moduleInstance.IsEmpty queue

    //
    static member Insert (value, queue) =
        moduleInstance.Insert (value, queue)

    //
    static member Meld (queue1, queue2) =
        moduleInstance.Meld (queue1, queue2)

    //
    static member FindMin (queue) =
        moduleInstance.FindMin queue

    //
    static member DeleteMin (queue) =
        moduleInstance.DeleteMin queue
    

