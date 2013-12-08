namespace PriorityQueues

//
[<Interface>]
type IPriorityQueueModule<'T, 'Elem when 'Elem : comparison> =
    //
    abstract Empty : 'T with get
    //
    abstract IsEmpty : 'T -> bool

    //
    abstract Insert : 'Elem * 'T -> 'T
    //
    abstract Meld : 'T * 'T -> 'T

    //
    abstract FindMin : 'T -> 'Elem
    //
    abstract DeleteMin : 'T -> 'T
