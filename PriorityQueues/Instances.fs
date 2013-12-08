namespace PriorityQueues

//
type BinomialQueue<'Elem when 'Elem : comparison> =
    PriorityQueueFunctor<
        PriorityQueues.Binomial.Forest<'Elem>,
        'Elem,
        PriorityQueues.Binomial.BinomialQueueModule<'Elem>>

//
type SkewBinomialQueue<'Elem when 'Elem : comparison> =
    PriorityQueueFunctor<
        PriorityQueues.SkewBinomial.Forest<'Elem>,
        'Elem,
        PriorityQueues.SkewBinomial.SkewBinomialQueueModule<'Elem>>

//
type RootedBinomialQueue<'Elem when 'Elem : comparison> =
    PriorityQueueFunctor<
        PriorityQueues.Rooted.RootedQueue<
            PriorityQueues.Binomial.Forest<'Elem>,
            'Elem>,
        'Elem,
        PriorityQueues.Rooted.RootedQueueModule<
            PriorityQueues.Binomial.Forest<'Elem>,
            'Elem,
            PriorityQueues.Binomial.BinomialQueueModule<'Elem>>>

//
type RootedSkewBinomialQueue<'Elem when 'Elem : comparison> =
    PriorityQueueFunctor<
        PriorityQueues.Rooted.RootedQueue<
            PriorityQueues.SkewBinomial.Forest<'Elem>,
            'Elem>,
        'Elem,
        PriorityQueues.Rooted.RootedQueueModule<
            PriorityQueues.SkewBinomial.Forest<'Elem>,
            'Elem,
            PriorityQueues.SkewBinomial.SkewBinomialQueueModule<'Elem>>>
