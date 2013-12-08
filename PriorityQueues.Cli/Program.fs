// Okasaki's "Optimal Purely Functional Priority Queues" in F#.

open PriorityQueues


let randList count =
    let rand = System.Random ()
    List.init count <| fun _ -> rand.Next ()

let randList64 count =
    let rand = System.Random ()
    List.init count <| fun _ ->
        let top = rand.Next ()
        let bottom = rand.Next ()
        ((int64 top) <<< 32) ||| (int64 bottom)


[<EntryPoint>]
let main argv =
    let lst = randList 2000
    let lst64 = randList64 2000

    let foo =
        (SkewBinomial.empty, lst)
        ||> List.fold (fun queue x ->
            SkewBinomial.insert (x, queue))

    let eq =
        let fooList = SkewBinomial.toList foo
        (List.sort lst) = fooList


    (* Examples of using priority queues via "functors" *)
    let baz =
        (SkewBinomialQueue<int>.Empty, lst)
        ||> List.fold (fun queue x ->
            SkewBinomialQueue.Insert (x, queue))

    let barbar =
        (RootedSkewBinomialQueue<int64>.Empty, lst64)
        ||> List.fold (fun queue x ->
            RootedSkewBinomialQueue<int64>.Insert (x, queue))
    


    printfn "%A" argv
    0 // return an integer exit code
