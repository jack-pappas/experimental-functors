//
module PriorityQueues.Binomial


//
[<Measure>] type Rank

//
type Tree<'T when 'T : comparison> =
    Node of 'T * int<Rank> * Tree<'T> list

//
type Forest<'T when 'T : comparison> = Tree<'T> list

(* auxiliary functions *)

let private root (Node(x,r,c)) = x
let private rank (Node(x,r,c)) = r

let private link ((Node(x1,r1,c1) as t1), (Node(x2,r2,c2) as t2)) = (* t1 = t2 *)
    if x1 <= x2 then Node (x1, r1 + 1<_>, t2 :: c1) else Node (x2, r2 + 1<_>, t1 :: c2)

let rec private ins = function
    | t, [] -> [t]
    | t, t' :: ts ->
        (* rank t <= rank t' *)
        if rank t < rank t' then t :: t' :: ts else ins (link (t, t'), ts)
    
let empty = []

let isEmpty (ts : Forest<'T>) =
    List.isEmpty ts

let insert (x, ts : Forest<'T>) =
    ins (Node (x,0<_>,[]), ts)

let rec meld = function
    | [], ts
    | ts, [] -> ts
    | t1 :: ts1 , t2 :: ts2 ->
        if rank t1 < rank t2 then
            t1 :: meld (ts1, t2 :: ts2)
        elif rank t2 < rank t1 then
            t2 :: meld (t1 :: ts1, ts2)
        else
            ins (link (t1, t2), meld (ts1, ts2))

exception EMPTY

let rec findMin (ts : Forest<'T>) =
    match ts with
    | [] -> raise EMPTY
    | [t] -> root t
    | t :: ts ->
        let x = findMin ts
        if root t <= x then root t else x

let deleteMin (ts : Forest<'T>) : Forest<'T> =
    let rec getMin = function
        | [] -> raise EMPTY
        | [t] -> (t, [])
        | t :: ts ->
            let t', ts' = getMin ts
            if root t <= root t' then (t, ts) else (t', t :: ts')

    let (Node (x,r,c)), ts = getMin ts
    meld (List.rev c, ts)

let extractMin (ts : Forest<'T>) : 'T * Forest<'T> =
    findMin ts, deleteMin ts

let toList ts =
    let rec toList acc ts =
        match ts with
        | [] ->
            List.rev acc
        | _ ->
            let x, ts' = extractMin ts
            toList (x :: acc) ts'

    toList [] ts


//
[<Sealed>]
type BinomialQueueModule<'T when 'T : comparison> () =
    interface IPriorityQueueModule<Forest<'T>, 'T> with
        member __.Empty
            with get () = empty

        member __.IsEmpty (ts : Forest<'T>) =
            List.isEmpty ts

        member __.Insert (value : 'T, ts : Forest<'T>) : Forest<'T> =
            insert (value, ts)

        member __.Meld (queue1 : Forest<'T>, queue2 : Forest<'T>) : Forest<'T> =
            meld (queue1, queue2)

        member __.FindMin (ts : Forest<'T>) : 'T =
            findMin ts

        member __.DeleteMin (ts : Forest<'T>) : Forest<'T> =
            deleteMin ts

