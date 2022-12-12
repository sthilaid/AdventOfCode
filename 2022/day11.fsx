open System.IO

type Monkey = {items: list<uint64>; op: uint64 -> uint64; test: uint64 -> bool; positive: int; negative: int}

let readLines (filePath:string) = [
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
]

let removeEmptyLines lines = lines |> List.filter (fun l -> not (l = ""))
let purge tokens = tokens |> Seq.filter (fun token -> not (token = ""))

let parseItems (line: string) =
    line.Split [|' '|]
    |> purge
    |> Seq.map (fun token -> (token
                              |> Seq.filter (fun c -> not (c = ','))
                              |> Seq.toArray
                              |> (fun ar -> System.String ar)))
    |> Seq.skip 2
    |> Seq.map (fun token -> uint64 token)
    |> Seq.toList
    
let parseOp (line: string) =
    let opData = line.Split [|' '|] |> purge |> Seq.toList
    let getArg = fun x -> (if opData.[5] = "old" then x else uint64 opData.[5])
    match opData.[4] with
    | "+" -> fun old -> (old + (getArg old))
    | "*" -> fun old -> (old * (getArg old))
    | x ->
        printfn "MATCH ERROR: %A" x
        fun x -> x

let parseTest (line: string) =
    let data = line.Split [|' '|] |> purge |> Seq.toList
    let div = data.[3] |> uint64
    fun x -> (x % div) = 0UL

let parsePosNeg (line: string) =
    line.Split [|' '|] |> purge |> Seq.toList |> (fun x -> int x.[5])

let rec parseMonkeys (monkeys: list<Monkey>) (lines: list<string>) =
    if lines.Length < 6 then
        monkeys
    else
        let items = parseItems lines.[1]
        let op = parseOp lines.[2]
        let test = parseTest lines.[3]
        let pos = parsePosNeg lines.[4]
        let neg = parsePosNeg lines.[5]
        let monkey = {items=items; op=op; test=test; positive=pos; negative=neg}
        List.skip 6 lines |> parseMonkeys (monkey :: monkeys)

let applyTest m worry =
    if m.test worry then m.positive else m.negative

let appendItem m item = {items=(item :: m.items); op=m.op; test=m.test; positive=m.positive; negative=m.negative}
let purgeItems m = {items=[]; op=m.op; test=m.test; positive=m.positive; negative=m.negative}
let printMonkeysWithCount (counts, monkeys) =
    List.mapi2 (fun i monkey count ->
                printfn "monkey %d - items: %A - count: %d" i monkey.items count)
               monkeys
               counts
        |> ignore
    (counts, monkeys)

let printMonkeys monkeys =
    List.mapi2 (fun i monkey count ->
                printfn "monkey %d - items: %A" i monkey.items)
               monkeys
        |> ignore
    monkeys

let evalMonkey worry_fun monkeys (m_index, m) =
    List.fold (fun monkeys item ->
               // printfn "%d - %d ---------------------------------------" m_index item
               // printMonkeys ([], monkeys) |> ignore
               let newItemWorry = m.op item |> worry_fun 
               let nextMonkey = applyTest m newItemWorry
               monkeys |> List.mapi (fun i monkey ->
                                     if i = nextMonkey then appendItem monkey newItemWorry
                                     else monkey))
            monkeys
            m.items
    |> List.mapi (fun i monkey ->
                  if i = m_index then purgeItems monkey
                  else monkey)

let addCounts counts index ammount =
    counts
    |> List.mapi (fun i x -> if i = index then x + ammount else x)
    
let rec evalMonkeys worry_fun index (counts, (monkeys: list<Monkey>)) =
    if index >= monkeys.Length then
        (counts, monkeys)
    else
        let newMonkeys = evalMonkey worry_fun monkeys (index, monkeys.[index])
        evalMonkeys worry_fun (index+1) (addCounts counts index monkeys.[index].items.Length, newMonkeys)

let rec runSimulation rounds worry_fun (monkeys: list<Monkey>) =
    let rec run round (count, monkeys) =
        // if round = (rounds - 20) then
        //     printfn "%d-------------------------------------------------------" round
        //     (count, monkeys) |> printMonkeysWithCount |> ignore
        match round with
            | 0 -> (count, monkeys)
            | _ -> run (round-1) (evalMonkeys worry_fun 0 (count, monkeys))

    let counts = [for x in 1..monkeys.Length -> 0]
    run rounds (counts, monkeys)

let part1_worry_manamgement x = (x/3UL) |> uint64
//let part2_worry_manamgement x = x % (23UL*19UL*13UL*17UL) |> uint64 // for testinput
let part2_worry_manamgement x = x % (2UL*7UL*11UL*19UL*3UL*5UL*17UL*13UL)

let print_result prelude (counts, monkeys) =
    counts
    |> List.fold (fun (max1, max2) x ->
                  x |> uint64
                  |> (fun x -> if x > max1 then (x, max1) elif x > max2 then (max1, x) else (max1, max2)))
                 (0UL,0UL)
    |> fun (max1,max2) ->
        printfn "%s: %u*%u = %u" prelude max1 max2 (max1 * max2)
        (counts, monkeys)
                             

let monkeys =
    "day11.input"
    |> readLines
    |> removeEmptyLines
    |> parseMonkeys []
    |> List.rev

monkeys
|> runSimulation 20 part1_worry_manamgement
// |> printMonkeysWithCount
|> print_result "part1"

monkeys
|> runSimulation 10000 part2_worry_manamgement
// |> printMonkeysWithCount
|> print_result "part2"
