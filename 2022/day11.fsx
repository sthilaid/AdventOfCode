open System.IO

type Monkey = {items: list<int>; op: int -> int; test: int -> bool; positive: int; negative: int}

let readLines (filePath:string) = [
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
]

let removeEmptyLines lines = lines |> List.filter (fun l -> not (l = ""))

// let printlines (lines:list<string>) =
//     for l in lines do
//         printfn "%s" l

let purge tokens = tokens |> Seq.filter (fun token -> not (token = ""))

let parseItems (line: string) =
    line.Split [|' '|]
    |> purge
    |> Seq.map (fun token -> (token
                              |> Seq.filter (fun c -> not (c = ','))
                              |> Seq.toArray
                              |> (fun ar -> System.String ar)))
    |> Seq.skip 2
    |> Seq.map (fun token -> int token)
    |> Seq.toList
    
let parseOp (line: string) =
    let opData = line.Split [|' '|] |> purge |> Seq.toList
    let getArg = fun x -> if opData.[5] = "old" then x else int opData.[5]
    match opData.[4] with
    | "+" -> fun old -> old + (getArg old)
    | "*" -> fun old -> old * (getArg old)
    | x ->
        printfn "MATCH ERROR: %A" x
        fun x -> x

let parseTest (line: string) =
    let data = line.Split [|' '|] |> purge |> Seq.toList
    let div = int data.[3]
    fun x -> (x % div) = 0

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
let printMonkeys monkeys =
    monkeys |> List.mapi (fun i monkey ->
                         printfn "monkey %d - items: %A" i monkey.items
                         monkey)
    
let evalMonkey monkeys (m_index, m) =
    List.fold (fun monkeys item ->
               printfn "%d - %d ---------------------------------------" m_index item
               printMonkeys monkeys |> ignore
               let newItemWorry = m.op item |> (fun x -> x/3) |> int
               let nextMonkey = applyTest m newItemWorry
               monkeys |> List.mapi (fun i monkey ->
                                     if i = nextMonkey then appendItem monkey newItemWorry
                                     else monkey))
            monkeys
            m.items
    |> List.mapi (fun i monkey ->
                  if i = m_index then purgeItems monkey
                  else monkey)
    
let rec evalMonkeys index (monkeys: list<Monkey>) =
    if index >= monkeys.Length then
        monkeys
    else
        evalMonkey monkeys (index, monkeys.[index])
        |> evalMonkeys (index+1)

"day11.testinput"
|> readLines
|> removeEmptyLines
|> parseMonkeys []
|> List.rev
|> printMonkeys
|> evalMonkeys 0
|> printMonkeys
