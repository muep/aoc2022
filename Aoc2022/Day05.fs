module Aoc2022.Day05

type LineType =
    | Crates
    | Numbers
    | Empty
    | Move

let tget pos (t: string) = if t.Length > pos then t[pos] else ' '

let partition (lines: seq<string>) =
    let mutable currentGroup = Crates

    Seq.groupBy
        (fun (txt: string) ->
            let (nextGroup, detectedGroup) =
                match (currentGroup, tget 0 txt, tget 1 txt) with
                | Crates, ' ', '1' -> Empty, Numbers
                | Crates, ' ', _ -> Crates, Crates
                | Crates, '[', _ -> Crates, Crates
                | Empty, ' ', ' ' -> Move, Empty
                | Move, ' ', ' ' -> Move, Move
                | Move, _, _ -> Move, Move
                | _ -> invalidArg $"{currentGroup}, {txt})" "did not match"

            currentGroup <- nextGroup
            detectedGroup)
        lines
    |> Map.ofSeq
    |> (fun a -> (a[Crates], a[Numbers] |> Seq.head, a[Move]))

let initStacks (nums: string) crates =
    let stackPositions =
        nums.Split(" ")
        |> Seq.filter (fun a -> a.Length > 0)
        |> Seq.map (fun s -> (System.Int32.Parse s) - 1)
        |> List.ofSeq

    crates
    |> Seq.map (fun (crate: string) ->
        stackPositions
        |> List.map (fun stack -> crate[4 * stack + 1]))
    |> Seq.rev
    |> Seq.fold
        (fun (stacks: char list array) (crateLayer: char list) ->
            Seq.zip stacks crateLayer
            |> Seq.map (fun (oldStack, crate) ->
                if crate = ' ' then
                    oldStack
                else
                    crate :: oldStack)
            |> Array.ofSeq)
        (Array.create stackPositions.Length [])

let readIndex s = (System.Int32.Parse s) - 1

let readMove (s: string) =
    match s.Split " " with
    | [| "move"; cnt; "from"; src; "to"; dst |] -> (System.Int32.Parse cnt, readIndex src, readIndex dst)
    | _ -> invalidArg ("\"" + s + "\"") "bad move"

let part (partMoves: char list array * seq<int * int * int> -> char list array) path =
    System.IO.File.ReadAllLines path
    |> partition
    |> (fun (crates, nums, moves) -> (initStacks nums crates), (Seq.map readMove moves))
    |> partMoves
    |> Seq.map List.head
    |> (Array.ofSeq >> System.String.Concat)
    |> System.Console.WriteLine

let part1Moves (stacks, moves) =
    moves
    |> (Seq.map (fun (cnt, src, dst) -> seq { for i in 1..cnt -> (src, dst) })
        >> Seq.concat)
    |> Seq.fold
        (fun (stacks: char list array) (src, dst) ->
            let item = List.head stacks[src]
            stacks[dst] <- item :: stacks[dst]
            stacks[src] <- List.tail stacks[src]
            stacks)
        stacks

let part1 = part part1Moves
