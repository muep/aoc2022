module Aoc2022.Day03

let priorities =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

let priority (a: char) = 1 + priorities.IndexOf a

let splitMiddle (s:string) =
    let ln = s.Length
    [| s.[.. ln / 2 - 1]; s.[ln / 2 ..]|]

let commonItem (sacks: string[]) =
    sacks
    |> Seq.map Set.ofSeq
    |> Seq.reduce Set.intersect
    |> Seq.head

let part1 path =
    System.IO.File.ReadLines path
    |> Seq.map splitMiddle
    |> Seq.map commonItem
    |> Seq.map priority
    |> Seq.sum
    |> printfn "%d"

let part2 path =
    System.IO.File.ReadLines path
    |> Seq.chunkBySize 3
    |> Seq.map commonItem
    |> Seq.map priority
    |> Seq.sum
    |> printfn "%d"
