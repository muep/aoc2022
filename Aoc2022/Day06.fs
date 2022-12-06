module Aoc2022.Day06

let noDupes (chs:char[]) = chs.Length = (chs |> Set.ofSeq |> Set.count) 

let part1 path =
    System.IO.File.ReadLines path
    |> Seq.head
    |> Seq.windowed 4
    |> Seq.zip (Seq.initInfinite (fun a -> a + 4))
    |> Seq.filter (snd >> noDupes) 
    |> Seq.head
    |> fst
    |> printfn "%A"