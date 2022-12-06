module Aoc2022.Day06

let noDupes (chs:char[]) = chs.Length = (chs |> Set.ofSeq |> Set.count) 

let part windowsize path =
    System.IO.File.ReadLines path
    |> Seq.head
    |> Seq.windowed windowsize
    |> Seq.zip (Seq.initInfinite (fun a -> a + windowsize))
    |> Seq.filter (snd >> noDupes) 
    |> Seq.head
    |> fst
    |> printfn "%A"
    
let part1 = part 4
let part2 = part 14