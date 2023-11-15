module Aoc2022.Day01

let splitByEmpty seq =
    let groupId = ref 0

    seq
    |> Seq.groupBy (fun txt ->
        if txt = "" then
            groupId.Value <- groupId.Value + 1

        groupId.Value)
    |> Seq.map (snd >> (Seq.filter (fun t -> 0 < t.Length)))

let part1Result path =
    System.IO.File.ReadLines path
    |> splitByEmpty
    |> Seq.map ((Seq.map System.Int32.Parse) >> Seq.sum)
    |> Seq.max

let part2Result path =
    System.IO.File.ReadLines path
    |> splitByEmpty
    |> Seq.map ((Seq.map System.Int32.Parse) >> Seq.sum)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum

let part1 path =
    path |> part1Result |> System.Console.WriteLine

let part2 path =
    path |> part2Result |> System.Console.WriteLine
