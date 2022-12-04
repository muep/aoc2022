module Aoc2022.Day04

let readRange (s: string) =
    let rstart, rend =
        match s.Split("-") with
        | [| a; b |] -> System.Int32.Parse a, System.Int32.Parse b
        | _ -> invalidArg s "Expected pattern X-Y"

    set { rstart..rend }

let readAssignment (s: string) =
    match s.Split(",") with
    | [| a; b |] -> readRange a, readRange b
    | _ -> invalidArg s "Expected two ranges"

let part theAskedForThing path =
    path
    |> System.IO.File.ReadLines
    |> Seq.map readAssignment
    |> Seq.filter theAskedForThing
    |> Seq.length
    |> printfn "%d"

let part1 =
    part (fun (r0, r1) -> (Set.isSubset r0 r1) || (Set.isSubset r1 r0))

let part2 =
    part (fun (r0, r1) -> (Set.intersect r0 r1) |> (Seq.isEmpty >> not))
