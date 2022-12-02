module Aoc2022.Day02

type Play =
    | Rock
    | Paper
    | Scissors

let selectionScore play =
    match play with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let readPlay s =
    match s with
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | _ -> invalidArg s "Expected one of A,B,C,X,Y,Z"

let readRound (s:string) =
    match s.Split " " |> Seq.take 2 |> Seq.map readPlay |> Seq.toArray with
    | [| opponent;me |] -> opponent,me
    | _ -> invalidArg s "Expected a line with two plays"

let resultScore (opponent:Play,me:Play) =
    match (opponent,me) with
    | Rock, Rock -> 3
    | Rock, Paper -> 6
    | Rock, Scissors -> 0
    | Paper, Rock -> 0
    | Paper, Paper -> 3
    | Paper, Scissors -> 6
    | Scissors, Rock -> 6
    | Scissors, Paper -> 0
    | Scissors, Scissors -> 3

let roundScore round =
    (resultScore round) + (selectionScore (snd round)) 

let part1 path =
    System.IO.File.ReadLines path
    |> Seq.map (readRound >> roundScore)
    |> Seq.sum
    |> printfn "%d"