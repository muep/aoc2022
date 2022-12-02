module Aoc2022.Day02

type Play =
    | Rock
    | Paper
    | Scissors

type Outcome =
    | Lose
    | Draw
    | Win

let outcomes =
    [ Rock, Rock, Draw
      Rock, Paper, Win
      Rock, Scissors, Lose
      Paper, Rock, Lose
      Paper, Paper, Draw
      Paper, Scissors, Win
      Scissors, Rock, Win
      Scissors, Paper, Lose
      Scissors, Scissors, Draw ]

let selectionScore play =
    match play with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let outcomeScore outcome =
    match outcome with
    | Lose -> 0
    | Draw -> 3
    | Win -> 6

let readPlay s =
    match s with
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | _ -> invalidArg s "Expected one of A,B,C,X,Y,Z"

let readOutcome s =
    match s with
    | "X" -> Lose
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> invalidArg s "Expected one of X,Y,Z"

let pairFromLine (s:string) =
    match s.Split " "
          |> Seq.take 2
          |> Seq.toArray
        with
    | [| a; b |] -> a, b
    | _ -> invalidArg s "Expected two things"

let readInstruction1 = pairFromLine >> (fun (o, m)
                                          -> readPlay o, readPlay m)

let readInstruction2 = pairFromLine >> (fun (opp, out)
                                          -> readPlay opp, readOutcome out)

let selectPlay1 (opponentPlay, myPlay) =
    outcomes
    |> Seq.filter (fun (op, my, _) -> op = opponentPlay && my = myPlay)
    |> Seq.head

let selectPlay2 (opponentPlay, desiredOutcome) =
    outcomes
    |> Seq.filter (fun (op, _, outcome)
                    -> op = opponentPlay && outcome = desiredOutcome)
    |> Seq.head

let roundScore (_, myPlay, outcome) =
    (outcomeScore outcome) + (selectionScore myPlay)

let part1 path =
    System.IO.File.ReadLines path
    |> Seq.map readInstruction1
    |> Seq.map selectPlay1
    |> Seq.map roundScore
    |> Seq.sum
    |> printfn "%d"

let part2 path =
    System.IO.File.ReadLines path
    |> Seq.map readInstruction2
    |> Seq.map selectPlay2
    |> Seq.map roundScore
    |> Seq.sum
    |> printfn "%d"
