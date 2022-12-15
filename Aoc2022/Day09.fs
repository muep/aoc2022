module Aoc2022.Day09

type State =
    { head: (int * int)
      tail: (int * int)
      tailPositions: Set<(int * int)> }

let initialState =
    { head = (0, 0)
      tail = (0, 0)
      tailPositions = Set.empty }

let moveState
    { head = (headx, heady)
      tail = (tailx, taily)
      tailPositions = tailPositions }
    (dx, dy)
    =
    let head = (headx + dx, heady + dy)

    let tail =
        match dx, dy with
        | (_, 0) ->
            if (dx * headx) <= (dx * tailx) then
                tailx, taily
            else
                headx, heady
        | (0, _) ->
            if (dy * heady) <= (dy * taily) then
                tailx, taily
            else
                headx, heady
        | _ -> invalidArg "" "Expected one of the cardinal directions"

    { head = head
      tail = tail
      tailPositions = Set.add tail tailPositions }

let readMoveLine (line: string) =
    let move, cnt =
        match line[0], System.Int32.Parse(line[2..]) with
        | 'L', cnt -> ((-1, 0), cnt)
        | 'R', cnt -> ((1, 0), cnt)
        | 'U', cnt -> ((0, 1), cnt)
        | 'D', cnt -> ((0, -1), cnt)
        | _ -> invalidArg line "Expected direction and count"

    Array.create cnt move

let stepsFrom path =
    System.IO.File.ReadLines path |> Seq.map readMoveLine |> Seq.concat

let part1Result path =
    stepsFrom path
    |> Seq.fold moveState initialState
    |> (fun a -> a.tailPositions.Count)

let part1 path =
    part1Result path
    |> System.Console.WriteLine

let part2 path =
    System.Console.WriteLine "Not implemented"
