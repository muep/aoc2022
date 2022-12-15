module Aoc2022.Day08

type FMap =
    { width: int
      length: int
      heights: byte array }

let load path =
    System.IO.File.ReadLines path
    |> Seq.fold (fun (maxLen, buf) line -> System.Int32.Max(line.Length, maxLen), buf + line) (0, "")
    |> (fun (width, buf) ->
        { width = width
          length = buf.Length / width
          heights =
            buf
            |> Seq.map (string >> System.Byte.Parse)
            |> Seq.toArray })

let candidates fmap =
    Seq.allPairs [ 0 .. fmap.width - 1 ] [
        0 .. fmap.length - 1
    ]

let height fmap (row, col) = fmap.heights[col + fmap.width * row]

let sideways row colStart colEnd =
    Seq.map (fun col -> (row, col)) [ colStart..colEnd ]

let lengthwise rowStart rowEnd col =
    Seq.map (fun row -> (row, col)) [ rowStart..rowEnd ]

let visible fmap (row, col) =
    let lowerThanHere pos =
        (height fmap pos) < (height fmap (row, col))

    let sidewaysLowVisible =
        sideways row 0 (col - 1)
        |> Seq.forall lowerThanHere

    let sidewaysHighVisible =
        sideways row (col + 1) (fmap.width - 1)
        |> Seq.forall lowerThanHere

    let lenLowVisible =
        lengthwise 0 (row - 1) col
        |> Seq.forall lowerThanHere

    let lenHighVisible =
        lengthwise (row + 1) (fmap.length - 1) col
        |> Seq.forall lowerThanHere

    sidewaysLowVisible
    || sidewaysHighVisible
    || lenLowVisible
    || lenHighVisible

let part1 path =
    let fmap = load path

    candidates fmap
    |> Seq.filter (visible fmap)
    |> Seq.length
    |> System.Console.WriteLine

let isBetween minv maxv v = minv <= v && v <= maxv

let scenicScore fmap (row, col) =
    let obstructing pos =
        (height fmap pos) >= (height fmap (row, col))

    let countVisible2 (count, blocked) pos =
        match blocked, obstructing pos with
        | true, _ -> count, blocked
        | false, blockedHere -> (count + 1), blockedHere

    let startState = (0, false)

    let sideLow =
        sideways row 0 (col - 1)
        |> Seq.rev
        |> Seq.fold countVisible2 startState
        |> fst

    let sideHigh =
        sideways row (col + 1) (fmap.width - 1)
        |> Seq.fold countVisible2 startState
        |> fst

    let lenLow =
        lengthwise 0 (row - 1) col
        |> Seq.rev
        |> Seq.fold countVisible2 startState
        |> fst

    let lenHigh =
        lengthwise (row + 1) (fmap.length - 1) col
        |> Seq.fold countVisible2 startState
        |> fst

    sideLow * sideHigh * lenLow * lenHigh

let part2Result path =
    let fmap = load path

    candidates fmap
    |> Seq.map (scenicScore fmap)
    |> Seq.max

let part2 path =
    part2Result path |> System.Console.WriteLine
