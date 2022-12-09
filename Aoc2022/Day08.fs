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
          heights = buf |> Seq.map (string >> System.Byte.Parse) |> Seq.toArray })

let candidates fmap =
    Seq.allPairs [ 0 .. fmap.width - 1 ] [ 0 .. fmap.length - 1 ]

let height fmap (row, col) = fmap.heights[col + fmap.width * row]

let sideways row colStart colEnd =
    Seq.map (fun col -> (row, col)) [ colStart..colEnd ]

let lengthwise rowStart rowEnd col =
    Seq.map (fun row -> (row, col)) [ rowStart..rowEnd ]

let visible fmap (row, col) =
    let lowerThanHere pos =
        (height fmap pos) < (height fmap (row, col))

    let sidewaysLowVisible = sideways row 0 (col - 1) |> Seq.forall lowerThanHere

    let sidewaysHighVisible =
        sideways row (col + 1) (fmap.width - 1) |> Seq.forall lowerThanHere

    let lenLowVisible = lengthwise 0 (row - 1) col |> Seq.forall lowerThanHere

    let lenHighVisible =
        lengthwise (row + 1) (fmap.length - 1) col |> Seq.forall lowerThanHere

    sidewaysLowVisible || sidewaysHighVisible || lenLowVisible || lenHighVisible

let part1 path =
    let fmap = load path

    candidates fmap
    |> Seq.filter (visible fmap)
    |> Seq.length
    |> System.Console.WriteLine

let isBetween minv maxv v = minv <= v && v <= maxv

let scenicScore fmap (row, col) =
    let lowerOrEqualToHere pos =
        (height fmap pos) <= (height fmap (row, col))

    let maxHeight = height fmap (row, col)

    let countVisible (minHeight, cnt) pos =
        let hht = height fmap pos

        if minHeight <=  (min maxHeight hht) then
            (hht, cnt + 1)
        else
            (minHeight, cnt)

    let startState = (0uy, 0)

    let sideLow =
        sideways row 0 (col - 1) |> Seq.rev |> Seq.fold countVisible startState |> snd

    let sideHigh =
        sideways row (col + 1) (fmap.width - 1)
        |> Seq.fold countVisible startState
        |> snd

    let lenLow =
        lengthwise 0 (row - 1) col |> Seq.rev |> Seq.fold countVisible startState |> snd

    let lenHigh =
        lengthwise (row + 1) (fmap.length - 1) col
        |> Seq.fold countVisible startState
        |> snd

    sideLow * sideHigh * lenLow * lenHigh

let part2 path =
    let fmap = load path

    candidates fmap
    |> Seq.map (scenicScore fmap)
    |> Seq.max
    |> System.Console.WriteLine
