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

let visible fmap (row, col) =
    let heightHere = height fmap (row, col)

    let sidewaysLowVisible =
        [ 0 .. col - 1 ]
        |> Seq.filter (fun c -> col <> c)
        |> Seq.forall (fun c -> height fmap (row, c) < heightHere)

    let sidewaysHighVisible =
        [ col + 1 .. fmap.width - 1 ]
        |> Seq.filter (fun c -> col <> c)
        |> Seq.forall (fun c -> height fmap (row, c) < heightHere)

    let lenLowVisible =
        [ 0 .. row - 1 ]
        |> Seq.filter (fun r -> row <> r)
        |> Seq.forall (fun r -> height fmap (r, col) < heightHere)

    let lenHighVisible =
        [ row + 1 .. fmap.length - 1 ]
        |> Seq.filter (fun r -> row <> r)
        |> Seq.forall (fun r -> height fmap (r, col) < heightHere)

    sidewaysLowVisible || sidewaysHighVisible || lenLowVisible || lenHighVisible

let part1 path =
    let fmap = load path

    candidates fmap
    |> Seq.filter (visible fmap)
    |> Seq.length
    |> System.Console.WriteLine

let part2 path = ()
