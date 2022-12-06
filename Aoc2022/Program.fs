type Solution =
    { part1: string -> unit
      part2: string -> unit }

let solutions =
    [ (1,
       { part1 = Aoc2022.Day01.part1
         part2 = Aoc2022.Day01.part2 })
      (2,
       { part1 = Aoc2022.Day02.part1
         part2 = Aoc2022.Day02.part2 })
      (3,
       { part1 = Aoc2022.Day03.part1
         part2 = Aoc2022.Day03.part2 })
      (4,
       { part1 = Aoc2022.Day04.part1
         part2 = Aoc2022.Day04.part2 })
      (5,
       { part1 = Aoc2022.Day05.part1
         part2 = Aoc2022.Day05.part2 })
      (6,
       { part1 = Aoc2022.Day06.part1
         part2 = Aoc2022.Day06.part2 }) ]
    |> Map.ofList

let runAll () =
    Aoc2022.Day01.part1 "input/day-01.ex"
    Aoc2022.Day01.part1 "input/day-01.txt"
    Aoc2022.Day01.part2 "input/day-01.txt"
    Aoc2022.Day02.part1 "input/day-02.ex"
    Aoc2022.Day02.part1 "input/day-02.txt"
    Aoc2022.Day02.part2 "input/day-02.txt"
    Aoc2022.Day03.part1 "input/day-03.ex"
    Aoc2022.Day03.part1 "input/day-03.txt"
    Aoc2022.Day03.part2 "input/day-03.txt"
    Aoc2022.Day04.part1 "input/day-04.ex"
    Aoc2022.Day04.part1 "input/day-04.txt"
    Aoc2022.Day04.part2 "input/day-04.txt"
    Aoc2022.Day05.part1 "input/day-05.ex"
    Aoc2022.Day05.part1 "input/day-05.txt"
    Aoc2022.Day05.part2 "input/day-05.ex"
    Aoc2022.Day05.part2 "input/day-05.txt"
    Aoc2022.Day06.part1 "input/day-06.ex3"
    Aoc2022.Day06.part1 "input/day-06.txt"
    Aoc2022.Day06.part2 "input/day-06.txt"

let runDay inputPath day =
    solutions.[day].part1 inputPath
    solutions.[day].part2 inputPath
    ()

let runDayDefault day =
    runDay $"input/day-%02d{day}.txt" day

let usageText = """
usage:
    Aoc2022          this help
    Aoc2022 all      run all the implemented solutions
    Aoc2022 day      run both parts of a daily solution with default input
    Aoc2022 day path run both parts of a daily solution with given input
"""

[<EntryPoint>]
let main args =
    match args with
    | [||] -> System.Console.WriteLine usageText
    | [| "all" |] -> runAll ()
    | [| "day"; num |] -> num |> System.Int32.Parse |> runDayDefault
    | [| "day"; num; inputPath |] -> num |> System.Int32.Parse |> runDay inputPath
    | _ -> invalidArg $"%A{args}" "Unexpected command line syntax"

    0
