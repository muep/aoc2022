module Aoc2022.Tests.Day01Tests

open Aoc2022
open Xunit


let inputPath filename = __SOURCE_DIRECTORY__ + "/../Aoc2022/input/" + filename

[<Fact>]
let ``Day 01 part 1 example`` () =
    Assert.Equal(24000, "day-01.ex" |> inputPath |> Day01.part1Result)

[<Fact>]
let ``Day 01 part 1 full`` () =
    Assert.Equal(69912, "day-01.txt" |> inputPath |> Day01.part1Result)

[<Fact>]
let ``Day 01 part 2 example`` () =
    Assert.Equal(45000, "day-01.ex" |> inputPath |> Day01.part2Result)

[<Fact>]
let ``Day 01 part 2 full`` () =
    Assert.Equal(208180, "day-01.txt" |> inputPath |> Day01.part2Result)
