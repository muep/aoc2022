module Aoc2022.Tests.Day09Tests

open Aoc2022
open Xunit

let inputpath filename = __SOURCE_DIRECTORY__ + "/../Aoc2022/input/" + filename

[<Fact>]
let ``Day 09 part 2`` () =
    Assert.Equal(13, "day-09.ex" |> inputpath |> Day09.part1Result)
    Assert.Equal(5735, "day-09.txt" |> inputpath |> Day09.part1Result)
    