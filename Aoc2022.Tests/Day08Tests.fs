module Aoc2022.Tests.Day08Tests

open Aoc2022
open Xunit

let inputpath filename = __SOURCE_DIRECTORY__ + "/../Aoc2022/input/" + filename

[<Fact>]
let ``Day 08 part 2 example`` () =
    Assert.Equal(8, "day-08.ex" |> inputpath |> Day08.part2Result)

[<Fact>]
let ``Day 08 part 2 full`` () =
    Assert.Equal(444528, "day-08.txt" |> inputpath |> Day08.part2Result)

