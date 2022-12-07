module Aoc2022.Day07

type Cmd =
    | Cd of string
    | Up
    | Ls

let readCmd (s: string) =
    match s.Split(" ") with
    | [| "cd"; ".." |] -> Up
    | [| "cd"; path |] -> Cd(path)
    | [| "ls" |] -> Ls
    | _ -> invalidArg s "not recognizable command"

type Process = { cmd: Cmd; output: string }

let readProcess (s: string) =
    match s.IndexOf("\n") with
    | -1 -> { cmd = readCmd s; output = "" }
    | n ->
        { cmd = readCmd (s.Substring(0, n))
          output = s.Substring(n + 1) }

let lsFileSize (lsOut: string) =
    lsOut.Split("\n")
    |> Seq.filter ((fun (a: string) -> a.StartsWith("dir ")) >> not)
    |> Seq.map (fun (ln: string) -> ln.Split(" ") |> Seq.head |> System.Int32.Parse)
    |> Seq.sum

type State =
    { cwd: string list
      dirsizes: Map<string list, int> }

let getOrZero (sizes: Map<string list, int>) (dir: string list) =
    match sizes.TryFind(dir) with
    | Some a -> a
    | None -> 0

let incrementSizes (cwd: string list) (cwsize: int) (dirsizes: Map<string list, int>) =
    cwd
    |> List.rev
    |> Seq.fold (fun subs d -> (d :: (List.head subs)) :: subs) [ [] ]
    |> Seq.fold (fun (dszs: Map<string list, int>) d -> dszs.Add(d, cwsize + (getOrZero dszs d))) dirsizes

let updateState state proc =
    match proc.cmd with
    | Cd "/" -> { state with cwd = [] }
    | Cd where -> { state with cwd = where :: state.cwd }
    | Up -> { state with cwd = List.tail state.cwd }
    | Ls -> { state with dirsizes = incrementSizes state.cwd (lsFileSize proc.output) state.dirsizes }

let trimEnds (s: string) = s.Trim()

let notEmpty (s: string) = s.Length > 0

let getSizes path =
    System.IO.File.ReadAllText path
    |> (fun a -> a.Split "$ ")
    |> Seq.map trimEnds
    |> Seq.filter notEmpty
    |> Seq.map readProcess
    |> Seq.fold updateState { cwd = []; dirsizes = Map.empty }
    |> (fun state -> state.dirsizes)

let part1 path =
    getSizes path
    |> Map.filter (fun _ sz -> sz <= 100000)
    |> Map.fold (fun acc _ sz -> acc + sz) 0
    |> System.Console.WriteLine

let capacity = 70000000
let required = 30000000

let part2 path =
    let dirsizes = getSizes path

    let minSize =
        required + dirsizes[[]] - capacity

    dirsizes
    |> Map.filter (fun _ size -> size >= minSize)
    |> Map.toSeq
    |> Seq.map (fun (_, size) -> size)
    |> Seq.min
    |> System.Console.WriteLine
