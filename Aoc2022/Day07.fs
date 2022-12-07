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
      totalSize: int
      dirsizes: Map<string list, int> }

let getSize (sizes: Map<string list,int>) (dir:string list) =
    match sizes.TryFind(dir) with
    | Some(a) -> a
    | None -> 0

let incrementSizes (cwd: string list) (cwsize: int) (dirsizes: Map<string list, int>) =
    cwd
    |> Seq.fold (fun subs d -> (d :: (List.head subs)) :: subs) [ [] ]
    |> Seq.fold (fun (dszs: Map<string list, int>) d -> dszs.Add(d, cwsize + (getSize dszs d)))
                dirsizes

let updateState state proc =
    match proc.cmd with
    | Cd ("/") -> { state with cwd = [] }
    | Cd (where) -> { state with cwd = where :: state.cwd }
    | Up -> { state with cwd = List.tail state.cwd }
    | Ls -> { state with
               totalSize = state.totalSize + (lsFileSize proc.output)
               dirsizes = incrementSizes state.cwd (lsFileSize proc.output) state.dirsizes}

let showCmds cmds =
    for cmd in cmds do
        printfn "%A" cmd

let trimEnds (s: string) = s.Trim()

let notEmpty (s: string) = s.Length > 0

let part1 path =
    System.IO.File.ReadAllText path
    |> (fun a -> a.Split "$ ")
    |> Seq.map trimEnds
    |> Seq.filter notEmpty
    |> Seq.map readProcess
    |> Seq.fold
        updateState
        { cwd = []
          totalSize = 0
          dirsizes = Map.empty }
    |> System.Console.WriteLine
