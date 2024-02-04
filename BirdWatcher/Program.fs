module BirdWatcher

// https://exercism.org/tracks/fsharp/exercises/bird-watcher

let lastWeek: int [] = [| 0; 2; 5; 3; 7; 8; 4 |]

let yesterday (counts: int []) : int = counts[counts.Length - 2]

let total (counts: int []) : int =
    counts
    |> Array.fold (fun accumulation birds -> accumulation + birds) 0

let dayWithoutBirds (counts: int []) : bool = counts |> Array.contains 0

let incrementTodaysCount (counts: int []) : int [] =
    let newCounts = counts
    newCounts[newCounts.Length - 1] <- newCounts[newCounts.Length - 1] + 1
    newCounts

let unusualWeek (counts: int []) : bool =
    match counts with
    | [| _; 0; _; 0; _; 0; _ |] -> true
    | [| _; 10; _; 10; _; 10; _ |] -> true
    | [| 5; _; 5; _; 5; _; 5 |] -> true
    | _ -> false

let shouldEqualInt (equal: int) (test: int) : bool = equal = test

let shouldEqualBool (equal: bool) (test: bool) : bool = equal = test

let totalBirdsResults: bool list =
    [ total [| 0; 0; 2; 0; 0; 1; 0 |]
      |> shouldEqualInt 3
      total [| 5; 9; 12; 6; 8; 8; 17 |]
      |> shouldEqualInt 65 ]

let unusualWeekResults: bool list =
    [ unusualWeek [| 1; 0; 2; 0; 3; 0; 4 |]
      |> shouldEqualBool true
      unusualWeek [| 10; 0; 6; 0; 9; 0; 4 |]
      |> shouldEqualBool true
      unusualWeek [| 6; 10; 2; 10; 5; 10; 8 |]
      |> shouldEqualBool true
      unusualWeek [| 16; 10; 8; 10; 4; 10; 7 |]
      |> shouldEqualBool true
      unusualWeek [| 5; 1; 5; 2; 5; 3; 5 |]
      |> shouldEqualBool true
      unusualWeek [| 5; 12; 5; 6; 5; 5; 5 |]
      |> shouldEqualBool true
      unusualWeek [| 2; 2; 1; 0; 1; 1; 1 |]
      |> shouldEqualBool false
      unusualWeek [| 2; 0; 1; 1; 1; 0; 1 |]
      |> shouldEqualBool false
      unusualWeek [| 2; 9; 1; 10; 1; 11; 1 |]
      |> shouldEqualBool false
      unusualWeek [| 5; 0; 5; 1; 4; 0; 6 |]
      |> shouldEqualBool false ]

let success =
    not (
        totalBirdsResults |> List.exists (fun e -> not e)
        || unusualWeekResults |> List.exists (fun e -> not e)
    )

printfn $"Success: {success}"
