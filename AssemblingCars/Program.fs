module CarsAssemble

// https://exercism.org/tracks/fsharp/exercises/cars-assemble

let successRate (speed: int) : float =
    let percentage =
        match speed with
        | 0 -> 0.0
        | speed when speed >= 1 && speed <= 4 -> 100.0
        | speed when speed >= 5 && speed <= 8 -> 90.0
        | 9 -> 80.0
        | 10 -> 77.0
        | _ -> 0.0

    percentage / 100.0

let productionRatePerHour (speed: int) : float =
    (float (speed * 221)) * successRate speed

let workingItemsPerMinute (speed: int) : int =
    int ((productionRatePerHour speed) / 60.0)

let shouldEqual (equal: int) (test: int) : bool = equal = test

let results: bool list =
    [ workingItemsPerMinute 0 |> shouldEqual 0
      workingItemsPerMinute 1 |> shouldEqual 3
      workingItemsPerMinute 5 |> shouldEqual 16
      workingItemsPerMinute 8 |> shouldEqual 26
      workingItemsPerMinute 9 |> shouldEqual 26
      workingItemsPerMinute 10 |> shouldEqual 28 ]

let success = not (results |> List.exists (fun e -> not e))

printfn $"Success: {success}"
