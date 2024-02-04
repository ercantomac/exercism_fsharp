module NthPrime

// https://exercism.org/tracks/fsharp/exercises/nth-prime

let prime nth : int option =
    if nth = 0 then
        None
    elif nth = 1 then
        Some(2)
    else
        let mutable number = 3
        let mutable index = 2

        while index < nth do
            number <- (number + 2)
            let squareRoot = (sqrt (float number)) |> int
            let testDividers = [ 3..squareRoot ]

            let canBeDivided =
                testDividers
                |> List.exists (fun divider -> number % divider = 0)

            if not canBeDivided then
                index <- (index + 1)

        Some(number)

let shouldEqual (equal: int option) (test: int option) : bool = equal = test

let results: bool list =
    [ prime 1 |> shouldEqual (Some 2)
      prime 2 |> shouldEqual (Some 3)
      prime 6 |> shouldEqual (Some 13)
      prime 10001 |> shouldEqual (Some 104743)
      prime 0 |> shouldEqual None ]

let success = not (results |> List.exists (fun e -> not e))

printfn $"Success: {success}"
