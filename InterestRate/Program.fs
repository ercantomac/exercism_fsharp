module InterestIsInteresting

// https://exercism.org/tracks/fsharp/exercises/interest-is-interesting

let interestRate (balance: decimal) : single =
    match balance with
    | balance when balance < 0.0m -> 3.213f
    | balance when balance >= 0.0m && balance < 1000.0m -> 0.5f
    | balance when balance >= 1000.0m && balance < 5000.0m -> 1.621f
    | _ -> 2.475f

let interest (balance: decimal) : decimal =
    balance * decimal (interestRate balance) / 100.0m

let annualBalanceUpdate (balance: decimal) : decimal = balance + interest balance

let amountToDonate (balance: decimal) (taxFreePercentage: float) : int =
    match balance with
    | balance when balance < 0.0m -> 0
    | _ ->
        int (
            balance / 100.0m
            * decimal taxFreePercentage
            * 2.0m
        )

let shouldEqualDecimal (equal: decimal) (test: decimal) : bool = equal = test

let shouldEqualInt (equal: int) (test: int) : bool = equal = test

let annualBalanceUpdateResults: bool list =
    [ annualBalanceUpdate 0.0m
      |> shouldEqualDecimal 0.0000m
      annualBalanceUpdate 0.000001m
      |> shouldEqualDecimal 0.000001005m
      annualBalanceUpdate 1_000.0m
      |> shouldEqualDecimal 1016.210000m
      annualBalanceUpdate 1_000.0001m
      |> shouldEqualDecimal 1016.210101621m
      annualBalanceUpdate 898124017.826243404425m
      |> shouldEqualDecimal 920352587.26744292868451875m
      annualBalanceUpdate -0.123M
      |> shouldEqualDecimal -0.12695199M
      annualBalanceUpdate -152964.231M
      |> shouldEqualDecimal -157878.97174203M ]

let amountToDonateResults: bool list =
    [ amountToDonate 0.0m 2.0 |> shouldEqualInt 0
      amountToDonate 0.000001m 2.1 |> shouldEqualInt 0
      amountToDonate 1_000.0m 2.0 |> shouldEqualInt 40
      amountToDonate 1_000.0001m 0.99
      |> shouldEqualInt 19
      amountToDonate 898124017.826243404425m 2.65
      |> shouldEqualInt 47600572
      amountToDonate -0.123M 3.33 |> shouldEqualInt 0
      amountToDonate -152964.231M 5.4
      |> shouldEqualInt 0 ]

let success =
    not (
        annualBalanceUpdateResults
        |> List.exists (fun e -> not e)
        || amountToDonateResults
           |> List.exists (fun e -> not e)
    )

printfn $"Success: {success}"
