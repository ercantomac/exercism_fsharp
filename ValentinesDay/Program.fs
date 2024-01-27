module ValentinesDay

// https://exercism.org/tracks/fsharp/exercises/valentines-day

type Approval =
    | Yes
    | No
    | Maybe

type Cuisine =
    | Korean
    | Turkish

type Genre =
    | Crime
    | Horror
    | Romance
    | Thriller

type Activity =
    | BoardGame
    | Chill
    | Movie of Genre
    | Restaurant of Cuisine
    | Walk of int

let rateActivity (activity: Activity) : Approval =
    let approval: Approval =
        match activity with
        | BoardGame -> No
        | Chill -> No
        | Movie genre ->
            match genre with
            | Romance -> Yes
            | _ -> No
        | Restaurant cuisine ->
            match cuisine with
            | Korean -> Yes
            | Turkish -> Maybe
        | Walk km ->
            match km with
            | km when km < 3 -> Yes
            | km when km >= 3 && km < 5 -> Maybe
            | _ -> No

    approval


let shouldEqual (equal: Approval) (test: Approval) : bool = equal = test

let results: bool list =
    [ rateActivity BoardGame |> shouldEqual No
      rateActivity Chill |> shouldEqual No
      rateActivity (Movie Crime) |> shouldEqual No
      rateActivity (Movie Horror) |> shouldEqual No
      rateActivity (Movie Romance) |> shouldEqual Yes
      rateActivity (Movie Thriller) |> shouldEqual No
      rateActivity (Restaurant Korean)
      |> shouldEqual Yes
      rateActivity (Restaurant Turkish)
      |> shouldEqual Maybe
      rateActivity (Walk 1) |> shouldEqual Yes
      rateActivity (Walk 2) |> shouldEqual Yes
      rateActivity (Walk 3) |> shouldEqual Maybe
      rateActivity (Walk 4) |> shouldEqual Maybe
      rateActivity (Walk 5) |> shouldEqual No
      rateActivity (Walk 8) |> shouldEqual No ]

let success = not (results |> List.exists (fun e -> not e))

printfn $"Success: {success}"
