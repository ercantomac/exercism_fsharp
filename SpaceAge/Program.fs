module SpaceAge

// https://exercism.org/tracks/fsharp/exercises/space-age

type Planet =
    | Mercury
    | Venus
    | Earth
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune

let getPlanetYears (planet: Planet) : float =
    match planet with
    | Mercury -> 0.2408467
    | Venus -> 0.61519726
    | Earth -> 1.0
    | Mars -> 1.8808158
    | Jupiter -> 11.862615
    | Saturn -> 29.447498
    | Uranus -> 84.016846
    | Neptune -> 164.79132

let earthSeconds = (60.0 * 60.0 * 24.0 * 365.25)

let age (planet: Planet) (seconds: int64) : float =
    (float seconds / earthSeconds) / getPlanetYears planet

let shouldEqual (equal: float) (test: float) : bool = equal = System.Math.Round(test, 2)

let results: bool list =
    [ age Earth 1000000000L |> shouldEqual 31.69
      age Mercury 2134835688L |> shouldEqual 280.88
      age Venus 189839836L |> shouldEqual 9.78
      age Mars 2129871239L |> shouldEqual 35.88
      age Jupiter 901876382L |> shouldEqual 2.41
      age Saturn 2000000000L |> shouldEqual 2.15
      age Uranus 1210123456L |> shouldEqual 0.46
      age Neptune 1821023456L |> shouldEqual 0.35 ]

let success = not (results |> List.exists (fun e -> not e))

printfn $"Success: {success}"
