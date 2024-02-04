module LogLevels

// https://exercism.org/tracks/fsharp/exercises/log-levels

let message (logLine: string) : string = ((logLine.Split ':')[1]).Trim()

let logLevel (logLine: string) : string =
    let splitted = (logLine.Split ':')[0]
    splitted[ 1 .. (splitted.Length - 2) ].ToLower()

let reformat (logLine: string) : string =
    let message = message logLine
    let level = logLevel logLine
    $"{message} ({level})"


let shouldEqual (equal: string) (test: string) : bool = equal = test

let results: bool list =
    [ reformat "[ERROR]: Segmentation fault"
      |> shouldEqual "Segmentation fault (error)"
      reformat "[WARNING]: Decreased performance"
      |> shouldEqual "Decreased performance (warning)"
      reformat "[INFO]: Disk defragmented"
      |> shouldEqual "Disk defragmented (info)"
      reformat "[ERROR]: \t Corrupt disk\t \t \r\n"
      |> shouldEqual "Corrupt disk (error)" ]

let success = not (results |> List.exists (fun e -> not e))

printfn $"Success: {success}"
