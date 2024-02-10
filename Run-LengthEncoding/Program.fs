module RunLengthEncoding

// https://exercism.org/tracks/fsharp/exercises/run-length-encoding

let encode (input: string) : string =
    if input = "" then
        ""
    else
        let mutable resultString = ""
        let charList = input.ToCharArray() |> Seq.toList
        let mutable checkForChar = charList[0]
        let mutable counter = 0

        for character in charList do
            if (character = checkForChar) then
                counter <- (counter + 1)
            else
                let stringToAppend =
                    match counter with
                    | 1 -> $"{checkForChar}"
                    | _ -> $"{counter}{checkForChar}"

                resultString <- (resultString + stringToAppend)
                counter <- 1
                checkForChar <- character

        let stringToAppend =
            match counter with
            | 1 -> $"{checkForChar}"
            | _ -> $"{counter}{checkForChar}"

        resultString <- (resultString + stringToAppend)

        resultString

let decode (input: string) =
    let mutable resultString = ""
    let charList = input.ToCharArray() |> Seq.toList
    let mutable number = 0
    let mutable digitCounter = 1

    for character in charList do
        if (System.Char.IsNumber(character)) then
            let numericValue = System.Char.GetNumericValue(character) |> int
            number <- (number * digitCounter)
            number <- (number + numericValue)
            digitCounter <- (digitCounter * 10)
        else
            if number = 0 then number <- 1

            for i in 1..number do
                resultString <- (resultString + $"{character}")

            number <- 0
            digitCounter <- 1

    resultString

let shouldEqual (equal: string) (test: string) : bool = equal = test

let results: bool list =
    [ encode "" |> shouldEqual ""
      encode "XYZ" |> shouldEqual "XYZ"
      encode "AABBBCCCC" |> shouldEqual "2A3B4C"
      encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"
      |> shouldEqual "12WB12W3B24WB"
      encode "  hsqq qww  "
      |> shouldEqual "2 hs2q q2w2 "
      encode "aabbbcccc" |> shouldEqual "2a3b4c"
      decode "" |> shouldEqual ""
      decode "XYZ" |> shouldEqual "XYZ"
      decode "2A3B4C" |> shouldEqual "AABBBCCCC"
      decode "12WB12W3B24WB"
      |> shouldEqual "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"
      decode "2 hs2q q2w2 "
      |> shouldEqual "  hsqq qww  "
      decode "2a3b4c" |> shouldEqual "aabbbcccc"
      "zzz ZZ  zZ"
      |> encode
      |> decode
      |> shouldEqual "zzz ZZ  zZ" ]

let success = not (results |> List.exists (fun e -> not e))

printfn $"Success: {success}"
