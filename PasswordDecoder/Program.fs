open System
let inline flip f x y = f y x
module CharCode =
    [<Struct>] type Case = Upper | Lower
    [<Struct>] type CharCode = CharCode of struct(int * Case)
    let create c =
        if 'a' <= c && c <= 'z' then struct(int c - int 'a', Lower)|> CharCode |> ValueSome
        elif 'A' <= c && c <= 'Z' then struct(int c - int 'A', Upper) |> CharCode |> ValueSome
        else ValueNone
    let private normalized case code =
        struct((let modulo = code % 26 in if modulo < 0 then modulo + 26 else modulo), case) |> CharCode
    let add i (CharCode struct(code, case)) = i + code |> normalized case
    let subtract i (CharCode struct(code, case)) = i - code |> normalized case
    let subtractFrom i (CharCode struct(code, case)) = code - i |> normalized case
    let destruct (CharCode struct(code, case)) = code % 26 + int (match case with Upper -> 'A' | Lower -> 'a') |> char
    let run f c = match create c with ValueSome x -> f x |> destruct | ValueNone -> c
let rightShift amount = CharCode.add amount |> CharCode.run |> String.map
let leftShift amount = CharCode.subtractFrom amount |> CharCode.run |> String.map
let reflect = CharCode.subtract 25 |> CharCode.run |> String.map
let reverse = Seq.rev<char> >> String.Concat
let twoDecode input alphabetMap =
    let map =
        alphabetMap
        |> List.zip <| ['A'..'Z']
        |> List.fold (fun map (k, v) -> Map.add k v map) Map.empty
    input
    |> List.fold (fun (acc, prev) c ->
        match prev with
        | ValueSome p -> (p, c)::acc, ValueNone
        | ValueNone -> acc, ValueSome c) ([], ValueNone)
    |> fst
    |> List.rev //Using :: makes a reversed list, so reversing here to get original
    |> List.map (flip Map.find map)
    |> String.Concat
let twoCharDecode input =
    let input = List.ofSeq input
    match input |> List.filter (fun x -> 'A' <= x && x <= 'Y') |> List.distinct with
    | [_; _; _; _; _] as list ->
        // From: http://www.fssnip.net/4u/title/Very-Fast-Permutations
        // Original: http://stackoverflow.com/questions/286427/calculating-permutations-in-f
        // Much faster than anything else I've tested
        let rec insertions x = function
            | []             -> [[x]]
            | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))
        let rec permutations = function
            | []      -> seq [ [] ]
            | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))
        permutations list
        |> Seq.sort                                                       (*25-char map, Z is its own thing*)
        |> Seq.map (fun code -> code |> String.Concat, twoDecode input <| List.allPairs code code)
        |> ValueSome
    | _ -> ValueNone
let telephoneDecode input =
    let map = ['2', '1' //ABC
               '2', '2'
               '2', '3'
               '3', '1' //DEF
               '3', '2'
               '3', '3'
               '4', '1' //GHI
               '4', '2'
               '4', '3'
               '5', '1' //JKL
               '5', '2'
               '5', '3'
               '6', '1' //MNO
               '6', '2'
               '6', '3'
               '7', '1' //PQRS
               '7', '2'
               '7', '3'
               '7', '4'
               '8', '1' //TUV
               '8', '2'
               '8', '3'
               '9', '1' //WXYZ
               '9', '2'
               '9', '3'
               '9', '4']
    match List.fold (fun struct(prev, state) item -> match prev with
                                                     | ValueSome p ->
                                                       struct(ValueNone, state && List.contains (p, item) map)
                                                     | ValueNone -> struct(ValueSome item, state))
          struct(ValueNone, true) input with
    | struct(ValueNone, true) ->
        twoDecode input map |> ValueSome
    | _ -> ValueNone
let morseDecode (code:string) =
    if String.forall (fun c -> c < 'A' || 'Z' < c && c < 'a' || 'z' < c) code then
        let codeBook = 
            Map ["-", 'T'
                 "--", 'M'
                 "---", 'O'
                 "-----", '0'
                 "----.", '9'
                 "---..", '8'
                 "---...", ':'
                 "--.", 'G'
                 "--.-", 'Q'
                 "--..", 'Z'
                 "--..--", ','
                 "--...", '7'
                 "-.", 'N'
                 "-.-", 'K'
                 "-.--", 'Y'
                 "-.--.", '('
                 "-.--.-", ')'
                 "-.-.", 'C'
                 "-.-.--", '!'
                 "-.-.-.", ';'
                 "-..", 'D'
                 "-..-", 'X'
                 "-..-.", '/'
                 "-...", 'B'
                 "-...-", '='
                 "-....", '6'
                 "-....-", '-'
                 ".", 'E'
                 ".-", 'A'
                 ".--", 'W'
                 ".---", 'J'
                 ".----", '1'
                 ".----.", '\''
                 ".--.", 'P'
                 ".--.-.", '@'
                 ".-.", 'R'
                 ".-.-.", '+'
                 ".-.-.-", '.'
                 ".-..", 'L'
                 ".-..-.", '"'
                 ".-...", '&'
                 "..", 'I'
                 "..-", 'U'
                 "..---", '2'
                 "..--..", '?'
                 "..-.", 'F'
                 "...", 'S'
                 "...-", 'V'
                 "...--", '3'
                 "...-..-", '$'
                 "....", 'H'
                 "....-", '4'
                 ".....", '5']
        code.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun c -> match Map.tryFind c codeBook with Some c -> string c | None -> c)
        |> String.Concat
    else code

let rec loop () =
    printf "Enter encoded message or morse code: "
    let input = Console.ReadLine()
                |> String.collect (function '—' -> "--" | '…' -> "..." | c -> string c) //Thanks, Mac Notes app.
                |> morseDecode

    printfn "Original: %s" input
    printfn "Reverse: %s" <| reverse input
    printfn "Reflect: %s" <| reflect input
    match telephoneDecode (List.ofSeq input) with
    | ValueSome decoded -> printfn "Telephone decode: %s" decoded
    | ValueNone -> printfn "Telephone decode not applicable"
    for i in 1..17 do printfn "Left shift %d: %s" i <| leftShift i input
    for i in 1..18 do printfn "Right shift %d: %s" i <| rightShift i input
    match twoCharDecode input with
    | ValueSome list -> for key, decoded in list do
                            printfn "Two-char decode using %s: %s" key decoded
    | ValueNone -> printfn "Two-char decode not applicable"
    printfn ""
    loop ()
loop ()