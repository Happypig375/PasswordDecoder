open System
let inline flip f x y = f y x
let inline (|LowerCase|UpperCase|NotAlphabet|) c = if 'a' <= c && c <= 'z' then LowerCase
                                                   elif 'A' <= c && c <= 'Z' then UpperCase
                                                   else NotAlphabet
module CharCode =
    [<Struct>] type Case = Upper | Lower
    [<Struct>] type CharCode = CharCode of struct(int * Case)
    let create = function
        | LowerCase as c -> struct(int c - int 'a', Lower)|> CharCode |> ValueSome
        | UpperCase as c -> struct(int c - int 'A', Upper) |> CharCode |> ValueSome
        | NotAlphabet -> ValueNone
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
//An coding method using a 
let twoLetterDecode input alphabetMap =
    let map =
        alphabetMap
        |> Seq.zip <| Seq.init 26 (char >> (+) 'A')
        |> Seq.fold (fun map (k, v) -> Map.add k v map) Map.empty
    input
    |> Seq.fold (fun state c ->
        match state with
        | Some acc, prev ->
            match prev with
            | ValueSome p ->
                match c with
                | LowerCase | UpperCase ->
                    match Map.tryFind (p, c) map with
                    | Some mappedChar -> mappedChar::acc |> Some, ValueNone
                    | None -> None, ValueNone
                | NotAlphabet -> c::acc |> Some, ValueNone
            | ValueNone ->
                match c with
                | LowerCase | UpperCase -> acc |> Some, ValueSome c
                | NotAlphabet -> c::acc |> Some, ValueNone
        | None, _ -> state) (Some [], ValueNone)
    |> fst
    |> Option.map (List.rev //Using :: makes a reversed list, so reversing here to get original
        >> String.Concat)

let twoCharDecodeAllPermutationsOf input listToPermute makeAlphabetMap insertExtraToCodeString =
    // From: http://www.fssnip.net/4u/title/Very-Fast-Permutations
    // Original: http://stackoverflow.com/questions/286427/calculating-permutations-in-f
    // Much faster than anything else I've tested
    let rec insertions x = function
        | []             -> [[x]]
        | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))
    let rec permutations = function
        | []      -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))
    permutations listToPermute
    |> Seq.distinct
    |> Seq.sort
    |> Seq.choose (fun code -> makeAlphabetMap code
                               |> twoLetterDecode input
                               |> Option.map (fun decoded -> code |> String.Concat<char> |> insertExtraToCodeString, decoded))
    
//e.g. Using ABCDE, AA -> A; AB -> B; ...; AE -> E; BA -> F; BB -> G; BC -> H; ...; ED -> X; EE -> Y; ZZ -> Z
let twoCharDecode5 input =
    let input = List.ofSeq input
    let rec decode5Or6 = function
        | [_] | [_; _] | [_; _; _] | [_; _; _; _] | [_; _; _; _; _] as list -> decode5Or6 ('?'::list)
        | [_; _; _; _; _; last] as list -> twoCharDecodeAllPermutationsOf input list
                                                (fun l -> let first5, last = List.splitAt 5 l
                                                          let last = List.exactlyOne last
                                                          Seq.append (Seq.allPairs first5 first5) [last, last])
                                                (flip (+) <| sprintf "(Z=%c)" last)
        | _ -> Seq.empty
    let allDecoded = input |> List.filter (fun x -> 'A' <= x && x <= 'Z') |> List.distinct |> decode5Or6
    if Seq.isEmpty allDecoded then ValueNone else ValueSome allDecoded

//e.g. Using ABCDEF, AA -> A; AB -> B; ...; AE -> E; AF -> F; BA -> G; BB -> H; ...; DF -> X; EA -> Y; EB -> Z; EC -> (invalid)
let twoCharDecode6 input =
    let input = List.ofSeq input
    let rec decode5Or6 = function
        | [_] | [_; _] | [_; _; _] | [_; _; _; _] | [_; _; _; _; _] as list -> decode5Or6 ('?'::list)
        | [_; _; _; _; _; _] as list -> twoCharDecodeAllPermutationsOf input list (fun l -> List.allPairs l l) id
        | _ -> Seq.empty
    let allDecoded = input |> List.filter (fun x -> 'A' <= x && x <= 'Z') |> List.distinct |> decode5Or6
    if Seq.isEmpty allDecoded then ValueNone else ValueSome allDecoded
    
///Like a telephone keypad, each number button has its corrosponding alphabets
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
        twoLetterDecode input map
    | _ -> None
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
    | Some decoded -> printfn "Telephone decode: %s" decoded
    | None -> printfn "Telephone decode not applicable"
    for i in 1..13 do printfn "Left shift %d: %s" i <| leftShift i input
    for i in 1..13 do printfn "Right shift %d: %s" i <| rightShift i input
    match twoCharDecode5 input with
    | ValueSome list -> for key, decoded in list do
                            printfn "Two-char decode using %s: %s" key decoded
    | ValueNone -> printfn "Two-char decode not applicable"
    match twoCharDecode6 input with
    | ValueSome list -> for key, decoded in list do
                            printfn "Two-char decode using %s: %s" key decoded
    | ValueNone -> printfn "Two-char decode not applicable"
    printfn ""
    loop ()
loop ()
//Test case: AAABACADAEBABBBCBDBECACBCCCDCEDADBDCDDDEEAEBECEDEEZZ