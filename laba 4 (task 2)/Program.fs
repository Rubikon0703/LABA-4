open System

type Tree<'T> =
    | Empty
    | Node of 'T * Tree<'T> * Tree<'T>

let rec insert value tree =
    match tree with
    | Empty -> Node (value, Empty, Empty)
    | Node (v, left, right) ->
        if value < v then Node (v, insert value left, right)
        elif value > v then Node (v, left, insert value right)
        else tree

let fromList values =  
    List.fold (fun acc v -> insert v acc) Empty values

let rec printTreeIndent indent tree =
    match tree with
    | Empty -> ()
    | Node (v, left, right) ->
        printTreeIndent (indent + "    ") right
        printfn "%s%O" indent v
        printTreeIndent (indent + "    ") left

let print tree =
    printfn "Дерево:"
    printTreeIndent "" tree

let rec foldTree f acc tree =
    match tree with
    | Empty -> acc
    | Node (v, left, right) ->
        let accLeft = foldTree f acc left
        let accNode = f v accLeft
        foldTree f accNode right


let rec toList tree =
    match tree with
    | Empty -> []
    | Node (v, left, right) ->
        (toList left) @ [v] @ (toList right)

let getEvenElementsViaTreeFold tree =
    let rec collectEven tree acc =
        match tree with
        | Empty -> acc
        | Node (v, left, right) ->
            let accAfterLeft = collectEven left acc
            let accAfterCurrent = 
                if v % 2 = 0 then 
                    List.fold (fun accList x -> x :: accList) accAfterLeft [v]
                else accAfterLeft
            collectEven right accAfterCurrent
    
    collectEven tree [] |> List.rev

let getEvenElementsViaListFold tree =
    tree
    |> toList  
    |> List.fold (fun acc x -> 
        if x % 2 = 0 then x :: acc  
        else acc) []
    |> List.rev  


let random = Random ()
let generateRandomInt minVal maxVal =
    random.Next(minVal, maxVal + 1)

let rec readInt prompt (validator: int -> bool) errorMsg =
    printf "%s" prompt
    match Int32.TryParse (Console.ReadLine ()) with
    | true, x when validator x -> x
    | _ ->
        printfn "%s" errorMsg
        readInt prompt validator errorMsg

[<EntryPoint>]
let main argv =
    let count = readInt "Введите количество чисел: " 
                  (fun x -> x > 0)
                  "Ошибка: введите положительное целое число."
    let minVal = readInt "Введите минимальное значение: " 
                  (fun _ -> true) 
                  "Ошибка: введите целое число."
    let maxVal = readInt "Введите максимальное значение: " 
                  (fun x -> x >= minVal) 
                  "Максимальное значение меньше минимального."
    let numbers = 
        List.init count 
            (fun _ -> generateRandomInt minVal maxVal)
    printfn "\nСгенерированный список чисел: %A" numbers
    let tree = fromList numbers
    printfn "\nИсходное дерево:"
    print tree
    let evens1 = getEvenElementsViaTreeFold tree
    let evens2 = getEvenElementsViaListFold tree
    printfn "\nСписок чётных элементов: %A" evens1
    printfn "\nСписок чётных элементов: %A" evens2
    0