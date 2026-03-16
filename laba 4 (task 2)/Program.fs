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

let rec toList tree =
    match tree with
    | Empty -> []
    | Node (v,left,right)->(toList left) @ [v] @ (toList right)

let print tree = printfn "Содержимое дерева: %A" (toList tree)

let rec foldTree f acc tree =
    match tree with
    | Empty -> acc
    | Node (v, left, right) ->
        let accLeft = foldTree f acc left
        let accNode = f v accLeft
        foldTree f accNode right

let getEvenElements tree =
    foldTree (fun x acc -> if x % 2 = 0 then 
                            x :: acc else acc) [] tree
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
    let evens = getEvenElements tree
    printfn "\nСписок чётных элементов: %A" evens
    0