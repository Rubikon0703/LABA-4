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

let rec map f tree =
    match tree with
    | Empty -> Empty
    | Node(v, left, right) -> Node(f v, map f left, map f right)

let addCharToStrings ch tree = map (fun s -> s + string ch) tree

// Генерация случайных строк
let random = Random ()
let generateRandomString minLen maxLen =
    let len = random.Next(minLen, maxLen + 1)
    let chars = 
        Array.init len (fun _ -> char (random.Next(97, 123))) 
        // 'a'..'z'
    String chars

let rec readInt prompt (validator: int -> bool) errorMsg =
    printf "%s" prompt
    match Int32.TryParse (Console.ReadLine ()) with
    | true, x when validator x -> x
    | _ ->
        printfn "%s" errorMsg
        readInt prompt validator errorMsg

let rec readChar prompt =
    printf "%s" prompt
    let input = Console.ReadLine ()
    if String.IsNullOrEmpty input then
        printfn "Cимвол не может быть пустым. Повторите ввод."
        readChar prompt
    else
        input.[0]

[<EntryPoint>]
let main argv =
    let count = readInt "Введите количество строк: " 
                    (fun x -> x > 0)   
                    "Ошибка: введите положительное целое число."
    let minLen = readInt "Введите минимальную длину строки: "  
                    (fun x -> x > 0) 
                    "Ошибка: длина должна быть положительной."
    let maxLen = readInt "Введите максимальную длину строки: " 
                    (fun x -> x >= minLen) 
                    "максимальная длина меньше минимальной."

   
    let strings =   
        List.init count     
            (fun _ -> generateRandomString minLen maxLen)
    printfn "\nСгенерированный список строк: %A" strings

  
    let originalTree = fromList strings
    printfn "\nИсходное дерево:"
    print originalTree
    let ch = readChar "Введите символ для добавления: "
    let newTree = addCharToStrings ch originalTree
    printfn "\nДерево после добавления символа '%c':" ch
    print newTree
    0
