open System

type Tree<'T> =
    | Empty
    | Node of 'T * Tree<'T> * Tree<'T>

let rec insert value tree =
    match tree with
    | Empty -> Node (value, Empty, Empty)
    | Node (v, left, right) ->
        if value < v then
            Node (v, insert value left, right)
        elif value > v then
            Node (v, left, insert value right)
        else tree

let fromList values =
    List.fold (fun acc v -> insert v acc) Empty values

let rec toList tree =
    match tree with
    | Empty -> []
    | Node (v, left, right) ->
        (toList left) @ [v] @ (toList right)

let print tree =
    printfn "Содержимое дерева: %A" (toList tree)

let rec foldTree f acc tree =
    match tree with
    | Empty -> acc
    | Node (v, left, right) ->
        let accLeft = foldTree f acc left
        let accNode = f v accLeft
        foldTree f accNode right

let getEvenElements tree =
    foldTree (fun x acc ->
        if x % 2 = 0 then x :: acc else acc) [] tree
    |> List.rev

let rec readInts currentList =
    printf "Введите целое число (или пустую строку для окончания ввода): "
    let input = Console.ReadLine ()
    if String.IsNullOrEmpty input then
        List.rev currentList
    else
        match Int32.TryParse input with
        | true, num -> readInts (num :: currentList)
        | false, _ ->
            printfn "Ошибка: введите корректное целое число."
            readInts currentList

[<EntryPoint>]
let main argv =


    let numbers = readInts []

    if List.isEmpty numbers then
        printfn "Не введено ни одного числа. Выход."
        0
    else
        let tree = fromList numbers
        printfn "\nИсходное дерево:"
        print tree

        let evens = getEvenElements tree
        printfn "\nСписок чётных элементов: %A" evens

        0