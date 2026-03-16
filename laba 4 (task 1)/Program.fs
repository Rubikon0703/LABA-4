open System

type Tree<'T> =
    | Empty
    | Node of 'T * Tree<'T> * Tree<'T>

let rec map f tree =
    match tree with
    | Empty -> Empty
    | Node (v, left, right) ->
        Node (f v, map f left, map f right)

let addCharToStrings ch tree =
    map (fun s -> s + string ch) tree

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

let rec readStrings currentList =
    printf "Введите строку (или пустую для окончания ввода): "
    match Console.ReadLine () with
    | "" -> List.rev currentList
    | s -> readStrings (s :: currentList)

let rec readChar () =
    printf "Введите один символ для добавления в конец строк: "
    let input = Console.ReadLine ()
    if String.IsNullOrEmpty input then
        printfn "!символ не может быть пустым! Повторите ввод."
        readChar ()
    elif input.Length > 1 then
        printfn ("Предупреждение:введено более одного символа.") 
        printfn ("Использую первый символ '%c'.") input.[0]
        input.[0]
    else
        input.[0]

[<EntryPoint>]
let main argv =

    let strings = readStrings []

    if List.isEmpty strings then
        printfn "Не введено ни одной строки. Выход."
        0
    else
        let ch = readChar ()
        let originalTree = fromList strings
        printfn "\nИсходное дерево:"
        print originalTree

        let newTree = addCharToStrings ch originalTree
        printfn "\nДерево после добавления символа '%c':" ch
        print newTree

       
        0