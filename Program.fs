// Learn more about F# at http://fsharp.org
open System
open Busqueda
open Busqueda.Ejemplos

[<EntryPoint>]
let main argv =
    printfn "Lenght: %A" (List.length [1;2;3;4;5;])
    printfn "Map : %A"(List.map( fun x -> x * x ) [1;2;3;4;5;]) 
    printfn "Fold : %A"(List.fold( fun x y ->  x + " " + string y ) "" [1;2;3;4;5;]) 
    
    let algo = (Vacuum.test, 50)

    match Vacuum.test BFS.Strategy with
        | Some n -> printfn "Resultado %A" (Chapter3.action n)
        | None -> printfn "No solucion"

    match Vacuum.test DFS.Strategy with
        | Some n -> printfn "Resultado: %A" (Chapter3.action n)
        | None -> printfn "No sol"

    //match Vacuum.test DFSL.Strategy with
        //| Some n -> printfn "Resultado: %A" (Chapter3.action n)
        //| None -> printfn "No sol"

    match Puzzle.test BFS.Strategy with
        | Some n -> printfn "Resultado %A" (Chapter3.action n)
        | None -> printfn "No Solucion"
        
    
    let heuristic1 = float <<HPuzzle.heuristicFunction1 (1,2,3,4,5,6,7,8,0) << (fun n -> n.State)

    let algorithm = Chapter3.graphSearch (AStar.id heuristic1) (AStar.Strategy heuristic1)

    let algorithm2 = Chapter3.graphSearch (Greedy.id) (Greedy.Strategy heuristic1)

    match HPuzzle.test algorithm HPuzzle.initialState_23 with  
        | Some n -> printfn "Resultado %A" (Chapter3.action n)
        | None -> printfn "No Solucion"

    match HPuzzle.test algorithm2 HPuzzle.initialState_23 with  
        | Some n -> printfn "Resultado %A" (Chapter3.action n)
        | None -> printfn "No Solucion"
    0 // return an integer exit code
