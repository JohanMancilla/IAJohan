namespace Busqueda

(*
Definicion Option esto para evitar el uso de el termino NULL para los punteros
None es tipo de dato vacio o que no precede a nada
y Some es que tiene un hijo
type option<'a> =
    | Some of 'a
    | None 
*)
//Estructura de datos de los nodos para el problema que hacer
//Tiene dos atributos siendo un estado y una accion
type Node<'s,'a> = 
    {
        Deep: int
        Path_Cost: float
        State: 's
        Father: Node<'s,'a> option
        Action: 'a option
    }
//Estructura de datos que contiene al problema
//Tiene dos atributos siendo un estado y una accion
type Problem<'s,'a> =
    {
        Init: 's
        F_Goal: 's -> bool
        F_Sussecion: 's -> list<'a * 's> 
        F_Cost: 's -> 'a -> 's -> float
    }   
//Estructura de datos que tiene a la estrategia a seguir 
//Tiene dos atributos siendo un estado y una accion y una bolsa
type Strategy<'s,'a,'b> = 
    {
        Next: 'b -> (Node<'s,'a> * 'b) option 
        Add: 'b -> Node<'s,'a> -> 'b
        Init: Node<'s,'a> -> 'b
    }   
type Stadistics = 
    {
        Generted_Nodes: uint64
        Procesed_Nodes: uint64
    }    
module Chapter3 = 
    let expand  father problem = 
        problem.F_Sussecion father.State
        |> List.map (fun (action, state) -> 
            {
                State = state
                Father = Some father
                Deep = father.Deep + 1
                Action = Some action 
                Path_Cost = father.Path_Cost + problem.F_Cost father.State action state
            })
    
    let treeSearch strategy (problem:Problem<'s,'a>) = 
        let firstNode = 
            {
                State = problem.Init
                Father = None
                Deep = 0
                Path_Cost = 0.0
                Action = None
            }
        let bag = strategy.Init firstNode
        let rec loop bag = 
            match strategy.Next bag with
            | Some (s,bag) -> 
                if problem.F_Goal s.State
                then Some s
                else expand s problem
                    |> List.fold strategy.Add bag 
                    |> loop
            | None -> None
        loop bag
    
    let rec action node = 
        match node.Father with
        | Some father -> action father @ [Option.get node.Action]
        | None _ -> []
    
    let graphSearch projection strategy (problem:Problem<'s,'a>) = 
        let firstNode = 
            {
                State = problem.Init
                Father = None
                Deep = 0
                Path_Cost = 0.0
                Action = None
            }

        let bag = strategy.Init firstNode
        //let mutable closed = Set.empty
        let rec loop (closed,bag) = 
            match strategy.Next bag with
            | Some (s,bag) -> 
                if problem.F_Goal s.State
                then Some s
                else if Set.contains (projection s) closed 
                     then loop (closed,bag)
                     else //closed<- Set.add (projection s) closed 
                          expand s problem
                          |> List.fold strategy.Add bag 
                          |> (fun bag -> loop (Set.add (projection s) closed, bag))
            | None -> None
        loop (Set.empty,bag)
    