namespace Busqueda

type PriorityQueue<'d,'c when 'd : comparison > = 
    Map<'d, 'c>

module PriorityQueue = 
    let EmptyQueue = Map.empty
    //List.append funciona igual que el operando @ 
    let Enqueue key queue state = Map.add (key state) state queue
 

    let Dequeue queue = 
        match Map.tryFindKey(fun _ _ -> true) queue with
        | Some key -> Some(Map.find key queue, Map.remove key queue)
        | None -> None
(*
        En general es la misma estructura de funcion donde se pregunta si la cola tiene datos donde sacamos el primer elemento
    y regresamos los demas
    let Dequeue = function
        | x::xs -> Some (x,xs) //Esto regresa el estado inicial o la cabeza de la cola y regresamos el resto de la lista
        | [] -> None *)