namespace Busqueda

type Queue<'a> = list<'a>*list<'a>

module Queue = 
    let Empty = [],[]
    //let Empty = []
    //List.append funciona igual que el operando @ 
    //let Enqueue queue state = queue @ [state]
    let Enqueue (input, output) x = (x::input, output)
 (*
    En general es la misma estructura de funcion donde se pregunta si la cola tiene datos donde sacamos el primer elemento
    y regresamos los demas
    let Dequeue queue = 
        match queue with
        | x::xs -> Some(x,xs)
        | [] -> None
 *)
    (*let Dequeue = function
        | x::xs -> Some (x,xs) //Esto regresa el estado inicial o la cabeza de la cola y regresamos el resto de la lista
        | [] -> None*)
    let rec Dequeue (input,output) = 
        match output with
        | x :: output -> Some (x, (input,output))
        | [] -> match input with
                | _ :: _ -> Dequeue ([],List.rev input)
                | _ -> None