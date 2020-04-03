namespace Busqueda

type Stack<'a> = list<'a>

module Stack = 
    let EmptyStack = []
    //List.append funciona igual que el operando @ 
    let Push queue state = queue @ [state]
    //let Push stack state = List.append[stack,state]
 (*
    En general es la misma estructura de funcion donde se pregunta si la cola tiene datos donde sacamos el primer elemento
    y regresamos los demas

    let Pop stack = 
        match stack with
        | x::xs -> Some(x,xs)
        | [] -> None
 *)
    let Pop = function
        | x::xs -> Some (x,xs) //Esto regresa el estado inicial o la cabeza de la cola y regresamos el resto de la lista
        | [] -> None