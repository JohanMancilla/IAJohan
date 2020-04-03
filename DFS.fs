namespace Busqueda

module DFS = 
    open Stack

    let Strategy<'s,'a> = 
        {
            Next = Pop
            Add  = Push
            Init = Push EmptyStack
        } : Strategy<'s,'a,Stack<Node<'s,'a>>>