namespace Busqueda

module DFSL = 
    open Stack

    let Strategy<'s,'a> l = 
        {
            Next = Pop
            Add  = fun stack state ->
                if state.Deep <= l
                then Push stack state
                else stack
            Init = Push EmptyStack
        } : Strategy<'s,'a,Stack<Node<'s,'a>>>