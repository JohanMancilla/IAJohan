namespace Busqueda

module BFS = 
    open Queue

    let Strategy<'s,'a> = 
        {
            Next = Dequeue
            Add  = Enqueue
            Init = Enqueue Empty
        } : Strategy<'s,'a,Queue<Node<'s,'a>>>