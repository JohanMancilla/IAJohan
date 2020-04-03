namespace Busqueda

module UniformCost = 
    open PriorityQueue
    let key = (fun node -> (node.Path_Cost,node.State))
    let Strategy<'s,'a when 's: comparison > = 
        {
            Next = Dequeue 
            Add  = Enqueue key
            Init = Enqueue key EmptyQueue
        } : Strategy<'s,'a,Map<float*'s,Node<'s,'a>>>