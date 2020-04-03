namespace Busqueda

module Greedy =
    let Strategy<'s,'a when 's : comparison> h = 
        {
            Next = fun m -> 
                         match Map.tryFindKey (fun _ _ -> true) m with 
                         | Some k -> Some (Map.find k m , Map.remove k m)
                         | None -> None
            Add = fun m n -> Map.add (h n, n.State) n m
            Init = fun n -> Map.add(h n, n.State)n Map.empty     
        }:Strategy<'s,'a,Map<float * 's,Node<'s,'a>>>
    let id n = n.State