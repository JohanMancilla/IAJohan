namespace Busqueda

module DFSLI = 
    open DFSL
    let rec IDFS problem l =  
       match Chapter3.treeSearch (DFSL.Strategy l) problem with
        | Some n -> printf "Resultado %A" (Chapter3.action n)
        | None ->IDFS problem (l+1)

