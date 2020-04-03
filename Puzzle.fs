namespace Busqueda.Ejemplos

module Puzzle =
    open Busqueda
    type States = int * int * int* int * int* int * int* int * int
    type Actions = 
        | Left
        | Right
        | Up
        | Down
    
    let Init: States = (1,2,3,
                        4,5,6,
                        7,0,8)
    let Goal state = 
        state = (1,2,3,
                 4,5,6,
                 7,8,0) 
    let Cost _ _ _  =  1.0
    let Sussesions = function
        | (0,x2,x3,x4,x5,x6,x7,x8,x9) -> [Right, (x2,0,x3,x4,x5,x6,x7,x8,x9);
                                          Down, (x4,x2,x3,0,x5,x6,x7,x8,x9)]
        | (x1,0,x3,x4,x5,x6,x7,x8,x9) ->[Left, (0,x1,x3,x4,x5,x6,x7,x8,x9);
                                         Right, (x1,x3,0,x4,x5,x6,x7,x8,x9);
                                         Down, (x1,x5,x3,x4,0,x6,x7,x8,x9)]
        | (x1,x2,0,x4,x5,x6,x7,x8,x9) ->[Left, (x1,0,x2,x4,x5,x6,x7,x8,x9);
                                         Down, (x1,x2,x6,x4,x5,0,x7,x8,x9)]
        | (x1,x2,x3,0,x5,x6,x7,x8,x9) ->[Right, (x1,x2,x3,x5,0,x6,x7,x8,x9);
                                         Up, (0,x2,x3,x1,x5,x6,x7,x8,x9);
                                         Down, (x1,x2,x3,x7,x5,x6,0,x8,x9)]
        | (x1,x2,x3,x4,0,x6,x7,x8,x9) ->[Right, (x1,x2,x3,x4,x6,0,x7,x8,x9);
                                         Left, (x1,x2,x3,0,x4,x6,x7,x8,x9);
                                         Up, (x1,0,x3,x4,x2,x6,x7,x8,x9);
                                         Down, (x1,x2,x3,x4,x8,x6,x7,0,x9)]
        | (x1,x2,x3,x4,x5,0,x7,x8,x9) ->[Left, (x1,x2,x3,x4,0,x5,x7,x8,x9);
                                         Up, (x1,x2,0,x4,x5,x3,x7,x8,x9);
                                         Down, (x1,x2,x3,x4,x5,x9,x7,x8,0)]
        | (x1,x2,x3,x4,x5,x6,0,x8,x9) ->[Right, (x1,x2,x3,x4,x5,x6,x8,0,x9);
                                         Up, (x1,x2,x3,0,x5,x6,x4,x8,x9)]
        | (x1,x2,x3,x4,x5,x6,x7,0,x9) ->[Right, (x1,x2,x3,x4,x5,x6,x7,x9,0);
                                         Left, (x1,x2,x3,x4,x5,x6,0,x7,x9);
                                         Up, (x1,x2,x3,x4,0,x6,x7,x5,x9)]
        | (x1,x2,x3,x4,x5,x6,x7,x8,0) ->[Left, (x1,x2,x3,x4,x5,x6,x7,0,x8);
                                         Up, (x1,x2,x3,x4,x5,0,x7,x8,x6)]
        | _ ->    failwith "ERROR"
    let test strategy = 
        let problem = 
            {
                Init = Init
                F_Goal = Goal
                F_Sussecion = Sussesions
                F_Cost = Cost
            }  
        //Busqueda.Chapter3.treeSearch strategy problem
        Busqueda.Chapter3.graphSearch (fun n -> n.State) strategy problem
    let problem  = 
           {
                Init = Init
                F_Goal = Goal
                F_Sussecion = Sussesions
                F_Cost = Cost
            }  