namespace Busqueda.Ejemplos

module HPuzzle = 
    open Busqueda
    
    type state = int * int * int * int * int * int * int * int * int

    type actions = UP | LEFT | RIGHT | DOWN 

    let init = (1, 2, 3, 4, 0, 6, 7, 5, 8)
    let initialState_14 = (0, 3, 6, 2, 1, 7, 5, 4, 8)
    let initialState_16 = (1, 3, 0, 7, 6, 4, 8, 5, 2)
    let initialState_18 = (0, 8, 5, 4, 2, 1, 7, 6, 3)
    let initialState_20 = (1, 3, 0, 7, 2, 5, 4, 6, 8)
    let initialState_23 = (5, 2, 4, 1, 3, 0, 6, 8, 7)

    let sussesion state = 
      match state with
        | (0, x2, x3, x4, x5, x6, x7, x8, x9) -> 
            [(RIGHT, (x2, 0, x3, x4, x5, x6, x7, x8, x9))
             (DOWN, (x4, x2, x3, 0, x5, x6, x7, x8, x9))]
        | (x1, 0, x3, x4, x5, x6, x7, x8, x9) -> 
            [(LEFT, (0, x1, x3, x4, x5, x6, x7, x8, x9))
             (RIGHT, (x1, x3, 0, x4, x5, x6, x7, x8, x9))
             (DOWN, (x1, x5, x3, x4, 0, x6, x7, x8, x9))]
        | (x1, x2, 0, x4, x5, x6, x7, x8, x9) -> 
            [(LEFT, (x1, 0, x2, x4, x5, x6, x7, x8, x9))
             (DOWN, (x1, x2, x6, x4, x5, 0, x7, x8, x9))]
        | (x1, x2, x3, 0, x5, x6, x7, x8, x9) -> 
            [(UP, (0, x2, x3, x1, x5, x6, x7, x8, x9))
             (RIGHT, (x1, x2, x3, x5, 0, x6, x7, x8, x9))
             (DOWN, (x1, x2, x3, x7, x5, x6, 0, x8, x9))]
        | (x1, x2, x3, x4, 0, x6, x7, x8, x9) -> 
            [(UP, (x1, 0, x3, x4, x2, x6, x7, x8, x9))
             (LEFT, (x1, x2, x3, 0, x4, x6, x7, x8, x9))
             (RIGHT, (x1, x2, x3, x4, x6, 0, x7, x8, x9))
             (DOWN, (x1, x2, x3, x4, x8, x6, x7, 0, x9))]
        | (x1, x2, x3, x4, x5, 0, x7, x8, x9) -> 
            [(UP, (x1, x2, 0, x4, x5, x3, x7, x8, x9))
             (LEFT, (x1, x2, x3, x4, 0, x5, x7, x8, x9))
             (DOWN, (x1, x2, x3, x4, x5, x9, x7, x8, 0))]
        | (x1, x2, x3, x4, x5, x6, 0, x8, x9) -> 
            [(UP, (x1, x2, x3, 0, x5, x6, x4, x8, x9))
             (RIGHT, (x1, x2, x3, x4, x5, x6, x8, 0, x9))]
        | (x1, x2, x3, x4, x5, x6, x7, 0, x9) -> 
            [(LEFT, (x1, x2, x3, x4, x5, x6, 0, x7, x9))
             (UP, (x1, x2, x3, x4, 0, x6, x7, x5, x9))
             (RIGHT, (x1, x2, x3, x4, x5, x6, x7, x9, 0))]
        | (x1, x2, x3, x4, x5, x6, x7, x8, 0) -> 
            [(LEFT, (x1, x2, x3, x4, x5, x6, x7, 0, x8))
             (UP, (x1, x2, x3, x4, x5, 0, x7, x8, x6))]
        | _ -> []
    let invert = function | LEFT -> RIGHT
                          | RIGHT -> LEFT
                          | UP -> DOWN 
                          | DOWN -> UP
    let goal x = x = (1,2,3,4,5,6,7,8,0)

    let cost _ _ _ = 1.0

    let stateToList (x1,x2,x3,x4,x5,x6,x7,x8,x9) =
        [x1;x2;x3;x4;x5;x6;x7;x8;x9]
    
    let heuristicFunction1 goal state = 
        let diferent y x = if x <> y && x <> 0
                           then 1
                           else 0
        List.fold2 (fun suma x y -> diferent x y ) 0
                    (stateToList goal)
                    (stateToList state)
    
    let heuristicFunction2 (m1, m2, m3, m4, m5, m6, m7, m8, m9) (x1, x2, x3, x4, x5, x6, x7, x8, x9) =
        let cor i = (i % 3, i / 3)
        let index x = 
            if x = m1 then 0
            else if x = m2 then 1
            else if x = m3 then 2
            else if x = m4 then 3
            else if x = m5 then 4
            else if x = m6 then 5
            else if x = m7 then 6
            else if x = m8 then 7
            else 8
        let dis i x = 
            if x = 0 then 0
            else let (x1, x2) = cor i
                 let (y1, y2) = cor (index x)
                 abs (x1 - y1) + abs (x2 - y2)
        dis 0 x1 + 
        dis 1 x2 + 
        dis 2 x3 + 
        dis 3 x4 + 
        dis 4 x5 + 
        dis 5 x6 + 
        dis 6 x7 + 
        dis 7 x8 + 
        dis 8 x9
    let problem init = 
        {
            Init = init
            F_Goal = goal
            F_Sussecion = sussesion
            F_Cost = cost
        }
    let test algorithm init =
        algorithm (problem init)