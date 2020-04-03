namespace Busqueda.Ejemplos

module Vacuum =
    open Busqueda
    type Place = | A | B
    type Bracket = bool * bool
    type States = Bracket * Place
    type Actions = 
        | Left
        | Right
        | Suck
        | NoOp
    
    let Init: States = (true,true), A

    let Goal = function
        | (false,false), _ -> true
        | _ -> false

    let Sussesions = function
        | (a,b), A -> [Left,((a,b),A) 
                       Right,((a,b),B)
                       NoOp,((a,b),A)
                       Suck,((false,b),A)]
        | (a,b), B -> [Left,((a,b),A) 
                       Right,((a,b),B)
                       NoOp,((a,b),B)
                       Suck,((a,false),B)]
    let Cost _ a _= 
        match a with
        | NoOp -> 0.0
        | _ -> 1.0

    let test strategy = 
        let problem = 
            {
                Init = Init
                F_Goal = Goal
                F_Sussecion = Sussesions
                F_Cost = Cost
            }  
        Busqueda.Chapter3.treeSearch strategy problem
    let problem  = 
           {
                Init = Init
                F_Goal = Goal
                F_Sussecion = Sussesions
                F_Cost = Cost
            }  