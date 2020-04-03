namespace Busqueda.Ejemplos

module Balsa =
    open Busqueda

    //type state = bool * bool * bool * bool
    type place = Left | Right | BoatL | BoatR
    type obj = bool * bool * bool
    type state = obj * place

    //type actions = LEFT | RIGHT
    type actions = LEFT | RIGHT |MOVELEFT | MOVERIGHT

    let initialState_14 = (true, true, true), Left

    let sussesion state =
        match state with
            | (true, true, true), Left -> [MOVERIGHT, ((false, true, false), BoatR)
                                           MOVERIGHT, ((true, true, false), BoatR)]

            | _ -> []
            
            
            
            (*| (true, true, true, true) ->
                [(RIGHT,(true, false, true, false))] (*YA*)
            | (true, false, true, false) ->
                [(LEFT,(true, false, true, true))(*YA*)
                 (LEFT,(true, true, true, true))](*YA*)
            | (true, false, true, true) ->
                [(RIGHT,(false, false, true, false))(*YA*)
                 (RIGHT,(true, false, false, false))](*YA*)
            | (false, false, true, false) ->
                [(LEFT,(true, false, true, true))(*YA*)
                 (LEFT,(false, true, true, true))]
            | (true, false, false, false) ->
                [(LEFT,(true, true, false, true))
                 (LEFT,(true, false, true, true))](*YA*)
            //| (false, true, true, true) ->
                //[(RIGHT,())]
            | _ -> []*)


    (*
        0111 todos en lado A
        0001 zanahoria en lado A
        1111 todos en lado B
        1001 zanahoria en lado B
        0001 zanahoria en balsa
    *)
