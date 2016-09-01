module Types (..) where

import Graphics.Collage


type Logic
    = Power
    | Lamp
    | Contactor Bool
    | Switcher Bool


type Switchable a
    = Static a
    | Dynamic (Bool -> a)


type Status
    = Passive
    | Active Bool


type alias Pos =
    ( Float, Float )


type alias PortInfo =
    { orientation : Int, spans : { left : Float, right : Float } }


type alias ViewInfo =
    { width : Float, height : Float, plateWidth : Int, plateHeight : Int, ports : List ( Pos, PortInfo ) }


type alias Component =
    { logic : Switchable Logic, innards : Switchable Graphics.Collage.Form, viewInfo : ViewInfo }


dummyComponent =
    Component (Static Power) (Static (Graphics.Collage.group [])) (ViewInfo 0 0 0 0 [])


type alias Tile =
    { component : Component, orientation : Int, pos : Pos, status : Status, toToggle : Maybe Int }
