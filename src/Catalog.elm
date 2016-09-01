module Catalog (catalog, grid, portDistance, portSize) where

import Color exposing (..)
import Graphics.Collage exposing (circle, defaultLine, filled, group, move, outlined, path, traced)
import Types exposing (Component, Logic(..), Status(..), Switchable(..), ViewInfo)


grid =
    6


portDistance =
    16


portSize =
    4


catalog =
    [ ( power, Passive, Nothing )
    , ( lamp, Active False, Nothing )
    , ( contactor, Active False, Just False )
    , ( switcher, Active False, Just True )
    ]


power =
    Component
        (Static Power)
        (Static
            (group
                [ traced { defaultLine | width = 2 } <| path [ ( 4, 0 ), ( 40, 0 ) ]
                , traced { defaultLine | width = 3 } <| path [ ( 4, -20 ), ( 4, 20 ) ]
                , traced { defaultLine | width = 2 } <| path [ ( -4, 0 ), ( -40, 0 ) ]
                , traced { defaultLine | width = 6 } <| path [ ( -4, -12 ), ( -4, 12 ) ]
                ]
            )
        )
    <|
        ViewInfo
            40
            30
            30
            30
            [ ( ( 40, 0 ), { orientation = 0, spans = { left = 51, right = 60 } } )
            , ( ( -40, 0 ), { orientation = 2, spans = { left = 51, right = 60 } } )
            ]


lamp =
    let
        d =
            20 * sqrt (1 / 2)

        offForm =
            group
                [ outlined { defaultLine | width = 2 } <| circle 20
                , traced { defaultLine | width = 2 } <| path [ ( -d, -d ), ( d, d ) ]
                , traced { defaultLine | width = 2 } <| path [ ( -d, d ), ( d, -d ) ]
                , traced { defaultLine | width = 2 } <| path [ ( 20, 0 ), ( 51, 0 ) ]
                , traced { defaultLine | width = 2 } <| path [ ( -20, 0 ), ( -51, 0 ) ]
                ]

        onForm =
            group
                [ filled yellow <| circle 20
                , offForm
                ]
    in
        Component
            (Static Lamp)
            (Dynamic
                (\b ->
                    if b then
                        onForm
                    else
                        offForm
                )
            )
        <|
            ViewInfo
                51
                31
                41
                31
                [ ( ( 51, 0 ), { orientation = 0, spans = { left = 51, right = 60 } } )
                , ( ( -51, 0 ), { orientation = 2, spans = { left = 51, right = 60 } } )
                ]


contactor =
    let
        baseForm =
            group
                [ traced { defaultLine | width = 2 } <| path [ ( 20, -6 ), ( 49, -6 ) ]
                , move ( 20, -6 ) <| filled black (circle 4)
                , traced { defaultLine | width = 2 } <| path [ ( -20, -6 ), ( -49, -6 ) ]
                , move ( -20, -6 ) <| filled black (circle 4)
                ]

        offForm =
            group
                [ traced { defaultLine | width = 2 } <| path [ ( -20, -6 ), ( -20 + 40 * sqrt (3 / 4), 14 ) ]
                , baseForm
                ]

        onForm =
            group
                [ traced { defaultLine | width = 2 } <| path [ ( -20, -6 ), ( 20, -6 ) ]
                , baseForm
                ]
    in
        Component
            (Dynamic Contactor)
            (Dynamic
                (\b ->
                    if b then
                        onForm
                    else
                        offForm
                )
            )
        <|
            ViewInfo
                49
                29
                39
                29
                [ ( ( 49, -6 ), { orientation = 0, spans = { left = 54, right = 51 } } )
                , ( ( -49, -6 ), { orientation = 2, spans = { left = 42, right = 63 } } )
                ]


switcher =
    let
        baseForm =
            group
                [ traced { defaultLine | width = 2 } <| path [ ( 18, 20 ), ( 18, 46 ) ]
                , move ( 18, 20 ) <| filled black (circle 4)
                , traced { defaultLine | width = 2 } <| path [ ( 18, -20 ), ( 18, -46 ) ]
                , move ( 18, -20 ) <| filled black (circle 4)
                , traced { defaultLine | width = 2 } <| path [ ( -20, 0 ), ( -46, 0 ) ]
                , move ( -20, 0 ) <| filled black (circle 4)
                ]

        offForm =
            group
                [ traced { defaultLine | width = 2 } <| path [ ( -20, 0 ), ( 18, 20 ) ]
                , baseForm
                ]

        onForm =
            group
                [ traced { defaultLine | width = 2 } <| path [ ( -20, 0 ), ( 18, -20 ) ]
                , baseForm
                ]
    in
        Component
            (Dynamic Switcher)
            (Dynamic
                (\b ->
                    if b then
                        onForm
                    else
                        offForm
                )
            )
        <|
            ViewInfo
                46
                46
                36
                36
                [ ( ( 18, 46 ), { orientation = 1, spans = { left = 75, right = 48 } } )
                , ( ( 18, -46 ), { orientation = 3, spans = { left = 39, right = 84 } } )
                , ( ( -46, 0 ), { orientation = 2, spans = { left = 57, right = 66 } } )
                ]
