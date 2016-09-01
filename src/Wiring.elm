module Wiring (fromTo) where

import Catalog exposing (portDistance)
import List exposing (length, reverse, sum)
import List.Extra as List exposing (maximumBy, zip)
import Maybe exposing (withDefault)
import Types exposing (PortInfo, Pos)
import Util exposing (minus, plus, rotate)


swap { left, right } =
    { left = right, right = left }


fromTo : ( Pos, PortInfo ) -> ( Pos, Maybe PortInfo ) -> Pos -> List Pos
fromTo from to ( cx, cy ) =
    let
        pairs ps =
            zip ps (withDefault [] (List.tail ps))

        removeDups ps =
            case List.head ps of
                Nothing ->
                    []

                Just p ->
                    p
                        :: List.filterMap
                            (\( p, p' ) ->
                                if p /= p' then
                                    Just p'
                                else
                                    Nothing
                            )
                            (pairs ps)

        weight ps =
            let
                l =
                    length ps
            in
                sum
                    (List.map
                        (\( x, y ) ->
                            let
                                dx =
                                    x - cx

                                dy =
                                    y - cy
                            in
                                dx * dx + dy * dy
                        )
                        ps
                    )
                    / toFloat (l * l)

        -- known that either x1 == x2 or y1 == y2
        delta ( ( x1, y1 ), ( x2, y2 ) ) =
            abs (x1 - x2 + y1 - y2)

        goodness ps =
            let
                w =
                    weight ps
            in
                if List.any ((>) portDistance << delta) (pairs ps) then
                    0.5 * w
                else
                    w
    in
        (::) (fst from) <|
            flip (++) [ fst to ] <|
                withDefault [] (maximumBy goodness (List.map removeDups (wiringChoices from to)))


wiringChoices : ( Pos, PortInfo ) -> ( Pos, Maybe PortInfo ) -> List (List Pos)
wiringChoices ( from, { orientation, spans } ) ( to, info ) =
    let
        orientation' =
            4 - orientation

        from' =
            rotate orientation' from

        to' =
            rotate orientation' to

        info' =
            Maybe.map (\info -> { info | orientation = (info.orientation + orientation') % 4 }) info
    in
        List.map (List.map (rotate orientation << plus from')) (normalizedWiring spans (to' `minus` from') info')


normalizedWiring : { left : Float, right : Float } -> Pos -> Maybe PortInfo -> List (List Pos)
normalizedWiring spans1 (( x, y ) as to) info2 =
    case info2 of
        Nothing ->
            normalizedWiring' spans1 to info2

        Just { orientation, spans } ->
            if orientation == 0 && x > 0 then
                List.map (reverse << List.map (plus to)) (normalizedWiring' spans ( -x, -y ) (Just (PortInfo 0 spans1)))
            else
                normalizedWiring' spans1 to info2


normalizedWiring' : { left : Float, right : Float } -> Pos -> Maybe PortInfo -> List (List Pos)
normalizedWiring' spans1 (( x, y ) as to) info2 =
    if y < 0 then
        List.map (List.map (\( x, y ) -> ( x, -y ))) (normalizedWiring'' (swap spans1) ( x, -y ) (Maybe.map (\{ orientation, spans } -> PortInfo (4 - orientation) (swap spans)) info2))
    else
        normalizedWiring'' spans1 to info2


normalizedWiring'' : { left : Float, right : Float } -> Pos -> Maybe PortInfo -> List (List Pos)
normalizedWiring'' ({ left, right } as spans1) ( x, y ) info2 =
    case Maybe.map (\{ orientation, spans } -> ( orientation, spans )) info2 of
        Nothing ->
            if x > portDistance then
                [ [ ( portDistance, 0 ), ( portDistance, y ) ]
                , [ ( x, 0 ) ]
                ]
            else if x > 0 then
                [ [ ( x, 0 ) ] ]
            else if y > left then
                [ [ ( portDistance, 0 ), ( portDistance, left ), ( x, left ) ]
                , [ ( portDistance, 0 ), ( portDistance, y ) ]
                ]
            else if x < -portDistance then
                [ [ ( portDistance, 0 ), ( portDistance, left ), ( x, left ) ] ]
            else
                [ [ ( 0, y ) ] ]

        Just ( 3, _ ) ->
            let
                y' =
                    y - portDistance
            in
                if y' > left || x > portDistance && y' > 0 then
                    normalizedWiring'' spans1 ( x, y' ) Nothing |> List.map (flip (++) [ ( x, y' ) ])
                else if x > 0 then
                    [ [ ( x, 0 ) ] ]
                else if x < -portDistance then
                    [ [ ( portDistance, 0 ), ( portDistance, -right ), ( x, -right ) ] ]
                else
                    [ [ ( 0, y ) ] ]

        Just ( 1, spans2 ) ->
            if x > 0 then
                List.map
                    (reverse << List.map (\( x', y' ) -> ( x - y', x' + y )))
                    (normalizedWiring'' spans2 ( -y, x ) (Just (PortInfo 3 spans1)))
            else
                let
                    y' =
                        y + portDistance

                    x' =
                        x + spans2.right
                in
                    if x' > portDistance then
                        [ [ ( x', 0 ), ( x', y' ), ( x, y' ) ] ]
                    else
                        let
                            y'' =
                                max y' left
                        in
                            [ [ ( portDistance, 0 ), ( portDistance, y'' ), ( x, y'' ) ]
                            , [ ( portDistance, 0 ), ( portDistance, left ), ( x', left ), ( x', y'' ), ( x, y'' ) ]
                            ]

        Just ( 2, spans2 ) ->
            let
                x' =
                    x - portDistance
            in
                if x' > portDistance then
                    [ [ ( portDistance, 0 ), ( portDistance, y ) ]
                    , [ ( x', 0 ), ( x', y ) ]
                    ]
                else if x > 0 then
                    let
                        x'' =
                            x / 2
                    in
                        [ [ ( x'', 0 ), ( x'', y ) ] ]
                else
                    let
                        y' =
                            y - spans2.left

                        ( y1, y2 ) =
                            if y' > left then
                                ( left, y' )
                            else
                                ( min (-right) y', max left (y + spans2.right) )
                    in
                        [ [ ( portDistance, 0 ), ( portDistance, y1 ), ( x', y1 ), ( x', y ) ]
                        , [ ( portDistance, 0 ), ( portDistance, y2 ), ( x', y2 ), ( x', y ) ]
                        ]

        Just ( _, _ ) ->
            let
                x' =
                    x + portDistance
            in
                if x' > 0 || y > left then
                    [ [ ( portDistance, 0 ), ( portDistance, y ) ] ]
                else
                    [ [ ( portDistance, 0 ), ( portDistance, left ), ( x', left ), ( x', y ) ]
                    , [ ( portDistance, 0 ), ( portDistance, -right ), ( x', -right ), ( x', y ) ]
                    ]
