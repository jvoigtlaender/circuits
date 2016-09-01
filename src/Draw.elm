module Draw (makeGround, tile) where

import Catalog exposing (catalog)
import Color exposing (..)
import Graphics.Collage exposing (circle, collage, defaultLine, filled, move, outlined, path, rect, text, traced)
import Graphics.Input exposing (clickable, hoverable)
import Inputs exposing (create, hover)
import Maybe exposing (withDefault)
import Pictures exposing (Pictures)
import String
import Text
import Types exposing (Pos, Tile)
import Util exposing (applyStatus, plus, rotate)


drawPlate x y =
    collage
        (2 * x)
        (2 * y)
        [ outlined { defaultLine | width = 4 } (rect (2 * x) (2 * y)) ]


drawPort =
    filled black (circle Catalog.portSize)


drawEncircling =
    outlined { defaultLine | width = 2 } (circle (Catalog.portSize + 4))


makeGround xExtent yExtent =
    let
        pad =
            20

        xOff =
            pad + withDefault 0 (List.maximum (List.map (.width << .viewInfo << (\( a, _, _ ) -> a)) catalog))

        xRight =
            xExtent - 2 * xOff
    in
        ( [ move ( -xOff, 0 ) <| filled red (rect (xExtent + xRight) (2 * yExtent)) ]
        , Pictures.without
            [ outlined { defaultLine | width = 4 } <|
                rect (2 * xExtent) (2 * yExtent)
            , traced { defaultLine | width = 2 } <|
                path
                    [ ( xRight, -yExtent )
                    , ( xRight, yExtent )
                    ]
            ]
          <|
            (fst <|
                List.foldl
                    (\( component, status, toggleable ) ( pictures, oldY ) ->
                        let
                            { height, plateWidth, plateHeight, ports } =
                                component.viewInfo

                            plateForm =
                                Graphics.Collage.toForm <|
                                    clickable (Signal.message create.address ( component, status, ( xRight - xOff, oldY - height ), toggleable )) <|
                                        drawPlate plateWidth plateHeight

                            innerForm =
                                applyStatus component.innards status
                        in
                            ( Pictures.join pictures <|
                                Pictures.move ( xExtent - xOff, oldY - height ) <|
                                    Pictures.without (List.map (flip move drawPort << fst) ports) <|
                                        Pictures.without [ innerForm ] <|
                                            Pictures.with [ plateForm ] <|
                                                Pictures.empty
                            , oldY - 2 * height - pad
                            )
                    )
                    ( Pictures.empty, yExtent - pad )
                    catalog
            )
        )


tile : Signal.Address Int -> Signal.Address ( Int, Int ) -> Pos -> ( Int, Tile ) -> Pictures
tile turnAddress connectAddress ( mx, my ) ( id, { component, orientation, pos, status, toToggle } ) =
    let
        { plateWidth, plateHeight, ports } =
            component.viewInfo

        plateForm =
            Graphics.Collage.toForm <|
                hoverable
                    (Signal.message hover.address
                        << \h ->
                            if h then
                                Just id
                            else
                                Nothing
                    )
                <|
                    clickable (Signal.message turnAddress id) <|
                        drawPlate plateWidth plateHeight

        innerForm =
            applyStatus component.innards status

        portForm id =
            Graphics.Collage.toForm <|
                clickable (Signal.message connectAddress id) <|
                    collage (2 * Catalog.portSize) (2 * Catalog.portSize) [ drawPort ]

        portsToDraw =
            List.map (plus pos << rotate orientation << fst) ports

        nearMouse ( x, y ) =
            abs (mx - x) < Catalog.portSize && abs (my - y) < Catalog.portSize
    in
        Pictures.without
            (List.filterMap
                (\pos ->
                    if nearMouse pos then
                        Just (move pos drawEncircling)
                    else
                        Nothing
                )
                portsToDraw
            )
        <|
            Pictures.with (List.indexedMap (flip move << portForm << (,) id) portsToDraw) <|
                Pictures.move pos <|
                    (case toToggle of
                        Nothing ->
                            identity

                        Just i ->
                            let
                                ( cx, cy ) =
                                    if orientation % 2 == 0 then
                                        ( -plateWidth, plateHeight )
                                    else
                                        ( -plateHeight, plateWidth )
                            in
                                Pictures.without
                                    [ move ( toFloat cx + 7, toFloat cy - 7 ) <|
                                        text <|
                                            Text.fromString <|
                                                String.slice i (i + 1) "0123456789abcdefghijklmnopqrstuvwxyz"
                                    ]
                    )
                    <|
                        Pictures.rotate (toFloat orientation * pi / 2) <|
                            Pictures.without [ innerForm ] <|
                                Pictures.with [ plateForm ] <|
                                    Pictures.empty
