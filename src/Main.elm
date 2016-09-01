module Main (main) where

import Array exposing (Array)
import Dict exposing (Dict)
import Drag
import Draw
import Effects
import Graphics.Collage exposing (collage, defaultLine)
import Html exposing (Html)
import Inputs exposing (create, hover)
import Keyboard
import List.Extra as List exposing (getAt)
import Logic
import Maybe.Extra as Maybe exposing (mapDefault)
import Mouse
import Pictures
import StartApp exposing (start)
import Time
import Types exposing (Component, Pos, Status(..), Tile)
import Util exposing (applyStatus, isConnected, plus, rotate, snap, switchStatus)
import Wiring


port xExtent : Int


port yExtent : Int


noEffects =
    flip (,) Effects.none


type alias Model =
    { nextTile : Int
    , tiles : Dict Int Tile
    , toggles : Array ( Int, Bool )
    , connections : List ( ( Int, Int ), ( Int, Int ) )
    , turnBlocked : Bool
    , picked : Maybe Int
    , inConnect : Maybe ( Int, Int )
    , mouse : Pos
    , illegal : Bool
    }


init =
    noEffects <|
        { nextTile = 0
        , tiles = Dict.empty
        , toggles = Array.empty
        , connections = []
        , turnBlocked = False
        , picked = Nothing
        , inConnect = Nothing
        , mouse = ( 0, 0 )
        , illegal = False
        }


type Action
    = DnD (Maybe ( Int, Drag.Action ))
    | Create ( Component, Status, Pos, Maybe Bool )
    | Turn Int
    | Connect ( Int, Int )
    | Mouse Pos
    | Delete
    | Toggle ( Int, Bool )


( redFlag, arena ) =
    Draw.makeGround (toFloat xExtent) (toFloat yExtent)


view : Signal.Address Action -> Model -> Html
view address { tiles, connections, inConnect, mouse, illegal } =
    let
        portInfo ( tid, pid ) =
            case Dict.get tid tiles of
                Nothing ->
                    Debug.crash "IMPOSSIBLE!"

                Just { component, orientation, pos } ->
                    let
                        { ports } =
                            component.viewInfo
                    in
                        case Maybe.map (\( pos, info ) -> ( rotate orientation pos, { info | orientation = (info.orientation + orientation) % 4 } )) (getAt ports pid) of
                            Nothing ->
                                Debug.crash "IMPOSSIBLE!"

                            Just ( diff, info ) ->
                                ( pos `plus` diff, info )

        average ps =
            let
                ( xs, ys ) =
                    List.unzip ps

                n =
                    toFloat (List.length ps)
            in
                ( List.sum xs / n, List.sum ys / n )

        center =
            average
                (List.filterMap
                    (\( id, { pos } ) ->
                        if isConnected id connections then
                            Just pos
                        else
                            Nothing
                    )
                    (Dict.toList tiles)
                )
    in
        Html.fromElement <|
            collage (2 * xExtent) (2 * yExtent) <|
                (if illegal then
                    (++) redFlag
                 else
                    identity
                )
                <|
                    (case inConnect of
                        Nothing ->
                            identity

                        Just from ->
                            (::)
                                (Graphics.Collage.traced { defaultLine | width = 2 } <|
                                    Graphics.Collage.path <|
                                        Wiring.fromTo (portInfo from) ( mouse, Nothing ) center
                                )
                    )
                    <|
                        (++)
                            (List.map
                                (\( from, to ) ->
                                    Graphics.Collage.traced { defaultLine | width = 2 } <|
                                        Graphics.Collage.path <|
                                            Wiring.fromTo
                                                (portInfo from)
                                                (case portInfo to of
                                                    ( pos, info ) ->
                                                        ( pos, Just info )
                                                )
                                                center
                                )
                                connections
                            )
                        <|
                            Pictures.picturesToForms <|
                                List.foldr Pictures.join arena <|
                                    List.map (Draw.tile (Signal.forwardTo address Turn) (Signal.forwardTo address Connect) mouse) (Dict.toList tiles)


update action ({ nextTile, tiles, toggles, connections, turnBlocked, picked, inConnect } as model) =
    noEffects <|
        case action of
            DnD Nothing ->
                model

            DnD (Just ( id, Drag.Lift )) ->
                { model
                    | turnBlocked = False
                    , picked = Just id
                }

            DnD (Just ( id, Drag.MoveBy ( dx, dy ) )) ->
                { model
                    | turnBlocked = True
                    , tiles = Dict.update id (Maybe.map (\tile -> { tile | pos = tile.pos `plus` ( toFloat dx, toFloat (-dy) ) })) tiles
                }

            DnD (Just ( id, Drag.Release )) ->
                { model
                    | picked = Nothing
                    , tiles =
                        Dict.update
                            id
                            (Maybe.map
                                (\tile ->
                                    { tile
                                        | pos =
                                            case tile.pos of
                                                ( x, y ) ->
                                                    ( snap x, snap y )
                                    }
                                )
                            )
                            tiles
                }

            Create ( component, status, pos, toggleable ) ->
                case toggleable of
                    Just mode ->
                        { model
                            | nextTile = nextTile + 1
                            , tiles = Dict.insert nextTile (Tile component 0 pos status (Just (Array.length toggles))) tiles
                            , toggles = Array.push ( nextTile, mode ) toggles
                        }

                    Nothing ->
                        { model
                            | nextTile = nextTile + 1
                            , tiles = Dict.insert nextTile (Tile component 0 pos status Nothing) tiles
                        }

            Turn id ->
                if turnBlocked then
                    model
                else
                    { model | tiles = Dict.update id (Maybe.map (\tile -> { tile | orientation = (tile.orientation + 1) % 4 })) tiles }

            Connect id ->
                case inConnect of
                    Nothing ->
                        { model | inConnect = Just id }

                    Just from ->
                        if from == id then
                            { model | inConnect = Nothing }
                        else
                            { model
                                | inConnect = Nothing
                                , connections = ( from, id ) :: connections
                            }
                                |> updateLogic

            Mouse pos ->
                { model | mouse = pos }

            Delete ->
                case picked of
                    Nothing ->
                        { model | inConnect = Nothing }

                    Just id ->
                        if isConnected id connections then
                            { model
                                | turnBlocked = True
                                , connections = List.filter (\( ( from, _ ), ( to, _ ) ) -> from /= id && to /= id) connections
                            }
                                |> updateLogic
                        else
                            { model
                                | picked = Nothing
                                , tiles = Dict.remove id tiles
                                , inConnect =
                                    case inConnect of
                                        Nothing ->
                                            Nothing

                                        Just ( from, _ ) ->
                                            if from == id then
                                                Nothing
                                            else
                                                inConnect
                            }

            Toggle ( id, down ) ->
                case Array.get id toggles of
                    Nothing ->
                        model

                    Just ( id, mode ) ->
                        if down && mode || not mode && mapDefault False (\{ status } -> status /= Active down) (Dict.get id tiles) then
                            { model | tiles = Dict.update id (Maybe.map (\tile -> { tile | status = switchStatus tile.status })) tiles }
                                |> updateLogic
                        else
                            model


updateLogic ({ tiles, connections } as model) =
    let
        tileStates =
            List.map (\( id, { component, status } ) -> ( id, applyStatus component.logic status )) (Dict.toList tiles)

        ( illegal, toActivate ) =
            Logic.process tileStates connections
    in
        { model
            | tiles = List.foldl (\( id, b ) -> Dict.update id (Maybe.map (\tile -> { tile | status = Active b }))) tiles toActivate
            , illegal = illegal
        }


app =
    start
        { init = init
        , view = view
        , update = update
        , inputs =
            [ Signal.map DnD (Drag.trackMany Nothing hover.signal)
            , Signal.map Create create.signal
            , Signal.map
                (\( x, y ) -> Mouse ( toFloat (x - xExtent), toFloat (yExtent - y) ))
                (Time.delay
                    -- only needed because of a bug in https://github.com/evancz/start-app
                    0
                    Mouse.position
                )
            , Signal.map (always Delete) (Signal.filter (flip List.member [ 46, 120 ]) 0 Keyboard.presses)
              -- deleting only works in Firefox, not in Chrome or IE
            , Signal.map Toggle Inputs.toggling
            ]
        }


main =
    app.html
