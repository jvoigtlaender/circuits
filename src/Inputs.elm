module Inputs (create, hover, toggling) where

import Automaton exposing (Automaton)
import Char exposing (KeyCode)
import Keyboard
import Maybe.Extra as Maybe exposing (mapDefault)
import Set exposing (Set)
import Types exposing (Component, Pos, Status(..))


create : Signal.Mailbox ( Component, Status, Pos, Maybe Bool )
create =
    Signal.mailbox ( Types.dummyComponent, Passive, ( 0, 0 ), Nothing )


hover : Signal.Mailbox (Maybe Int)
hover =
    Signal.mailbox Nothing


keyAutomaton : Automaton (Set KeyCode) (Maybe ( KeyCode, Bool ))
keyAutomaton =
    let
        step newSet oldSet =
            flip (,) newSet <|
                case Set.toList (Set.diff newSet oldSet) of
                    k :: [] ->
                        Just ( k, True )

                    [] ->
                        case Set.toList (Set.diff oldSet newSet) of
                            k :: [] ->
                                Just ( k, False )

                            _ ->
                                Nothing

                    _ ->
                        Debug.crash "IMPOSSIBLE!"
    in
        Automaton.hiddenState Set.empty step


toggling : Signal ( Int, Bool )
toggling =
    Signal.filterMap
        (mapDefault
            Nothing
            (\( i, down ) ->
                if 47 < i && i < 58 then
                    Just ( i - 48, down )
                else if 64 < i && i < 91 then
                    Just ( i - 55, down )
                else
                    Nothing
            )
        )
        ( 0, False )
        (Automaton.run keyAutomaton Nothing Keyboard.keysDown)
