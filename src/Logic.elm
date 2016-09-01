module Logic (process) where

import Dict
import Matrix exposing (Matrix)
import Maybe exposing (withDefault)
import Set
import Types exposing (Logic(..))
import Array
import List.Extra as List
import Basics.Extra as Basics
import Ratio exposing (Rational)


inner : Logic -> Maybe ( Int, Int )
inner logic =
    case logic of
        Contactor False ->
            Nothing

        Contactor True ->
            Just ( 0, 1 )

        Switcher False ->
            Just ( 0, 2 )

        Switcher True ->
            Just ( 1, 2 )

        _ ->
            Debug.crash "IMPOSSIBLE!"


ports : Logic -> List Int
ports logic =
    case logic of
        Switcher _ ->
            [ 0, 1, 2 ]

        _ ->
            [ 0, 1 ]


process : List ( comparable, Logic ) -> List ( ( comparable, Int ), ( comparable, Int ) ) -> ( Bool, List ( comparable, Bool ) )
process tiles connections =
    let
        ( lamps, others ) =
            List.partition ((==) Lamp << snd) tiles

        ( powers, contacts ) =
            List.partition ((==) Power << snd) others

        connectedPorts =
            let
                ( ports1, ports2 ) =
                    List.unzip connections
            in
                Set.toList (Set.union (Set.fromList ports1) (Set.fromList ports2))

        connections' =
            connections ++ List.filterMap (\( id, logic ) -> Maybe.map (\( from, to ) -> ( ( id, from ), ( id, to ) )) (inner logic)) contacts

        illegal =
            isIllegal powers connections' connectedPorts
    in
        ( illegal
        , if illegal || List.isEmpty connections then
            List.map (\( l, _ ) -> ( l, False )) lamps
          else
            onLamps lamps powers connections' (List.concatMap (\( id, logic ) -> List.map ((,) id) (ports logic)) tiles)
        )


connectedIn : Matrix Bool -> Int -> Int -> Bool
connectedIn matrix i j =
    withDefault False (Matrix.get i j matrix)


isIllegal : List ( comparable, Logic ) -> List ( ( comparable, Int ), ( comparable, Int ) ) -> List ( comparable, Int ) -> Bool
isIllegal powers connections connectedPorts =
    let
        n =
            List.length connectedPorts

        indexing =
            Dict.fromList (List.indexedMap (flip (,)) connectedPorts)

        index p =
            withDefault (-1) (Dict.get p indexing)

        powerPorts =
            List.map (\( id, _ ) -> ( index ( id, 0 ), index ( id, 1 ) )) powers

        symmetrize =
            List.concatMap
                (\( from, to ) ->
                    let
                        from' =
                            index from

                        to' =
                            index to
                    in
                        [ ( from', to' ), ( to', from' ) ]
                )

        exceptLamps =
            powerPorts ++ symmetrize connections

        exceptLampsTransitive =
            warshall n (List.foldl (\( from, to ) -> Matrix.set from to True) (Matrix.repeat n n False) exceptLamps)
    in
        List.any (uncurry (connectedIn exceptLampsTransitive)) (List.map Basics.swap powerPorts)


onLamps : List ( comparable, Logic ) -> List ( comparable, Logic ) -> List ( ( comparable, Int ), ( comparable, Int ) ) -> List ( comparable, Int ) -> List ( comparable, Bool )
onLamps lamps powers connections allPorts =
    let
        pick pred list =
            case list of
                [] ->
                    Debug.crash "IMPOSSIBLE!"

                head :: tail ->
                    if pred head then
                        ( head, tail )
                    else
                        let
                            ( picked, rest ) =
                                pick pred tail
                        in
                            ( picked, head :: rest )

        merge ( x, y ) sets =
            case pick (Set.member x) sets of
                ( set, rest ) ->
                    if Set.member y set then
                        set :: rest
                    else
                        case pick (Set.member y) rest of
                            ( set', rest' ) ->
                                Set.union set set' :: rest'

        allPorts' =
            List.foldl merge (List.map Set.singleton allPorts) connections

        n =
            List.length allPorts'

        indexing =
            List.foldl (uncurry (\i -> flip (Set.foldl (flip Dict.insert i)))) Dict.empty (List.indexedMap (,) allPorts')

        index p =
            case Dict.get p indexing of
                Nothing ->
                    Debug.crash "IMPOSSIBLE!"

                Just i ->
                    i

        powerPorts =
            List.map (\( id, _ ) -> ( index ( id, 0 ), index ( id, 1 ) )) powers

        symmetrize =
            List.concatMap (\( from, to ) -> [ ( from, to ), ( to, from ) ])

        ground =
            n - 1

        matrix =
            List.foldl
                (\( from, to ) ->
                    Matrix.update from from ((+) 1)
                        >> Matrix.update from to (flip (-) 1)
                )
                (Matrix.repeat ground ground 0)
                (symmetrize (powerPorts ++ List.map (\( id, _ ) -> ( index ( id, 0 ), index ( id, 1 ) )) lamps))

        vector =
            List.foldl (\( from, to ) -> Matrix.update 0 from ((+) 1) >> Matrix.update 0 to (flip (-) 1)) (Matrix.repeat 1 ground 0) powerPorts

        solution =
            gaussJordan ground <|
                case Matrix.concatHorizontal matrix vector of
                    Nothing ->
                        Debug.crash "IMPOSSIBLE!"

                    Just m ->
                        Matrix.map Ratio.fromInt m

        potentials =
            withDefault Array.empty <|
                -- if List.any (\i -> safeGet i i solution /= Ratio.fromInt 1) [0..ground - 1] then
                --  Nothing
                -- else
                Matrix.getColumn ground solution

        potential i =
            if i == ground then
                Just (Ratio.fromInt 0)
            else
                Array.get i potentials
    in
        List.map
            (\( l, _ ) ->
                ( l
                , case ( potential (index ( l, 0 )), potential (index ( l, 1 )) ) of
                    ( Just p0, Just p1 ) ->
                        p0 /= p1

                    _ ->
                        False
                )
            )
            lamps


warshall : Int -> Matrix Bool -> Matrix Bool
warshall n matrix =
    let
        step k matrix =
            Matrix.indexedMap (\i j t -> t || connectedIn matrix i k && connectedIn matrix k j) matrix
    in
        List.foldl step matrix [0..n - 1]


gaussJordan : Int -> Matrix Rational -> Matrix Rational
gaussJordan n =
    let
        swapIfNecessary i j m =
            if i == j then
                m
            else
                List.foldl (\c -> Matrix.set c j (safeGet c i m) >> Matrix.set c i (safeGet c j m)) m [i..n]

        forward i m =
            case Matrix.getColumn i m of
                Nothing ->
                    Debug.crash "IMPOSSIBLE!"

                Just column ->
                    case List.find (\( _, v ) -> v /= Ratio.fromInt 0) (List.drop i (Array.toIndexedList column)) of
                        Nothing ->
                            m

                        Just ( j, v ) ->
                            let
                                m' =
                                    swapIfNecessary i j m
                                        |> Matrix.set i i (Ratio.fromInt 1)
                                        |> flip (List.foldl (\c -> Matrix.update c i (flip Ratio.divide v))) [i + 1..n]
                            in
                                List.foldl
                                    (\r ->
                                        let
                                            f =
                                                Ratio.negate (safeGet i r m')
                                        in
                                            Matrix.set i r (Ratio.fromInt 0)
                                                >> flip (List.foldl (\c -> Matrix.update c r (Ratio.add (Ratio.multiply f (safeGet c i m'))))) [i + 1..n]
                                    )
                                    m'
                                    [i + 1..n - 1]

        backward i m =
            if safeGet i i m == Ratio.fromInt 0 then
                m
            else
                List.foldl
                    (\r ->
                        let
                            k =
                                Ratio.negate (safeGet i r m)
                        in
                            Matrix.set i r (Ratio.fromInt 0)
                                >> flip (List.foldl (\c -> Matrix.update c r (Ratio.add (Ratio.multiply k (safeGet c i m))))) [i + 1..n]
                    )
                    m
                    [0..i - 1]
    in
        flip (List.foldl forward) [0..n - 1] >> flip (List.foldl backward) (List.reverse [1..n - 1])


safeGet i j m =
    case Matrix.get i j m of
        Nothing ->
            Debug.crash "IMPOSSIBLE!"

        Just x ->
            x
