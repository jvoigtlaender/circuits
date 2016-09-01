module Util (..) where

import Catalog
import Types exposing (Status(..), Switchable(..))


minus ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


plus ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


rotate n ( x, y ) =
    case n of
        1 ->
            ( -y, x )

        2 ->
            ( -x, -y )

        3 ->
            ( y, -x )

        _ ->
            ( x, y )


snap z =
    toFloat (round (z / Catalog.grid) * Catalog.grid)


isConnected id =
    List.any (\( ( from, _ ), ( to, _ ) ) -> from == id || to == id)


switchStatus status =
    case status of
        Active b ->
            Active (not b)

        Passive ->
            Debug.crash "IMPOSSIBLE!"


applyStatus switchable status =
    case ( switchable, status ) of
        ( Static static, _ ) ->
            static

        ( Dynamic dynamic, Active b ) ->
            dynamic b

        _ ->
            Debug.crash "IMPOSSIBLE!"
