-- only needed to work around this bug: https://github.com/elm-lang/core/issues/334
module Pictures (Pictures, empty, join, move, picturesToForms, rotate, with, without) where

import Graphics.Collage exposing (Form)


type Pictures
    = Pictures (List Form) (List Form)


move : ( Float, Float ) -> Pictures -> Pictures
move by (Pictures a b) =
    Pictures (List.map (Graphics.Collage.move by) a) (List.map (Graphics.Collage.move by) b)


rotate : Float -> Pictures -> Pictures
rotate by (Pictures a b) =
    Pictures (List.map (Graphics.Collage.rotate by) a) (List.map (Graphics.Collage.rotate by) b)


empty : Pictures
empty =
    Pictures [] []


without : List Form -> Pictures -> Pictures
without a (Pictures b c) =
    Pictures (a ++ b) c


with : List Form -> Pictures -> Pictures
with a (Pictures b c) =
    Pictures b (a ++ c)


join : Pictures -> Pictures -> Pictures
join (Pictures a b) (Pictures c d) =
    Pictures (a ++ c) (b ++ d)


picturesToForms : Pictures -> List Form
picturesToForms (Pictures a b) =
    a ++ b
