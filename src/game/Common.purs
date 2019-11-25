module Game.Common where
import MyPrelude
import Pha.Events.Decoder (Decoder, readNumber, readProp, currentTarget, getBoundingClientRect)

-- fonction utile pour labete et tiling
-- isomorphisme entre 2 façons de réprésenter une bête/tuile
--     (par coordonnées {row, col} ou par tableaau de booleéen partnt de (-2, -2), jusqu'à (2, 2)
_isoCustom :: Iso' (Array {row :: Int, col :: Int}) (Array Boolean)      
_isoCustom = iso from to where
        from = flip updateAtIndices (replicate 25 false) ∘  map \{row, col} -> (row * 5 + col + 12) ∧ true
        to = catMaybes ∘ mapWithIndex \i -> if _ then Just {row: i / 5 - 2, col: i `mod` 5 - 2} else Nothing

pointerDecoder :: Decoder { x :: Number, y :: Number }
pointerDecoder f = do
    {left, top, width, height} <- f # currentTarget >>= getBoundingClientRect
    x <- f # readProp "clientX" >>= readNumber
    y <- f # readProp "clientY" >>= readNumber
    pure {
        x: (x - left) / width,
        y: (y - top) / height
    }