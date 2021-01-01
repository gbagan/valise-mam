module Game.Common where
import MyPrelude
import Pha.Html (EventHandler)
import Web.Event.Event as E
import Web.HTML.HTMLElement as HE
import Web.UIEvent.MouseEvent as ME
import Unsafe.Coerce (unsafeCoerce)

-- fonction utile pour labete et tiling
-- isomorphisme entre 2 façons de réprésenter une bête/tuile
--     (par coordonnées {row, col} ou par tableaau de booleéen partant de (-2, -2), jusqu'à (2, 2)
_isoCustom ∷ Iso' (Array {row ∷ Int, col ∷ Int}) (Array Boolean)      
_isoCustom = iso from to where
        from = flip updateAtIndices (replicate 25 false) ∘  map \{row, col} → (row * 5 + col + 12) ∧ true
        to = catMaybes ∘ mapWithIndex \i → if _ then Just {row: i / 5 - 2, col: i `mod` 5 - 2} else Nothing

pointerDecoder ∷ EventHandler { x ∷ Number, y ∷ Number }
pointerDecoder ev = do
    case ME.fromEvent ev ∧ E.currentTarget ev of
        Just mouseEv ∧ Just el → do
            -- dans l'implémentation actuelle en purescript, getBoundingClientRect ne s'applique
            -- qu'à des HTMLElement et pas à des SVG Elements
            let el' = unsafeCoerce el ∷ HE.HTMLElement
            {left, top, width, height} ← HE.getBoundingClientRect el'
            pure $ Just {
                x: (toNumber(ME.clientX mouseEv) - left) / width,
                y: (toNumber(ME.clientY mouseEv) - top) / height
            }
        _ → pure Nothing