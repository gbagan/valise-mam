module Game.Common where

import MyPrelude

import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element as Element
import Web.Event.Event as E
import Web.PointerEvent.PointerEvent (PointerEvent)
import Web.PointerEvent.PointerEvent as PE
import Web.UIEvent.MouseEvent as ME

-- fonction utile pour labete et tiling
-- isomorphisme entre 2 façons de réprésenter une bête/tuile
--     (par coordonnées {row, col} ou par tableaau de booleéen partant de (-2, -2), jusqu'à (2, 2)
_isoCustom ∷ Iso' (Array {row ∷ Int, col ∷ Int}) (Array Boolean)      
_isoCustom = iso from to where
        from = flip updateAtIndices (replicate 25 false) ∘  map \{row, col} → (row * 5 + col + 12) ∧ true
        to = catMaybes ∘ mapWithIndex \i → if _ then Just {row: i / 5 - 2, col: i `mod` 5 - 2} else Nothing

releasePointerCapture ∷ PointerEvent → Effect Unit
releasePointerCapture ev =
    for_ (E.currentTarget $ PE.toEvent ev) \target ->
        for_ (Element.fromEventTarget target) \elem ->
            PE.releasePointerCapture (PE.pointerId ev) elem

pointerDecoder ∷ PointerEvent → Effect (Maybe { x ∷ Number, y ∷ Number })
pointerDecoder ev = do
    case E.currentTarget (PE.toEvent ev) of
        Just el → do
            -- dans l'implémentation actuelle en purescript, getBoundingClientRect ne s'applique
            -- qu'à des HTMLElement et pas à des SVG Elements
            let el' = unsafeCoerce el -- ∷ HE.HTMLElement
            let mev = PE.toMouseEvent ev
            {left, top, width, height} ← Element.getBoundingClientRect el'
            pure $ Just {
                x: (toNumber(ME.clientX mev) - left) / width,
                y: (toNumber(ME.clientY mev) - top) / height
            }
        _ → pure Nothing