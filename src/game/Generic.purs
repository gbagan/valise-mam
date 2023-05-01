module Game.Generic (GenericGame) where
import MamPrelude
import Pha.Html (Html)

type GenericGame model msg =
    {   init ∷ Maybe msg
    ,   view ∷ model → Html msg
    ,   onKeydown ∷ String → Maybe msg
    }