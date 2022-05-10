module Game.Generic (GenericGame) where
import MamPrelude
import Pha.Html (Html)

type GenericGame st msg =
    {   init ∷ Maybe msg
    ,   view ∷ st → Html msg
    ,   onKeydown ∷ String → Maybe msg
    }