module Game.Generic (GenericGame) where
import MyPrelude
import Pha.Html (Html)

type GenericGame st msg =
    {   init ∷ Maybe msg
    ,   view ∷ st → Html msg
    ,   onKeydown ∷ String → Maybe msg
    }