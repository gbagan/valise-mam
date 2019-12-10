module Game.Generic (GenericGame) where
import MyPrelude
import Pha (VDom)

type GenericGame st msg =
    {   init ∷ Maybe msg
    ,   view ∷ st → VDom msg
    ,   onKeydown ∷ String → Maybe msg
    }