module UI.Icon where

import MamPrelude

import Data.Tuple (uncurry)
import Lib.Util (partialUpdate, class PartialRecord)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Web.Event.Event (preventDefault)

data Icon = IconText String | IconSymbol String | IconNone

type Options =
  ( icon ∷ Icon
  , selected ∷ Boolean
  , tooltip ∷ Maybe String
  , round ∷ Boolean
  , large ∷ Boolean
  , hidden ∷ Boolean
  , disabled ∷ Boolean
  , style ∷ Array (Tuple String String)
  )

defaultOptions ∷ Record Options
defaultOptions =
  { icon: IconNone
  , selected: false
  , tooltip: Nothing
  , round: false
  , large: false
  , hidden: false
  , disabled: false
  , style: []
  }

iconbutton ∷ ∀ a opts. PartialRecord opts Options ⇒ Record opts → Array (H.Prop a) → Html a
iconbutton opts props =
  let
    { icon, selected, tooltip, round, large, hidden, disabled: d, style: st } = partialUpdate opts defaultOptions
  in
    H.button
      ( [ H.class_ "ui-icon"
        , H.class' "selected" selected
        , H.class' "round" round
        , H.class' "large" large
        , H.class' "hidden" hidden
        , P.disabled d
        , E.on "contextmenu" \ev → preventDefault ev *> pure Nothing
        ] <> props
      ) $
      [ case icon of
          IconSymbol symbol → H.svg ((uncurry H.style <$> st) <> [ P.width "100%", P.height "100%" ])
            [ H.use [ P.href symbol, H.class_ "ui-icon-symbol" ]
            ]
          IconText t → H.span [ H.class_ "ui-icon-text" ] [ H.text t ]
          IconNone → H.empty
      , H.maybe tooltip \t → H.span [ H.class_ "ui-icon-tooltip" ] [ H.text t ]
      ]
