module UI.Icons where

import MamPrelude
import Data.List (null) as L
import Pha.Html (Html, Prop)
import Pha.Html as H
import Pha.Html.Events as E
import Lib.Helpers (partialUpdate, class PartialRecord)
import Game.Core
  ( GModel
  , class Game
  , class ScoreGame
  , Dialog(..)
  , Mode(..)
  , bestScore
  , class MsgWithCore
  , core
  , CoreMsg(..)
  , _help
  , _dialog
  , _history
  , _redoHistory
  , _mode
  , _nbRows
  , _nbColumns
  , _locked
  , _customSize
  )
import UI.Icon (iconbutton, defaultOptions, Options, Icon(..)) as I

iconbutton ∷ ∀ pos ext msg opts. PartialRecord opts I.Options ⇒ GModel pos ext → (Record opts) → Array (Prop msg) → Html msg
iconbutton model opts props =
  let
    opts2 = partialUpdate opts I.defaultOptions
  in
    I.iconbutton opts2 { disabled = opts2.disabled || model ^. _locked } props

icongroup ∷ ∀ a. String → Array (Html a) → Html a
icongroup title children =
  H.div []
    [ H.h2 [] [ H.text title ]
    , H.div [ H.class_ "ui-icon-grid" ] children
    ]

iundo ∷ ∀ msg pos ext. MsgWithCore msg ⇒ GModel pos ext → Html msg
iundo model =
  iconbutton
    model
    { icon: I.IconSymbol "#undo"
    , tooltip: Just "Annule le dernier coup effectué"
    , disabled: L.null (model ^. _history)
    }
    [ E.onClick \_ → core Undo ]

iredo ∷ ∀ msg pos ext. MsgWithCore msg ⇒ GModel pos ext → Html msg
iredo model =
  iconbutton
    model
    { icon: I.IconSymbol "#undo"
    , tooltip: Just "Rejoue le coup annulé"
    , disabled: L.null (model ^. _redoHistory)
    , style: [ "transform" ∧ "scaleX(-1)" ]
    }
    [ E.onClick \_ → core Redo ]

ireset ∷ ∀ msg pos ext. MsgWithCore msg ⇒ GModel pos ext → Html msg
ireset model =
  iconbutton
    model
    { icon: I.IconSymbol "#reset"
    , tooltip: Just "Recommence la partie"
    , disabled: L.null (model ^. _history)
    }
    [ E.onClick \_ → core Reset ]

iclear ∷ ∀ msg pos ext. MsgWithCore msg ⇒ GModel pos ext → Html msg
iclear model =
  iconbutton
    model
    { icon: I.IconSymbol "#clear"
    , tooltip: Just "Réinitialise le jeu"
    }
    [ E.onClick \_ → core Clear ]

ihelp ∷ ∀ msg pos ext. MsgWithCore msg ⇒ GModel pos ext → Html msg
ihelp model =
  iconbutton
    model
    { icon: I.IconSymbol "#help"
    , tooltip: Just "Aide"
    , selected: model ^. _help
    }
    [ E.onClick \_ → core ToggleHelp ]

irules ∷ ∀ msg pos ext. MsgWithCore msg ⇒ GModel pos ext → Html msg
irules model =
  iconbutton
    model
    { icon: I.IconSymbol "#rules"
    , tooltip: Just "Règles"
    , selected: selected
    }
    [ E.onClick \_ → core SetRulesDialog ]
  where
  selected = case model ^. _dialog of
    Rules → true
    _ → false

class IconLabel a where
  iconLabel ∷ a → Record I.Options → Record I.Options

instance IconLabel Int where
  iconLabel val opts = opts { icon = I.IconText $ show val }
else instance IconLabel a where
  iconLabel _ opts = opts

-- | groupe d'icones à choix unique
iconSelectGroup
  ∷ ∀ msg pos ext sel
  . IconLabel sel
  ⇒ Eq sel
  ⇒ GModel pos ext
  → String
  → Array sel
  → sel
  → (sel → msg)
  → (sel → Record I.Options → Record I.Options)
  → Html msg
iconSelectGroup model title values selected action optionFn =
  icongroup title $ values <#> \val →
    iconbutton model
      ( optionFn val
          ( iconLabel val I.defaultOptions
              { selected = val == selected
              }
          )
      )
      [ E.onClick \_ → action val ]

-- | groupe d'icones à choix unique, fonction alternative 
iconSelectGroup'
  ∷ ∀ msg pos ext sel
  . IconLabel sel
  ⇒ Eq sel
  ⇒ GModel pos ext
  → String
  → sel
  → (sel → msg)
  → Array (Tuple sel (Record I.Options → Record I.Options))
  → Html msg
iconSelectGroup' model title selected action values =
  icongroup title $ values <#> \(val ∧ optionFn) →
    iconbutton model
      ( optionFn
          ( iconLabel val I.defaultOptions
              { selected = val == selected
              }
          )
      )
      [ E.onClick \_ → action val ]

-- | groupe d'icones à choix multiple
iconSelectGroupM
  ∷ ∀ msg pos ext t sel
  . IconLabel sel
  ⇒ Eq sel
  ⇒ Foldable t
  ⇒ GModel pos ext
  → String
  → Array sel
  → t sel
  → (sel → msg)
  → (sel → Record I.Options → Record I.Options)
  → Html msg
iconSelectGroupM model title values selected action optionFn =
  icongroup title $ values <#> \val →
    iconbutton model
      ( optionFn val
          ( iconLabel val I.defaultOptions
              { selected = elem val selected
              }
          )
      )
      [ E.onClick \_ → action val ]

-- | groupe d'icones pour le choix d'une taille de plateau
iconSizesGroup
  ∷ ∀ msg pos ext
  . MsgWithCore msg
  ⇒ GModel pos ext
  → Array (Tuple Int Int)
  → Boolean
  → Html msg
iconSizesGroup model sizeList customSize =
  icongroup "Dimensions de la grille"
    $
      ( sizeList <#> \(rows ∧ cols) →
          iconbutton model
            { icon: I.IconText $ show rows <> "x" <> show cols
            , selected: rows == crows && cols == ccols && not csize
            }
            [ E.onClick \_ → core (SetGridSize rows cols false) ]
      )
    <>
      ( if customSize then
          [ iconbutton model
              { icon: I.IconText "NxM"
              , tooltip: Just "Taille personnalisée"
              , selected: csize
              }
              [ E.onClick \_ → core (SetCustomSize true) ]
          ]
        else []
      )
  where
  crows = model ^. _nbRows
  ccols = model ^. _nbColumns
  csize = model ^. _customSize

-- | groupe d'icones pour les jeux à deux joueurs
icons2Players ∷ ∀ msg pos ext mov. MsgWithCore msg ⇒ Game pos ext mov ⇒ GModel pos ext → Html msg
icons2Players model =
  icongroup "Mode de jeu"
    [ iconbutton
        model
        { icon: I.IconSymbol "#school", selected: model ^. _mode == RandomMode, tooltip: Just "IA mode facile" }
        [ E.onClick \_ → core (SetMode RandomMode) ]
    , iconbutton
        model
        { icon: I.IconSymbol "#enstein", selected: model ^. _mode == ExpertMode, tooltip: Just "IA mode expert" }
        [ E.onClick \_ → core (SetMode ExpertMode) ]
    , iconbutton
        model
        { icon: I.IconSymbol "#duel", selected: model ^. _mode == DuelMode, tooltip: Just "Affronte un autre joueur" }
        [ E.onClick \_ → core (SetMode DuelMode) ]
    , iconbutton
        model
        { icon: I.IconText "2P⇨", disabled: not (L.null $ model ^. _history) || model ^. _mode == DuelMode, tooltip: Just "L'IA commence" }
        [ E.onClick \_ → core ComputerStarts ]
    ]

iconBestScore ∷ ∀ msg pos ext mov. MsgWithCore msg ⇒ ScoreGame pos ext mov ⇒ GModel pos ext → Html msg
iconBestScore model =
  icongroup ("Meilleur score (" <> maybe "∅" (show ∘ fst) (bestScore model) <> ")")
    [ iconbutton
        model
        { icon: I.IconSymbol "#cup", disabled: isNothing (bestScore model), tooltip: Just "Meilleur score" }
        [ E.onClick \_ → core SetScoreDialog ]
    ]