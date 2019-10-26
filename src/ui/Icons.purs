module UI.Icons where
import Prelude
import Data.Array (null, elem)
import Data.Maybe (Maybe(..))
import Optic.Core (Lens', (^.), (.~))
import Pha (VDom, Prop, class ClsAction, (🎲))
import Pha.Html (click, style)
import Lib.Game (State, undo, redo, reset, Dialog(Rules), Mode(..),
                _dialog, _history, _redoHistory, _mode)
import UI.Icon (iconbutton, icongroup, Options, Icon(..)) as I

iconbutton :: forall a b d.
    State a b
    -> (I.Options -> I.Options)
    -> Array (Prop d)
    -> VDom d
iconbutton state optionFn props = I.iconbutton optionFn props

iundo :: forall a b d. Lens' d (State a b) -> State a b -> VDom d
iundo lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#undo", tooltip = Just "Annule le dernier coup effectué", disabled = null $ state^._history})
        [click $ lens 🎲 undo]

iredo :: forall a b d. Lens' d (State a b) -> State a b -> VDom d
iredo lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#undo", tooltip = Just "Rejoue le coup annulé", disabled = null $ state^._redoHistory})
        [click $ lens 🎲 redo, style "transform" "scaleX(-1)"]

ireset :: forall a b d. Lens' d (State a b) -> State a b -> VDom d
ireset lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#reset", tooltip = Just "Recommence la partie", disabled = null $ state^._history})
        [click $ lens 🎲 reset]

irules :: forall a b d. Lens' d (State a b) -> State a b -> VDom d
irules lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#rules", tooltip = Just "Règles", selected = selected})
        [click $ lens 🎲 _dialog .~ Rules]
    where
        selected = case state^._dialog of
            Rules -> true
            _ -> false

iconSelectGroup :: forall a pos ext d act.
    Show a => Eq a => ClsAction (State pos ext) act =>
    Lens' d (State pos ext) -> State pos ext -> String -> Array a -> a -> (a -> act) -> VDom d
iconSelectGroup lens state title values selected action =
    I.icongroup title $ values <#> \val ->
        iconbutton state (_{
            icon = I.IconText $ show val,
            selected = val == selected
        }) [click $ lens 🎲 action val]

iconSelectGroupM :: forall a pos ext d act.
    Show a => Eq a => ClsAction (State pos ext) act =>
    Lens' d (State pos ext) -> State pos ext -> String -> Array a -> Array a -> (a -> act) -> VDom d
iconSelectGroupM lens state title values selected action =
    I.icongroup title $ values <#> \val ->
        iconbutton state (_{
            icon = I.IconText $ show val,
            selected = elem val selected
        }) [click $ lens 🎲 action val]

icons2Players :: forall a b d. Lens' d (State a b) -> State a b -> VDom d
icons2Players lens state =
    I.icongroup "Mode de jeu" [
        iconbutton
            state
            (_{icon = I.IconSymbol "#school", selected = state^._mode == RandomMode, tooltip = Just "IA mode facile"})
            [click $ lens 🎲 _mode .~ RandomMode],
        iconbutton
            state
            (_{icon = I.IconSymbol "#enstein", selected = state^._mode == ExpertMode, tooltip = Just "IA mode expert"})
            [click $ lens 🎲 _mode .~ ExpertMode],
        iconbutton
            state
            (_{icon = I.IconSymbol "#duel", selected = state^._mode == DuelMode, tooltip = Just "Affronte un autre joueur"})
            [click $ lens 🎲 _mode .~ DuelMode]
    ]

