module UI.Icons where
import Prelude
import Data.Array (null, elem)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Lens (Lens', (^.), (.~))
import Pha.Class (VDom, Prop, Action)
import Pha.Action (action, (🎲))
import Pha.Html (click, style)
import Game.Core (State, class Game, Dialog(Rules), Mode(..),
                undoA, redoA, resetA, toggleHelpA, setModeA, computerStartsA, setGridSizeA,
                _help, _dialog, _history, _redoHistory, _mode, _nbRows, _nbColumns, _locked)
import UI.Icon (iconbutton, icongroup, Options, Icon(..)) as I

iconbutton :: forall a b d.
    State a b
    -> (I.Options -> I.Options)
    -> Array (Prop d)
    -> VDom d
iconbutton state optionFn props =
    I.iconbutton (\opts -> let opts2 = optionFn opts in opts2{disabled = opts2.disabled || state^._locked}) props

iconSelect :: forall a pos ext sel. Eq sel =>
    Lens' a (State pos ext) -> State pos ext -> sel -> (sel -> Action (State pos ext)) -> sel
  -> (I.Options -> I.Options) -> VDom a
iconSelect lens state selection action value optionFn =
    iconbutton state (\opt -> optionFn $ opt{selected = value == selection}) [click $ lens 🎲 action value]

iundo :: forall a b d. Lens' d (State a b) -> State a b -> VDom d
iundo lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#undo", tooltip = Just "Annule le dernier coup effectué", disabled = null $ state^._history})
        [click $ lens 🎲 undoA]

iredo :: forall a b d. Lens' d (State a b) -> State a b -> VDom d
iredo lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#undo", tooltip = Just "Rejoue le coup annulé", disabled = null $ state^._redoHistory})
        [click $ lens 🎲 redoA, style "transform" "scaleX(-1)"]

ireset :: forall a b d. Lens' d (State a b) -> State a b -> VDom d
ireset lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#reset", tooltip = Just "Recommence la partie", disabled = null $ state^._history})
        [click $ lens 🎲 resetA]

ihelp ::forall a b d. Lens' d (State a b) -> State a b -> VDom d
ihelp lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#help", tooltip = Just "Aide", selected = state^._help})
        [click $ lens 🎲 toggleHelpA]

irules :: forall a b d. Lens' d (State a b) -> State a b -> VDom d
irules lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#rules", tooltip = Just "Règles", selected = selected})
        [click $ lens 🎲 action (_dialog .~ Rules)]
    where
        selected = case state^._dialog of
            Rules -> true
            _ -> false

iconSelectGroup :: forall a pos ext d.
    Show a => Eq a =>
    Lens' d (State pos ext) -> State pos ext -> String -> Array a -> (a -> I.Options -> I.Options) -> a -> (a -> Action (State pos ext)) -> VDom d
iconSelectGroup lens state title values optionFn selected action =
    I.icongroup title $ values <#> \val ->
        iconbutton state (optionFn val <<< (_{
            icon = I.IconText $ show val,
            selected = val == selected
        })) [click $ lens 🎲 action val]

iconSelectGroupM :: forall a pos ext d.
    Show a => Eq a =>
    Lens' d (State pos ext) -> State pos ext -> String -> Array a -> Array a -> (a -> Action (State pos ext)) -> VDom d
iconSelectGroupM lens state title values selected action =
    I.icongroup title $ values <#> \val ->
        iconbutton state (_{
            icon = I.IconText $ show val,
            selected = elem val selected
        }) [click $ lens 🎲 action val]

iconSizesGroup :: forall a pos ext mov. Game pos ext mov =>
    Lens' a (State pos ext) -> State pos ext -> Array (Tuple Int Int) -> Boolean -> VDom a
iconSizesGroup lens state sizeList customSize =
    I.icongroup "Dimensions de la grille" $
        sizeList <#> \(Tuple rows cols) ->
            iconbutton state (_{
                icon = I.IconText $ show rows <> "x" <> show cols,
                selected = rows == crows && cols == ccols
            }) [click $ lens 🎲 setGridSizeA rows cols] where
    crows = state^._nbRows
    ccols = state^._nbColumns

icons2Players :: forall a pos ext mov. Game pos ext mov => Lens' a (State pos ext) -> State pos ext -> VDom a
icons2Players lens state =
    I.icongroup "Mode de jeu" [
        iconbutton
            state
            (_{icon = I.IconSymbol "#school", selected = state^._mode == RandomMode, tooltip = Just "IA mode facile"})
            [click $ lens 🎲 setModeA RandomMode],
        iconbutton
            state
            (_{icon = I.IconSymbol "#enstein", selected = state^._mode == ExpertMode, tooltip = Just "IA mode expert"})
            [click $ lens 🎲 setModeA ExpertMode],
        iconbutton
            state
            (_{icon = I.IconSymbol "#duel", selected = state^._mode == DuelMode, tooltip = Just "Affronte un autre joueur"})
            [click $ lens 🎲 setModeA DuelMode],
        iconbutton
            state
            (_{icon = I.IconText "2P⇨", disabled = not (null $ state^._history) || state^._mode == DuelMode, tooltip = Just "L'IA commence"})
            [click $ lens 🎲 computerStartsA]
    ]

