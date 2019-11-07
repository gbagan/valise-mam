module UI.Icons where
import MyPrelude
import Pha (VDom, Prop, text)
import Pha.Action (Action, action, (🔍))
import Pha.Html (div', h2, class', click)
import Game.Core (GState, class Game, Dialog(Rules), Mode(..),
                undoA, redoA, resetA, toggleHelpA, setModeA, computerStartsA, setGridSizeA,
                _help, _dialog, _history, _redoHistory, _mode, _nbRows, _nbColumns, _locked, _customSize)
import UI.Icon (iconbutton, Options, Icon(..)) as I
import Game.Effs (EFFS)

iconbutton :: ∀a b d.
    GState a b
    -> (I.Options -> I.Options)
    -> Array (Prop d EFFS)
    -> VDom d EFFS
iconbutton state optionFn props =
    I.iconbutton (\opts -> let opts2 = optionFn opts in opts2{disabled = opts2.disabled || state^._locked}) props

icongroup :: ∀a. String -> Array (VDom a EFFS) -> VDom a EFFS
icongroup title children =
    div' [] [
        h2 [] [text title],
        div' [class' "ui-icon-grid" true] children
    ]

{-
iconSelect :: ∀a pos ext sel. Eq sel =>
    Lens' a (GState pos ext) -> GState pos ext -> sel -> (sel -> Action (GState pos ext)) -> sel
  -> (I.Options -> I.Options) -> VDom a
iconSelect lens state selection action value optionFn =
    iconbutton state (\opt -> optionFn $ opt{selected = value == selection}) [click $ lens 🔍 action value]
-}

iundo :: ∀a b d. Lens' d (GState a b) -> GState a b -> VDom d EFFS
iundo lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#undo", tooltip = Just "Annule le dernier coup effectué", disabled = null $ state^._history})
        [click $ lens 🔍 undoA]

iredo :: ∀a b d. Lens' d (GState a b) -> GState a b -> VDom d EFFS
iredo lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#undo",
            tooltip = Just "Rejoue le coup annulé",
            disabled = null $ state^._redoHistory,
            style = [Tuple "transform" "scaleX(-1)"]})
        [click $ lens 🔍 redoA]

ireset :: ∀a b d. Lens' d (GState a b) -> GState a b -> VDom d EFFS
ireset lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#reset", tooltip = Just "Recommence la partie", disabled = null $ state^._history})
        [click $ lens 🔍 resetA]

ihelp :: ∀a b d. Lens' d (GState a b) -> GState a b -> VDom d EFFS
ihelp lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#help", tooltip = Just "Aide", selected = state^._help})
        [click $ lens 🔍 toggleHelpA]

irules :: ∀a b d. Lens' d (GState a b) -> GState a b -> VDom d EFFS
irules lens state =
    iconbutton
        state
        (_{icon = I.IconSymbol "#rules", tooltip = Just "Règles", selected = selected})
        [click $ lens 🔍 action (_dialog .~ Rules)]
    where
        selected = case state^._dialog of
            Rules -> true
            _ -> false

iconSelectGroup :: ∀a pos ext d.
    Show a => Eq a =>
    Lens' d (GState pos ext) -> GState pos ext -> String -> Array a -> a -> (a -> Action (GState pos ext) EFFS) 
    -> (a -> I.Options -> I.Options) -> VDom d EFFS
iconSelectGroup lens state title values selected action optionFn =
    icongroup title $ values <#> \val ->
        iconbutton state (optionFn val ∘ (_{
            icon = I.IconText $ show val,
            selected = val == selected
        })) [click $ lens 🔍 action val]

iconSelectGroupM :: ∀a t pos ext d.
    Show a => Eq a => Foldable t =>
    Lens' d (GState pos ext) -> GState pos ext -> String -> Array a -> t a -> (a -> Action (GState pos ext) EFFS)
    -> (a -> I.Options -> I.Options) -> VDom d EFFS
iconSelectGroupM lens state title values selected action optionFn =
    icongroup title $ values <#> \val ->
        iconbutton state (optionFn val ∘ (_{
            icon = I.IconText $ show val,
            selected = elem val selected
        })) [click $ lens 🔍 action val]

iconSizesGroup :: ∀a pos ext mov. Game pos ext mov =>
    Lens' a (GState pos ext) -> GState pos ext -> Array (Tuple Int Int) -> Boolean -> VDom a EFFS
iconSizesGroup lens state sizeList customSize =
    icongroup "Dimensions de la grille" $
        (sizeList <#> \(Tuple rows cols) ->
            iconbutton state (_{
                icon = I.IconText $ show rows <> "x" <> show cols,
                selected = rows == crows && cols == ccols && not csize
            }) [click $ lens 🔍 setGridSizeA rows cols false]
        ) <> (if customSize then [
            iconbutton state (_{icon = I.IconText "NxM", tooltip = Just "Taille personnalisée", selected = csize})
                            [click $ lens 🔍 action (_customSize .~ true)]
        ] else [])
    where
    crows = state^._nbRows
    ccols = state^._nbColumns
    csize = state^._customSize

icons2Players :: ∀a pos ext mov. Game pos ext mov => Lens' a (GState pos ext) -> GState pos ext -> VDom a EFFS
icons2Players lens state =
    icongroup "Mode de jeu" [
        iconbutton
            state
            (_{icon = I.IconSymbol "#school", selected = state^._mode == RandomMode, tooltip = Just "IA mode facile"})
            [click $ lens 🔍 setModeA RandomMode],
        iconbutton
            state
            (_{icon = I.IconSymbol "#enstein", selected = state^._mode == ExpertMode, tooltip = Just "IA mode expert"})
            [click $ lens 🔍 setModeA ExpertMode],
        iconbutton
            state
            (_{icon = I.IconSymbol "#duel", selected = state^._mode == DuelMode, tooltip = Just "Affronte un autre joueur"})
            [click $ lens 🔍 setModeA DuelMode],
        iconbutton
            state
            (_{icon = I.IconText "2P⇨", disabled = not (null $ state^._history) || state^._mode == DuelMode, tooltip = Just "L'IA commence"})
            [click $ lens 🔍 computerStartsA]
    ]

