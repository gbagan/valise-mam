module UI.Icons where
import MyPrelude
import Data.List (null) as L
import Pha (VDom, Prop, text, class_)
import Pha.Action (Action, setState)
import Pha.Elements (div, h2)
import Pha.Attributes (onclick)
import Game.Core (GState, class Game, class ScoreGame, Dialog(..), Mode(..), bestScore,
                undoA, redoA, resetA, toggleHelpA, setModeA, computerStartsA, setGridSizeA,
                _help, _dialog, _history, _redoHistory, _mode, _nbRows, _nbColumns, _locked, _customSize)
import UI.Icon (iconbutton, Options, Icon(..)) as I
import Game.Effs (EFFS)

iconbutton :: ∀pos ext.
    GState pos ext
    -> (I.Options -> I.Options)
    -> Array (Prop (GState pos ext) EFFS)
    -> VDom (GState pos ext) EFFS
iconbutton state optionFn props =
    I.iconbutton (\opts -> let opts2 = optionFn opts in opts2{disabled = opts2.disabled || state^._locked}) props

icongroup :: ∀a. String -> Array (VDom a EFFS) -> VDom a EFFS
icongroup title children =
    div [] [
        h2 [] [text title],
        div [class_ "ui-icon-grid"] children
    ]

{-
iconSelect :: ∀a pos ext sel. Eq sel =>
    Lens' a (GState pos ext) -> GState pos ext -> sel -> (sel -> Action (GState pos ext)) -> sel
  -> (I.Options -> I.Options) -> VDom a
iconSelect lens state selection action value optionFn =
    iconbutton state (\opt -> optionFn $ opt{selected = value == selection}) [click $ lens 🔍 action value]
-}

iundo :: ∀pos ext. GState pos ext -> VDom (GState pos ext) EFFS
iundo state =
    iconbutton
        state
        _{icon = I.IconSymbol "#undo",
           tooltip = Just "Annule le dernier coup effectué",
           disabled = L.null (state^._history)}
        [onclick undoA]

iredo :: ∀pos ext. GState pos ext -> VDom (GState pos ext) EFFS
iredo state =
    iconbutton
        state
        _{icon = I.IconSymbol "#undo",
            tooltip = Just "Rejoue le coup annulé",
            disabled = L.null (state^._redoHistory),
            style = ["transform" ∧ "scaleX(-1)"]}
        [onclick redoA]

ireset :: ∀pos ext. GState pos ext -> VDom (GState pos ext) EFFS
ireset state =
    iconbutton
        state
        _{icon = I.IconSymbol "#reset", tooltip = Just "Recommence la partie", disabled = L.null (state^._history)}
        [onclick resetA]

ihelp :: ∀pos ext. GState pos ext -> VDom (GState pos ext) EFFS
ihelp state =
    iconbutton
        state
        _{icon = I.IconSymbol "#help", tooltip = Just "Aide", selected = state^._help}
        [onclick toggleHelpA]

irules :: ∀pos ext. GState pos ext -> VDom (GState pos ext) EFFS
irules state =
    iconbutton
        state
        _{icon = I.IconSymbol "#rules", tooltip = Just "Règles", selected = selected}
        [onclick $ setState (_dialog .~ Rules)]
    where
        selected = case state^._dialog of
            Rules -> true
            _ -> false

iconSelectGroup :: ∀pos ext sel. Show sel => Eq sel =>
    GState pos ext -> String -> Array sel -> sel -> (sel -> Action (GState pos ext) EFFS) 
    -> (sel -> I.Options -> I.Options) -> VDom (GState pos ext) EFFS
iconSelectGroup state title values selected action optionFn =
    icongroup title $ values <#> \val ->
        iconbutton state (optionFn val ∘ _{
            icon = I.IconText $ show val,
            selected = val == selected
        }) [onclick (action val)]

iconSelectGroupM :: ∀pos ext t sel.
    Show sel => Eq sel => Foldable t =>
    GState pos ext -> String -> Array sel -> t sel -> (sel -> Action (GState pos ext) EFFS)
    -> (sel -> I.Options -> I.Options) -> VDom (GState pos ext) EFFS
iconSelectGroupM state title values selected action optionFn =
    icongroup title $ values <#> \val ->
        iconbutton state (optionFn val ∘ _{
            icon = I.IconText $ show val,
            selected = elem val selected
        }) [onclick (action val)]

iconSizesGroup :: ∀pos ext mov. Game pos ext mov =>
    GState pos ext -> Array (Tuple Int Int) -> Boolean -> VDom (GState pos ext) EFFS
iconSizesGroup state sizeList customSize =
    icongroup "Dimensions de la grille" $
        (sizeList <#> \(rows ∧ cols) ->
            iconbutton state _{
                icon = I.IconText $ show rows <> "x" <> show cols,
                selected = rows == crows && cols == ccols && not csize
            } [onclick $ setGridSizeA rows cols false]
        ) <> (if customSize then [
            iconbutton state _{icon = I.IconText "NxM", tooltip = Just "Taille personnalisée", selected = csize}
                            [onclick $ setState (_customSize .~ true)]
        ] else [])
    where
    crows = state^._nbRows
    ccols = state^._nbColumns
    csize = state^._customSize

icons2Players :: ∀pos ext mov. Game pos ext mov => GState pos ext -> VDom (GState pos ext) EFFS
icons2Players state =
    icongroup "Mode de jeu" [
        iconbutton
            state
            _{icon = I.IconSymbol "#school", selected = state^._mode == RandomMode, tooltip = Just "IA mode facile"}
            [onclick $ setModeA RandomMode],
        iconbutton
            state
            _{icon = I.IconSymbol "#enstein", selected = state^._mode == ExpertMode, tooltip = Just "IA mode expert"}
            [onclick $ setModeA ExpertMode],
        iconbutton
            state
            _{icon = I.IconSymbol "#duel", selected = state^._mode == DuelMode, tooltip = Just "Affronte un autre joueur"}
            [onclick $ setModeA DuelMode],
        iconbutton
            state
            _{icon = I.IconText "2P⇨", disabled = not (L.null $ state^._history) || state^._mode == DuelMode, tooltip = Just "L'IA commence"}
            [onclick computerStartsA]
    ]

iconBestScore :: ∀pos ext mov. ScoreGame pos ext mov => GState pos ext -> VDom (GState pos ext) EFFS
iconBestScore state =
    icongroup ("Meilleur score (" <> maybe "∅" (show ∘ fst) (bestScore state) <> ")") [
        iconbutton
            state
            _{icon = I.IconSymbol "#cup", disabled = isNothing (bestScore state), tooltip = Just "Meilleur score"}
            [onclick $ setState (_dialog .~ ScoreDialog)]
    ]