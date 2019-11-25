module UI.Icons where
import MyPrelude
import Data.List (null) as L
import Pha (VDom, Prop, text, class_)
import Pha.Elements (div, h2)
import Pha.Events (onclick)
import Game.Core (GState, class Game, class ScoreGame, Dialog(..), Mode(..), bestScore,
                class MsgWithCore, core, CoreMsg(..),
                _help, _dialog, _history, _redoHistory, _mode, _nbRows, _nbColumns, _locked, _customSize)
import UI.Icon (iconbutton, Options, Icon(..)) as I

iconbutton :: ∀pos ext msg. GState pos ext -> (I.Options -> I.Options) -> Array (Prop msg) -> VDom msg
iconbutton state optionFn props =
    I.iconbutton (\opts -> let opts2 = optionFn opts in opts2{disabled = opts2.disabled || state^._locked}) props

icongroup :: ∀a. String -> Array (VDom a) -> VDom a
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

iundo :: ∀msg pos ext. MsgWithCore msg => GState pos ext -> VDom msg
iundo state =
    iconbutton
        state
        _{icon = I.IconSymbol "#undo",
           tooltip = Just "Annule le dernier coup effectué",
           disabled = L.null (state^._history)}
        [onclick $ core Undo]

iredo :: ∀msg pos ext. MsgWithCore msg => GState pos ext -> VDom msg
iredo state =
    iconbutton
        state
        _{icon = I.IconSymbol "#undo",
            tooltip = Just "Rejoue le coup annulé",
            disabled = L.null (state^._redoHistory),
            style = ["transform" ∧ "scaleX(-1)"]}
        [onclick $ core Redo]

ireset :: ∀msg pos ext. MsgWithCore msg => GState pos ext -> VDom msg
ireset state =
    iconbutton
        state
        _{icon = I.IconSymbol "#reset", tooltip = Just "Recommence la partie", disabled = L.null (state^._history)}
        [onclick $ core Reset]

ihelp :: ∀msg pos ext. MsgWithCore msg => GState pos ext -> VDom msg
ihelp state =
    iconbutton
        state
        _{icon = I.IconSymbol "#help", tooltip = Just "Aide", selected = state^._help}
        [onclick $ core ToggleHelp]

irules :: ∀msg pos ext. MsgWithCore msg => GState pos ext -> VDom msg
irules state =
    iconbutton
        state
        _{icon = I.IconSymbol "#rules", tooltip = Just "Règles", selected = selected}
        [onclick $ core $ SetRulesDialog]
    where
        selected = case state^._dialog of
            Rules -> true
            _ -> false

iconSelectGroup :: ∀msg pos ext sel. Show sel => Eq sel =>
    GState pos ext -> String -> Array sel -> sel -> (sel -> msg) -> (sel -> I.Options -> I.Options) -> VDom msg
iconSelectGroup state title values selected action optionFn =
    icongroup title $ values <#> \val ->
        iconbutton state (optionFn val ∘ _{
            icon = I.IconText $ show val,
            selected = val == selected
        }) [onclick (action val)]

iconSelectGroupM :: ∀msg pos ext t sel.
    Show sel => Eq sel => Foldable t =>
    GState pos ext -> String -> Array sel -> t sel -> (sel -> msg) -> (sel -> I.Options -> I.Options) -> VDom msg
iconSelectGroupM state title values selected action optionFn =
    icongroup title $ values <#> \val ->
        iconbutton state (optionFn val ∘ _{
            icon = I.IconText $ show val,
            selected = elem val selected
        }) [onclick (action val)]

iconSizesGroup :: ∀msg pos ext. MsgWithCore msg =>
    GState pos ext -> Array (Tuple Int Int) -> Boolean -> VDom msg
iconSizesGroup state sizeList customSize =
    icongroup "Dimensions de la grille" $
        (sizeList <#> \(rows ∧ cols) ->
            iconbutton state _{
                icon = I.IconText $ show rows <> "x" <> show cols,
                selected = rows == crows && cols == ccols && not csize
            } [onclick $ core (SetGridSize rows cols false)]
        ) <> (if customSize then [
            iconbutton state _{icon = I.IconText "NxM", tooltip = Just "Taille personnalisée", selected = csize}
                            [onclick $ core (SetCustomSize true)]
        ] else [])
    where
    crows = state^._nbRows
    ccols = state^._nbColumns
    csize = state^._customSize

icons2Players :: ∀msg pos ext mov. MsgWithCore msg => Game pos ext mov => GState pos ext -> VDom msg
icons2Players state =
    icongroup "Mode de jeu" [
        iconbutton
            state
            _{icon = I.IconSymbol "#school", selected = state^._mode == RandomMode, tooltip = Just "IA mode facile"}
            [onclick $ core (SetMode RandomMode)],
        iconbutton
            state
            _{icon = I.IconSymbol "#enstein", selected = state^._mode == ExpertMode, tooltip = Just "IA mode expert"}
            [onclick $ core (SetMode ExpertMode)],
        iconbutton
            state
            _{icon = I.IconSymbol "#duel", selected = state^._mode == DuelMode, tooltip = Just "Affronte un autre joueur"}
            [onclick $ core (SetMode DuelMode)],
        iconbutton
            state
            _{icon = I.IconText "2P⇨", disabled = not (L.null $ state^._history) || state^._mode == DuelMode, tooltip = Just "L'IA commence"}
            [onclick $ core ComputerStarts]
    ]

{-
iconBestScore :: ∀pos ext mov. ScoreGame pos ext mov => GState pos ext -> VDom (GState pos ext) EFFS
iconBestScore state =
    icongroup ("Meilleur score (" <> maybe "∅" (show ∘ fst) (bestScore state) <> ")") [
        iconbutton
            state
            _{icon = I.IconSymbol "#cup", disabled = isNothing (bestScore state), tooltip = Just "Meilleur score"}
            [onclick $ setState (_dialog .~ ScoreDialog)]
    ]
-}