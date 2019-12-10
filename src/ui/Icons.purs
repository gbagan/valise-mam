module UI.Icons where
import MyPrelude
import Data.List (null) as L
import Pha (VDom, Prop, text, class_)
import Pha.Elements (div, h2)
import Pha.Events (onclick)
import Lib.Util (partialUpdate, class PartialRecord)
import Game.Core (GState, class Game, class ScoreGame, Dialog(..), Mode(..), bestScore,
                class MsgWithCore, core, CoreMsg(..),
                _help, _dialog, _history, _redoHistory, _mode, _nbRows, _nbColumns, _locked, _customSize)
import UI.Icon (iconbutton, defaultOptions, Options, Icon(..)) as I

iconbutton ∷ ∀pos ext msg opts. PartialRecord opts I.Options ⇒ GState pos ext → (Record opts) → Array (Prop msg) → VDom msg
iconbutton state opts props =
    let opts2 = partialUpdate opts I.defaultOptions in
    I.iconbutton opts2{disabled = opts2.disabled || state^._locked} props

icongroup ∷ ∀a. String → Array (VDom a) → VDom a
icongroup title children =
    div []
    [   h2 [] [text title]
    ,   div [class_ "ui-icon-grid"] children
    ]

iundo ∷ ∀msg pos ext. MsgWithCore msg ⇒ GState pos ext → VDom msg
iundo state =
    iconbutton
        state
        {   icon: I.IconSymbol "#undo"
        ,   tooltip: Just "Annule le dernier coup effectué"
        ,   disabled: L.null (state^._history)
        }
        [onclick $ core Undo]

iredo ∷ ∀msg pos ext. MsgWithCore msg ⇒ GState pos ext → VDom msg
iredo state =
    iconbutton
        state
        {   icon: I.IconSymbol "#undo"
        ,   tooltip: Just "Rejoue le coup annulé"
        ,   disabled: L.null (state^._redoHistory)
        ,   style: ["transform" ∧ "scaleX(-1)"]
        }
        [onclick $ core Redo]

ireset ∷ ∀msg pos ext. MsgWithCore msg ⇒ GState pos ext → VDom msg
ireset state =
    iconbutton
        state
        {   icon: I.IconSymbol "#reset"
        ,   tooltip: Just "Recommence la partie"
        ,   disabled: L.null (state^._history)
        }
        [onclick $ core Reset]

ihelp ∷ ∀msg pos ext. MsgWithCore msg ⇒ GState pos ext → VDom msg
ihelp state =
    iconbutton
        state
        {   icon: I.IconSymbol "#help"
        ,   tooltip: Just "Aide"
        ,   selected: state^._help
        }
        [onclick $ core ToggleHelp]

irules ∷ ∀msg pos ext. MsgWithCore msg ⇒ GState pos ext → VDom msg
irules state =
    iconbutton
        state
        {   icon: I.IconSymbol "#rules"
        ,   tooltip: Just "Règles"
        ,   selected: selected
        }
        [onclick $ core $ SetRulesDialog]
    where
        selected = case state^._dialog of
            Rules → true
            _ → false

class DefIconText a where
    defIconText ∷ a → Record I.Options → Record I.Options

instance defint ∷ DefIconText Int where
    defIconText val opts = opts{icon = I.IconText $ show val}
else instance defa ∷ DefIconText a where
    defIconText _ opts = opts

-- | groupe d'icones à choix unique
iconSelectGroup ∷ ∀msg pos ext sel. DefIconText sel ⇒ Eq sel ⇒
    GState pos ext → String → Array sel → sel → (sel → msg) → (sel → Record I.Options → Record I.Options) → VDom msg
iconSelectGroup state title values selected action optionFn =
    icongroup title $ values <#> \val →
        iconbutton state (optionFn val (defIconText val I.defaultOptions{
            selected = val == selected
        })) [onclick (action val)]

-- | groupe d'icones à choix multiple
iconSelectGroupM ∷ ∀msg pos ext t sel.
    DefIconText sel ⇒ Eq sel ⇒ Foldable t ⇒
    GState pos ext → String → Array sel → t sel → (sel → msg) → (sel → Record I.Options → Record I.Options) → VDom msg
iconSelectGroupM state title values selected action optionFn =
    icongroup title $ values <#> \val →
        iconbutton state (optionFn val (defIconText val I.defaultOptions{
            selected = elem val selected
        })) [onclick (action val)]

-- | groupe d'icones pour le choix d'une taille de plateau
iconSizesGroup ∷ ∀msg pos ext. MsgWithCore msg ⇒
    GState pos ext → Array (Tuple Int Int) → Boolean → VDom msg
iconSizesGroup state sizeList customSize =
    icongroup "Dimensions de la grille" $
        (sizeList <#> \(rows ∧ cols) →
            iconbutton state 
            {   icon: I.IconText $ show rows <> "x" <> show cols
            ,   selected: rows == crows && cols == ccols && not csize
            }
            [onclick $ core (SetGridSize rows cols false)]
        ) <> (if customSize then [
            iconbutton state
            {   icon: I.IconText "NxM"
            ,   tooltip: Just "Taille personnalisée"
            ,   selected: csize
            }
            [onclick $ core (SetCustomSize true)]
        ] else [])
    where
    crows = state^._nbRows
    ccols = state^._nbColumns
    csize = state^._customSize

-- | groupe d'icones pour les jeux à deux joueurs
icons2Players ∷ ∀msg pos ext mov. MsgWithCore msg ⇒ Game pos ext mov ⇒ GState pos ext → VDom msg
icons2Players state =
    icongroup "Mode de jeu" [
        iconbutton
            state
            {icon: I.IconSymbol "#school", selected: state^._mode == RandomMode, tooltip: Just "IA mode facile"}
            [onclick $ core (SetMode RandomMode)],
        iconbutton
            state
            {icon: I.IconSymbol "#enstein", selected: state^._mode == ExpertMode, tooltip: Just "IA mode expert"}
            [onclick $ core (SetMode ExpertMode)],
        iconbutton
            state
            {icon: I.IconSymbol "#duel", selected: state^._mode == DuelMode, tooltip: Just "Affronte un autre joueur"}
            [onclick $ core (SetMode DuelMode)],
        iconbutton
            state
            {icon: I.IconText "2P⇨", disabled: not (L.null $ state^._history) || state^._mode == DuelMode, tooltip: Just "L'IA commence"}
            [onclick $ core ComputerStarts]
    ]

iconBestScore ∷ ∀msg pos ext mov. MsgWithCore msg ⇒ ScoreGame pos ext mov ⇒ GState pos ext → VDom msg
iconBestScore state =
    icongroup ("Meilleur score (" <> maybe "∅" (show ∘ fst) (bestScore state) <> ")") [
        iconbutton
            state
            {icon: I.IconSymbol "#cup", disabled: isNothing (bestScore state), tooltip: Just "Meilleur score"}
            [onclick $ core SetScoreDialog]
    ]