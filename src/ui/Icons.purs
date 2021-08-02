module UI.Icons where
import MyPrelude
import Data.List (null) as L
import Pha.Html (Html, Prop)
import Pha.Html as H
import Pha.Html.Events as E
import Lib.Util (partialUpdate, class PartialRecord)
import Game.Core (GState, class Game, class ScoreGame, Dialog(..), Mode(..), bestScore,
                class MsgWithCore, core, CoreMsg(..),
                _help, _dialog, _history, _redoHistory, _mode, _nbRows, _nbColumns, _locked, _customSize)
import UI.Icon (iconbutton, defaultOptions, Options, Icon(..)) as I

iconbutton ∷ ∀pos ext msg opts. PartialRecord opts I.Options ⇒ GState pos ext → (Record opts) → Array (Prop msg) → Html msg
iconbutton state opts props =
    let opts2 = partialUpdate opts I.defaultOptions in
    I.iconbutton opts2{disabled = opts2.disabled || state^._locked} props

icongroup ∷ ∀a. String → Array (Html a) → Html a
icongroup title children =
    H.div []
    [   H.h2 [] [H.text title]
    ,   H.div [H.class_ "ui-icon-grid"] children
    ]

iundo ∷ ∀msg pos ext. MsgWithCore msg ⇒ GState pos ext → Html msg
iundo state =
    iconbutton
        state
        {   icon: I.IconSymbol "#undo"
        ,   tooltip: Just "Annule le dernier coup effectué"
        ,   disabled: L.null (state^._history)
        }
        [E.onClick $ core Undo]

iredo ∷ ∀msg pos ext. MsgWithCore msg ⇒ GState pos ext → Html msg
iredo state =
    iconbutton
        state
        {   icon: I.IconSymbol "#undo"
        ,   tooltip: Just "Rejoue le coup annulé"
        ,   disabled: L.null (state^._redoHistory)
        ,   style: ["transform" ∧ "scaleX(-1)"]
        }
        [E.onClick $ core Redo]

ireset ∷ ∀msg pos ext. MsgWithCore msg ⇒ GState pos ext → Html msg
ireset state =
    iconbutton
        state
        {   icon: I.IconSymbol "#reset"
        ,   tooltip: Just "Recommence la partie"
        ,   disabled: L.null (state^._history)
        }
        [E.onClick $ core Reset]

iclear ∷ ∀msg pos ext. MsgWithCore msg ⇒ GState pos ext → Html msg
iclear state =
    iconbutton
        state
        {   icon: I.IconSymbol "#clear"
        ,   tooltip: Just "Réinitialise le jeu"
        }
        [E.onClick $ core Clear]

ihelp ∷ ∀msg pos ext. MsgWithCore msg ⇒ GState pos ext → Html msg
ihelp state =
    iconbutton
        state
        {   icon: I.IconSymbol "#help"
        ,   tooltip: Just "Aide"
        ,   selected: state^._help
        }
        [E.onClick $ core ToggleHelp]

irules ∷ ∀msg pos ext. MsgWithCore msg ⇒ GState pos ext → Html msg
irules state =
    iconbutton
        state
        {   icon: I.IconSymbol "#rules"
        ,   tooltip: Just "Règles"
        ,   selected: selected
        }
        [E.onClick $ core $ SetRulesDialog]
    where
        selected = case state^._dialog of
            Rules → true
            _ → false

class IconLabel a where
    iconLabel ∷ a → Record I.Options → Record I.Options

instance IconLabel Int where
    iconLabel val opts = opts{icon = I.IconText $ show val}
else instance IconLabel a where
    iconLabel _ opts = opts

-- | groupe d'icones à choix unique
iconSelectGroup ∷ ∀msg pos ext sel. IconLabel sel ⇒ Eq sel ⇒
    GState pos ext → String → Array sel → sel → (sel → msg) → (sel → Record I.Options → Record I.Options) → Html msg
iconSelectGroup state title values selected action optionFn =
    icongroup title $ values <#> \val →
        iconbutton state (optionFn val (iconLabel val I.defaultOptions{
            selected = val == selected
        })) [E.onClick (action val)]

-- | groupe d'icones à choix unique, fonction alternative 
iconSelectGroup' ∷ ∀msg pos ext sel. IconLabel sel ⇒ Eq sel ⇒
    GState pos ext → String → sel → (sel → msg) → Array (Tuple sel (Record I.Options → Record I.Options)) → Html msg
iconSelectGroup' state title selected action values =
    icongroup title $ values <#> \(val ∧ optionFn) →
        iconbutton state (optionFn (iconLabel val I.defaultOptions{
            selected = val == selected
        })) [E.onClick (action val)]

-- | groupe d'icones à choix multiple
iconSelectGroupM ∷ ∀msg pos ext t sel.
    IconLabel sel ⇒ Eq sel ⇒ Foldable t ⇒
    GState pos ext → String → Array sel → t sel → (sel → msg) → (sel → Record I.Options → Record I.Options) → Html msg
iconSelectGroupM state title values selected action optionFn =
    icongroup title $ values <#> \val →
        iconbutton state (optionFn val (iconLabel val I.defaultOptions{
            selected = elem val selected
        })) [E.onClick (action val)]

-- | groupe d'icones pour le choix d'une taille de plateau
iconSizesGroup ∷ ∀msg pos ext. MsgWithCore msg ⇒
    GState pos ext → Array (Tuple Int Int) → Boolean → Html msg
iconSizesGroup state sizeList customSize =
    icongroup "Dimensions de la grille" $
        (sizeList <#> \(rows ∧ cols) →
            iconbutton state 
            {   icon: I.IconText $ show rows <> "x" <> show cols
            ,   selected: rows == crows && cols == ccols && not csize
            }
            [E.onClick $ core (SetGridSize rows cols false)]
        ) <> (if customSize then [
            iconbutton state
            {   icon: I.IconText "NxM"
            ,   tooltip: Just "Taille personnalisée"
            ,   selected: csize
            }
            [E.onClick $ core (SetCustomSize true)]
        ] else [])
    where
    crows = state^._nbRows
    ccols = state^._nbColumns
    csize = state^._customSize

-- | groupe d'icones pour les jeux à deux joueurs
icons2Players ∷ ∀msg pos ext mov. MsgWithCore msg ⇒ Game pos ext mov ⇒ GState pos ext → Html msg
icons2Players state =
    icongroup "Mode de jeu" [
        iconbutton
            state
            {icon: I.IconSymbol "#school", selected: state^._mode == RandomMode, tooltip: Just "IA mode facile"}
            [E.onClick $ core (SetMode RandomMode)],
        iconbutton
            state
            {icon: I.IconSymbol "#enstein", selected: state^._mode == ExpertMode, tooltip: Just "IA mode expert"}
            [E.onClick $ core (SetMode ExpertMode)],
        iconbutton
            state
            {icon: I.IconSymbol "#duel", selected: state^._mode == DuelMode, tooltip: Just "Affronte un autre joueur"}
            [E.onClick $ core (SetMode DuelMode)],
        iconbutton
            state
            {icon: I.IconText "2P⇨", disabled: not (L.null $ state^._history) || state^._mode == DuelMode, tooltip: Just "L'IA commence"}
            [E.onClick $ core ComputerStarts]
    ]

iconBestScore ∷ ∀msg pos ext mov. MsgWithCore msg ⇒ ScoreGame pos ext mov ⇒ GState pos ext → Html msg
iconBestScore state =
    icongroup ("Meilleur score (" <> maybe "∅" (show ∘ fst) (bestScore state) <> ")") [
        iconbutton
            state
            {icon: I.IconSymbol "#cup", disabled: isNothing (bestScore state), tooltip: Just "Meilleur score"}
            [E.onClick $ core SetScoreDialog]
    ]