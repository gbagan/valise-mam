module Game.Paths.Model where
import MyPrelude
import Data.Array (init)
import Lib.Random as R
import Lib.Util (dCoords, rangeWithStep)
import Game.Core (GState, class Game, class MsgWithCore, CoreMsg, SizeLimit(..), 
        coreUpdate,
        _ext, newGame, genState, _nbRows, _nbColumns, _position, playA)
import Lib.Update (Update, get)

data Mode = Mode1 | Mode2
derive instance eqMode ∷ Eq Mode

-- | une position représente le chemin que l'on a effectué avec le héros
-- | c'est à dire la liste des positions des cases
-- | un coup (move) représente le numéro de cases où l'on souhaite aller
-- |    la case n'est pas forcemment adjacente à la position du héro
-- |    mais accessible en ligne horizontale ou verticale

type Position = Array Int
type Ext' = { exit ∷ Maybe Int, mode ∷ Mode }
newtype Ext = Ext Ext'
type State = GState Position Ext

istate ∷ State
istate = genState [] _{nbRows = 4, nbColumns = 6} (Ext { exit: Nothing, mode: Mode1 })

_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_exit ∷ Lens' State (Maybe Int)
_exit = _ext' ∘ prop (Proxy ∷ _ "exit")
_mode ∷ Lens' State Mode
_mode = _ext' ∘ prop (Proxy ∷ _ "mode")

-- renvoie un chemin horizontal ou vertical entre u et v si celui ci existe (u exclus du chemin)
pathBetween ∷ Int → Int → Int → Maybe (Array Int)
pathBetween columns u v =
    let {row, col} = dCoords columns u v in
    if row == 0 then
        Just (if u < v then rangeWithStep (u + 1) v 1 else rangeWithStep (u - 1) v (-1))
    else if col == 0 then
        Just (if u < v then rangeWithStep (u + columns) v columns else rangeWithStep (u - columns) v (-columns))
    else
        Nothing

-- teste si un chemin est valide (sans répétition de sommets sauf les extrémités si cela créé un cycle hamiltonien)
-- on ne peut pas passer par le sommet de sortie sauf si c'est le sommet final
isValidPath ∷ State → Array Int → Boolean
isValidPath state path = fromMaybe true $ do
    exit ← state ^. _exit
    path2 ← init path
    path3 ← tail path2
    begin ← head path
    end ← last path
    pure $ length (nub path2) == length path2 
        && not (elem exit path3)
        && not (elem end path3) 
        && (begin ≠ end 
           || length path == (state^._nbRows) * (state^._nbColumns) + (if begin == exit then 1 else 0) && end == exit
            )

instance game ∷ Game Position Ext Int where
    name _ = "paths"

    play state v =
        case last (state^._position) of
            Nothing → if state^._mode == Mode2 then Just [v] else Nothing
            Just last → do
                p ← pathBetween (state^._nbColumns) last v 
                guard $ not (null p) && isValidPath state (state^._position <> p)
                Just (state^._position <> p)

    isLevelFinished state =
        length (state^._position) == state^._nbColumns * state^._nbRows 
                                    + (if state^._exit == head (state^._position) then 1 else 0)

    initialPosition state = pure $ case state^._exit of
        Nothing → []
        Just exit → [exit]

    onNewGame state = flip (set _exit) state <$> do
        if state^._mode == Mode1 then
            Just <$> R.int' (state^._nbRows * state^._nbColumns) 
        else
            pure Nothing

    sizeLimit _ = SizeLimit 2 2 9 9

    -- méthodes par défault
    computerMove _ = pure Nothing
    updateScore st = st ∧ true
    onPositionChange = identity
    saveToJson _ = Nothing
    loadFromJson st _ = st

data Msg = Core CoreMsg | SelectVertex Int | SelectMode Mode
instance withcore ∷ MsgWithCore Msg where core = Core
    
update ∷ Msg → Update State Unit
update (Core msg) = coreUpdate msg

update (SelectVertex v) = do
    state ← get
    if null (state^._position) then
        _position .= [v]
    else if isNothing (state^._exit) then
        _exit .= Just v
    else
        playA v

update (SelectMode mode) = newGame $ set _mode mode