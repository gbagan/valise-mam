module Game.Roue.Model where

import MyPrelude
import Lib.Util (swap)
import Control.Monad.Rec.Class (tailRecM, Step(..))
import Game.Core (class Game, class MsgWithCore, class MsgWithDnd, GState,
    CoreMsg,  DndMsg(DropOnBoard),
    coreUpdate, dndUpdate,
    genState, newGame, lockAction, _ext, _position, _showWin, defaultSizeLimit)
import Lib.Update (Update, get, modify, delay)

type Position = Array (Maybe Int)

data Location = Panel Int | Wheel Int | Board
derive instance eqLoc ∷ Eq Location

type Ext' = {
    size ∷ Int,
    rotation ∷ Int,  --- nombre de rotations effectuées, peut-être négatif et n'est pas borné.
    dragged ∷ Maybe Location
}
newtype Ext = Ext Ext'
type State = GState Position Ext

-- | état initial
istate ∷ State
istate = genState [] identity (Ext {rotation: 0, size: 5, dragged: Nothing})

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_rotation ∷ Lens' State Int
_rotation = _ext' ∘ lens _.rotation _{rotation = _}
_size ∷ Lens' State Int
_size = _ext' ∘ lens _.size _{size = _}
_dragged ∷ Lens' State (Maybe Location)
_dragged = _ext' ∘ lens _.dragged _{dragged = _}

-- | renvoie un tableau indiquant quelles sont les balles alignées avec leur couleur
aligned ∷ State → Array Boolean
aligned state =
    state^._position # mapWithIndex \index → case _ of
        Nothing → false
        Just c → mod (index + rot) n == c
    where
        n = length $ state^._position
        rot = state^._rotation

-- | comme validRotation mais avec seconde conditition en moins 
validRotation' ∷ State → Boolean
validRotation' state = (length $ filter identity $ aligned state) == 1

-- | une rotation est valide si exactement une couleur est alignée et il y a une balle pour chaque couleur         
validRotation ∷ State → Boolean
validRotation state = validRotation' state && (all isJust $ state^._position )

instance roueGame ∷ Game (Array (Maybe Int)) Ext {from ∷ Location, to ∷ Location} where
    name _ = "roue"

    play state move = act (state^._position) where
        act = case move of 
            {from: Panel from, to: Wheel to} → updateAt to (Just from)
            {from: Wheel from, to: Wheel to } → swap from to
            {from: Wheel from, to: Board} → updateAt from Nothing
            _ → const Nothing
    
    initialPosition state = pure $ replicate (state^._size) Nothing

    isLevelFinished _ = false
    
    onNewGame = pure ∘ set _rotation 0

    computerMove _ = pure Nothing
    sizeLimit = defaultSizeLimit
    updateScore st = st ∧ true
    onPositionChange = identity
    saveToJson _ = Nothing
    loadFromJson st _ = st

-- | tourne la roue de i crans
rotate ∷ Int → State → State
rotate i = _rotation +~ i

data Msg = Core CoreMsg | DnD (DndMsg Location) | Rotate Int | SetSize Int | Check
instance withcore ∷ MsgWithCore Msg where core = Core
instance withdnd ∷ MsgWithDnd Msg Location where dndmsg = DnD  
    
update ∷ Msg → Update State
update (Core msg) = coreUpdate msg
update (DnD DropOnBoard) = modify \state →
        let state2 = state # set _dragged Nothing in
        case state^._dragged of
            Just (Wheel i) → state2 # set (_position ∘ ix i) Nothing
            _ → state2
update (DnD msg) = dndUpdate _dragged msg
update (Rotate i) = modify $ rotate i
update (SetSize i) = newGame $ set _size i
update Check = lockAction $ get >>= \st → tailRecM go (st^._size) where
        go 0 = do
            modify $ set _showWin true
            delay 1000
            modify $ set _showWin false
            pure (Done unit)
        go i = do
            st2 ← get
            if not (validRotation st2) then
                pure $ Done unit
            else do
                modify $ rotate 1
                delay 600
                pure $ Loop (i-1)
