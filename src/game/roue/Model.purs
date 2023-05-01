module Game.Roue.Model where

import MamPrelude
import Control.Monad.Rec.Class (tailRecM, Step(..))
import Game.Core (class Game, class MsgWithCore, class MsgWithDnd, GModel,
    CoreMsg,  DndMsg(DropOnBoard),
    coreUpdate, dndUpdate,
    genModel, newGame, lockAction, _ext, _position, _showWin, defaultSizeLimit, defaultUpdateScore)
import Lib.Util (count, swap)
import Lib.Update (UpdateMam, delay)

type Position = Array (Maybe Int)

data Location = Panel Int | Wheel Int | Board
derive instance Eq Location

type Ext' = {
    size ∷ Int,
    rotation ∷ Int,  --- nombre de rotations effectuées, peut-être négatif et n'est pas borné.
    dragged ∷ Maybe Location
}
newtype Ext = Ext Ext'
type Model = GModel Position Ext

-- | état initial
imodel ∷ Model
imodel = genModel [] identity (Ext {rotation: 0, size: 5, dragged: Nothing})

-- lenses
_ext' ∷ Lens' Model Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_rotation ∷ Lens' Model Int
_rotation = _ext' ∘ prop (Proxy ∷ _ "rotation")
_size ∷ Lens' Model Int
_size = _ext' ∘ prop (Proxy ∷ _ "size")
_dragged ∷ Lens' Model (Maybe Location)
_dragged = _ext' ∘ prop (Proxy ∷ _ "dragged")

-- | renvoie un tableau indiquant quelles sont les balles alignées avec leur couleur
aligned ∷ Model → Array Boolean
aligned model =
    model^._position # mapWithIndex \index → case _ of
        Nothing → false
        Just c → mod (index + rot) n == c
    where
        n = length $ model^._position
        rot = model^._rotation

-- | comme validRotation mais avec seconde conditition en moins 
validRotation' ∷ Model → Boolean
validRotation' model = count identity (aligned model) == 1

-- | une rotation est valide si exactement une couleur est alignée et il y a une balle pour chaque couleur         
validRotation ∷ Model → Boolean
validRotation model = validRotation' model && all isJust (model^._position)

instance Game Position Ext {from ∷ Location, to ∷ Location} where
    name _ = "roue"

    play model move = act (model^._position) where
        act = case move of 
            {from: Panel from, to: Wheel to} → updateAt to (Just from)
            {from: Wheel from, to: Wheel to } → swap from to
            {from: Wheel from, to: Board} → updateAt from Nothing
            _ → const Nothing
    
    initialPosition model = pure $ replicate (model^._size) Nothing

    isLevelFinished _ = false
    
    onNewGame = pure ∘ set _rotation 0

    computerMove _ = pure Nothing
    sizeLimit = defaultSizeLimit
    updateScore s = defaultUpdateScore s
    onPositionChange = identity
    saveToJson _ = Nothing
    loadFromJson model _ = model

-- | tourne la roue de i crans
rotate ∷ Int → Model → Model
rotate i = _rotation +~ i

data Msg = Core CoreMsg | DnD (DndMsg Location) | Rotate Int | SetSize Int | Check
instance MsgWithCore Msg where core = Core
instance MsgWithDnd Msg Location where dndmsg = DnD  
    
update ∷ Msg → UpdateMam Model Msg Unit
update (Core msg) = coreUpdate msg
update (DnD DropOnBoard) = modify_ \model →
        let model2 = model # set _dragged Nothing in
        case model^._dragged of
            Just (Wheel i) → model2 # set (_position ∘ ix i) Nothing
            _ → model2
update (DnD msg) = dndUpdate _dragged msg
update (Rotate i) = modify_ $ rotate i
update (SetSize i) = newGame $ set _size i
update Check = lockAction $ get >>= \st → tailRecM go (st^._size)
  where
  go 0 = do
        modify_ $ _showWin .~ true
        delay $ Milliseconds 1000.0
        modify_ $ _showWin .~ false
        pure (Done unit)
  go i = do
        st2 ← get
        if not (validRotation st2) then
          pure $ Done unit
        else do
          modify_ $ rotate 1
          delay $ Milliseconds 600.0
          pure $ Loop (i-1)
