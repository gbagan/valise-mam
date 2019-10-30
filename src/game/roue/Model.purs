module Game.Roue.Model where

import Prelude
import Data.Array (replicate, mapWithIndex, all, filter, length)
import Data.Lens (Lens', lens, set, (^.), (.~), (%~))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Lib.Util (swap)
import Game.Core (class Game, State(..), genState, newGame', lockAction, _position, _showWin, defaultSizeLimit)
import Pha.Action (Action(..), action)

type Position = Array (Maybe Int)

data Ball = Panel Int | Wheel Int | Board
derive instance eqBall :: Eq Ball

type Ext' = {size :: Int, rotation :: Int, dragged :: Maybe Ball}
newtype Ext = Ext Ext'
type RoueState = State Position Ext

roueState :: RoueState
roueState = genState [] identity (Ext {rotation: 0, size: 5, dragged: Nothing})

_ext :: Lens' RoueState Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))
_rotation :: Lens' RoueState Int
_rotation = _ext <<< lens (_.rotation) (_{rotation = _})
_size :: Lens' RoueState Int
_size = _ext <<< lens (_.size) (_{size = _})
_dragged :: Lens' RoueState (Maybe Ball)
_dragged = _ext <<< lens (_.dragged) (_{dragged = _})

-- renvoie un tableau indiquant quelles sont les balles alignées avec leur couleur
aligned :: RoueState -> Array Boolean
aligned state =
    state^._position # mapWithIndex (\index -> maybe false $ \c -> c == (index + rot) `mod` n)
    where
        n = length $ state^._position
        rot = state^._rotation

validRotation' :: RoueState -> Boolean
validRotation' state = (length $ filter identity $ aligned state) == 1

-- une rotation est valide si exactement une couleur est alignée et il y a une balle pour chque couleur         
validRotation :: RoueState -> Boolean
validRotation state = validRotation' state && (all isJust $ state^._position )

-- tourne la roue de i crans
rotate :: Int -> RoueState -> RoueState
rotate i = _rotation %~ add i

rotateA :: Int -> Action RoueState
rotateA i = action $ rotate i

setSizeA :: Int -> Action RoueState
setSizeA = newGame' (set _size)

checkA :: Action RoueState
checkA = lockAction $ Action \setState ev state -> aux setState (state^._size) state where
    aux setS 0 st2 = do
        _ <- liftEffect $ setS $ st2 # _showWin .~ true
        delay $ Milliseconds 1000.0
        liftEffect $ setS $ st2
    aux setS i st2 =
        if not (validRotation st2) then
            pure st2
        else do
            st3 <- liftEffect $ setS $ rotate 1 st2
            delay $ Milliseconds 600.0
            aux setS (i-1) st3


instance roueGame :: Game (Array (Maybe Int)) Ext {from :: Ball, to :: Ball} where
    play state move = act $ state^._position where
        act = case move of 
            {from: Panel from, to: Wheel to} -> ix to .~ Just from
            {from: Wheel from, to: Wheel to } -> swap from to
            {from: Wheel from, to: Board} -> ix from .~ Nothing
            _ -> identity
    
    canPlay _ _ = true
    -- {from: Wheel _} = true
    --canPlay _ {from: Move _ (Id _)) = true
    --canPlay _ _ = false
    
    initialPosition state = pure $ replicate (state^._size) Nothing

    isLevelFinished _ = false
    
    onNewGame = pure <<< (_rotation .~ 0)

    computerMove _ = Nothing
    sizeLimit = defaultSizeLimit


--    computed: state => ({
--        aligned: aligned(state),
--        validRotation: validRotation(state),
--    })
-- });
