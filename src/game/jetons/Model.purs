module Game.Jetons.Model where

import MyPrelude
import Lib.Util ((..), dCoords)
import Game.Core (class Game, class ScoreGame, GState(..), SizeLimit (..), Objective(..), ShowWinStrategy(..),
                  genState, updateScore', _position, _nbColumns, _nbRows, defaultOnNewGame)

type Position = Array Int
type Ext' = { dragged :: Maybe Int }
newtype Ext = Ext Ext'
type State = GState Position Ext

istate :: State
istate = genState [] (_{nbRows = 4, nbColumns = 4}) (Ext { dragged: Nothing })

_ext :: Lens' State Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))
_dragged :: Lens' State (Maybe Int)
_dragged = _ext ∘ lens (_.dragged) (_{dragged = _})

instance jetonsGame :: Game (Array Int) Ext { from :: Int, to :: Int } where
    play state {from, to} =
        let position = state^._position in
        fromMaybe position $ do
            pfrom <- position !! from
            position # updateAt from 0 >>= modifyAt to (add pfrom)
    
    canPlay state {from, to} =
        let position = state^._position
            {row, col} = dCoords (state^._nbColumns) from to in
        fromMaybe false $
            (\pfrom pto -> pfrom > 0 && pfrom <= pto && row * row + col * col == 1)
            <$> position !! from <*> position !! to
    
    initialPosition state = pure $ replicate (state^._nbRows * state^._nbColumns) 1

    isLevelFinished state =
        let position = state^._position
            columns = state^._nbColumns
        in
        (0 .. (length position - 1)) # all \i ->
            let x = fromMaybe 0 $ position !! i
                y = if (i+1) `mod` columns == 0 then 0 else fromMaybe 0 $ position !! (i+1)
                z = fromMaybe 0 $ position !! (i+columns) in
            x * (y + z) == 0

    sizeLimit _ = SizeLimit 1 2 6 12

    computerMove _ = Nothing
    onNewGame = defaultOnNewGame
    updateScore = updateScore' AlwaysShowWin

instance scoregame :: ScoreGame (Array Int) Ext { from :: Int, to :: Int } where
    objective state = Minimize
    scoreFn = length ∘ filter (_ > 0) ∘ view _position
    scoreHash state = show (state^._nbRows) <> "-" <> show (state^._nbColumns)
    isCustomGame _ = false

