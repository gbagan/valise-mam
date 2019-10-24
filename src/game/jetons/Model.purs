module Game.Jetons.Model where

import Prelude
import Data.Array (length, replicate, (!!), (..), updateAt, modifyAt, all)
import Data.Maybe (Maybe (..), fromMaybe)
import Optic.Core ((^.))
import Lib.Core (dCoords)
import Lib.Game (class Game, State, _position, _nbColumns, _nbRows)


-- export default template({
--     state: {
--        rows: 4,
--        columns: 4

--        sizeLimit: [1, 2, 6, 12],

type Position = Array Int
data Move = Move Int Int
data Ext = Ext
type JetonState = State Position Unit

instance jetonsGame :: Game (Array Int) Ext Move where
    play state (Move from to) =
        let position = state^._position in
        fromMaybe position $ do
            pfrom <- position !! from
            position # updateAt from 0 >>= modifyAt to (add pfrom)
    
    canPlay state (Move from to) =
        let position = state^._position
            columns = state^._nbColumns
        in
        fromMaybe false $ do
            let {row, col} = dCoords columns from to
            pfrom <- position !! from
            pto <- position !! to
            pure $ pfrom > 0 && pfrom <= pto && row * row + col * col == 1
    
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
    
    computerMove _ = Nothing

    -- objective = "minimize",
    --function = nbNonEmptyCells
    -- params = attrs('columns,rows')
    -- isCustomLevel _ = false

-- nbNonEmptyCells St{position} = position # filter (_ > 0) # length
