module Game.Jetons.Model where

import Prelude
import Data.Tuple (Tuple (Tuple))
import Data.Maybe (fromMaybe)
import Data.Array ((!!), filter, sortWith, modifyAtIndices, all)
import Lib.Core (repeat2)
import Lib.Game (class Game, State (St), canPlay)

data Move = Move Int Int
data NimCls = NimCls

instance nimGame :: Game NimCls (Array (Tuple Int Int)) (nbBases :: Int) Move where
    canPlay (St {position, turn}) (Move pile pos) = fromMaybe false $ do
        Move p q <- position !! pile
        pure $ pos /= p && (if turn == 0 then pos > q else pos < q)

    play (St {position, turn}) (Move pile pos) = position # modifyAtIndices [pile] update
        where update (Tuple p1 p2) = if turn == 0 then Tuple pos p2 else Tuple p1 pos
    
    isLevelFinished (St {position, turn, length}) = position # all test
        where test (Tuple p1 p2) = p2 - p1 == 1 && (if turn then p2 == length - 2 else p1 == 0)

    initialPosition (St {nbPiles}) = 
        sequence $ replicate nbPiles $ do
            x <- randomInt 5
            y <- randomInt 5
            pure $ Tuple x (y + 5)

possibleMoves state@(St {nbPiles, position, length, turn}) =
    repeat2 nbPiles length $ \p -> {pile: p.row, position: p.col}
    # filter \m -> canPlay state m
    # sortWith \move ->
        let Tuple a b = position !! move.pile in
        if turn == 0 then a - move.position else move.position - b

-- isLostPosition (St {position}) = position
--    # map \t -> snd t - snd t - 1
--    # foldl


--    state: {
--        nbPiles: 4,
--        mode: 'expert',  // random | expert | duel
--        turn: 0,   //  0: player 1,  1: player 2
--        length: 10,
--    },

--    actions: $ => ({
--        setNbPiles: $.newGame('nbPiles'),
--        setLimit: $.newGame('limit'),
--        shuffle: $.newGame(),
--    }),

--    computed: state => ({
--        isLevelFinished: isLevelFinished(state),
--    })
--});
