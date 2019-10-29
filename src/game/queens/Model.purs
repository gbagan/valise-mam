module Game.Queens.Model where

import Prelude
import Data.Array (foldr, mapWithIndex, replicate, zipWith, all)
import Data.Lens (Lens', lens, (^.), (.~), (%~))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Lib.Util (tabulate, dCoords, map2)
import Game.Core (State(..), class Game, SizeLimit(..), genState, _position, _nbRows, _nbColumns)
import Pha.Class (Action(..))
import Pha.Action (action)

-- export const piecesList = ['rook', 'bishop', 'king', 'knight', 'queen'];

data Piece = Rook | Bishop | King | Knight | Queen | Custom | Empty
derive instance eqPiece :: Eq Piece

type Position = Array Piece
type Ext' = {
    selectedPiece :: Piece,
    selectedSquare :: Maybe Int
}
newtype Ext = Ext Ext'
type QueensState = State Position Ext

queensState :: QueensState
queensState = genState [] (_{nbRows = 8, nbColumns = 8}) (Ext {selectedPiece: Queen, selectedSquare: Nothing})

_ext :: Lens' QueensState Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))
_selectedPiece :: Lens' QueensState Piece
_selectedPiece = _ext <<< lens (_.selectedPiece) (_{selectedPiece = _})
_selectedSquare :: Lens' QueensState (Maybe Int)
_selectedSquare = _ext <<< lens (_.selectedSquare) (_{selectedSquare = _})

-- const f9 = repeat(9, false);
-- const f25 = repeat(25, false);

-- teste si une pièce peut se déplacer de x cases horizontalement et de y cases verticalement
legalMoves :: Piece -> Int -> Int -> Boolean
legalMoves Queen x y = (x * x - y * y) * x * y == 0
legalMoves King x y = x * x + y * y <= 2
legalMoves Rook x y = x * y == 0
legalMoves Bishop x y = x * x - y * y == 0
legalMoves Knight x y = x * x + y * y == 5
legalMoves _ _ _ = false

-- teste si la pièce de type "piece" à la position index1 peut attquer la pièce à la position index2
-- suppose que la pièce est différent de Empty
canCapture :: QueensState -> Piece -> Int -> Int -> Boolean
canCapture state piece index1 index2 =
    let {row, col} = dCoords (state^._nbColumns) index1 index2 in
    if piece /= Custom then 
        index1 /= index2 && legalMoves piece row col
    else
        true
            -- (row * row - col * col) * row * col == 0 && customMoves.directions[3 * Math.sign(dRow) + Math.sign(dCol) + 4]
            -- || dRow ** 2 + dCol ** 2 <= 8 && customMoves.local[5 * dRow + dCol + 12]

-- renvoie l'ensemble des positions pouvant être attaqué par une pièce à la position index sous forme de tableau de booléens
attackedBy :: QueensState -> Piece -> Int -> Array Boolean
attackedBy state piece index =
    tabulate (state^._nbRows * state^._nbColumns) (canCapture state piece index)

---arrayOr = zipWith t1 => t2 => t1.map((x, i) => x || t2[i]);

-- renvoie l'ensemble des cases pouvant être attaquées par une pièce sur le plateau
capturableSquares :: QueensState -> Array Boolean
capturableSquares state = state^._position # mapWithIndex Tuple
    # foldr
        (\(Tuple index piece) -> if piece == Empty then identity else zipWith disj (attackedBy state piece index))
        (replicate (state^._nbRows * state^._nbColumns) false)
        

attackedBySelected :: QueensState -> Array Boolean
attackedBySelected state =
     maybe (replicate (state^._nbRows * state^._nbColumns) false)
                            (attackedBy state $ state^._selectedPiece) 
                            (state^._selectedSquare)

     
-- const isCustom = state => state.multiPieces || state.customSize || state.allowedPieces.includes('custom');
{-

const toggleAllowedPiece = (state, piece) =>
    state.allowedPieces.length === 1 && state.allowedPieces[0] === piece &&
        state.allowedPieces
    || state.multiPieces &&
        piecesList.filter(p2 => (p2 === piece) !== state.allowedPieces.includes(p2))
    || [piece];
-}

instance pathGame :: Game (Array Piece) Ext Int where 
    canPlay _ _ = true

    play state index = 
        let selectedPiece = state^._selectedPiece in
        state^._position # ix index %~ \t -> if t == selectedPiece then Empty else selectedPiece

    initialPosition state = pure $ replicate (state^._nbRows * state^._nbColumns) Empty

    isLevelFinished state = all identity $ map2 (capturableSquares state) (state^._position)
                            \index captured piece -> not captured || piece == Empty
    
    onNewGame state = pure $ state # _selectedPiece .~ Queen
      -- selectedPiece: state.allowedPieces[0]}),
    
    sizeLimit _ = SizeLimit 3 3 9 9

    computerMove _ = Nothing

    {-
      score: {
            objective: 'maximize',
            function: state => state.position |> countBy(identity),
            params: attrs('columns,rows,allowedPieces'),
        }
    },

    state: {
        columns: 8,
        rows: 8,
        allowedPieces: ['rook'],
        help: false,
        customMoves: { local: f25, directions: f9 },
        multiPieces: false,
        selectedSquare: null,
    },

    actions: $ => ({
        selectAllowedPiece: $.newGame(piece => state => ({...state, allowedPieces: toggleAllowedPiece(state, piece)})),
        selectPieceType: update('selectedPiece'),
        selectSquare: update('selectedSquare'),
        flipDirection: $.newGame(direction => set(['customMoves', 'directions', direction], not)),
        flipLocal: $.newGame(position => set(['customMoves', 'local', position], not)),
        customize: $.newGame({allowedPieces: ['custom'], dialog: 'custompiece', multiPieces: false}),

        toggleMultiPieces: $.newGame(() => state =>
            state 
                |> set('allowedPieces', x => x[0] === 'custom' ? ['queen'] : state.multiPieces ? [x[0]] : x)
                |> set('multiPieces', not)
        ),
    }),

    computed: state => ({
        attackedSquares: capturableSquares(state),
        attackedBySelected: attackedBySelected(state),
    })
});
-}
selectSquareA :: Maybe Int -> Action QueensState
selectSquareA a = action $ _selectedSquare .~ a