module Game.Queens.Model where
import MyPrelude
import Lib.Util (tabulate, dCoords, map2)
import Data.Array.NonEmpty (NonEmptyArray, fromArray, head, singleton) as N
import Game.Core (GState(..), class Game, Dialog(..), SizeLimit(..), genState, newGame, _dialog, _position, _nbRows, _nbColumns, playA')
import Pha.Action (Action, action, RNG, DELAY)

piecesList :: Array Piece
piecesList = [Rook, Bishop, King, Knight, Queen]

data Piece = Rook | Bishop | King | Knight | Queen | Custom | Empty
derive instance eqPiece :: Eq Piece
instance showPiece :: Show Piece where 
    show Queen = "queen"
    show King = "king" 
    show Rook = "rook" 
    show Bishop = "bishop"
    show Knight = "knight"
    show _ = "custom"

type Position = Array Piece
type Ext' = {
    selectedPiece :: Piece,
    selectedSquare :: Maybe Int,
    allowedPieces :: N.NonEmptyArray Piece,
    multiPieces :: Boolean,
    customLocalMoves :: Array Boolean,
    customDirections :: Array Boolean
}
newtype Ext = Ext Ext'
type State = GState Position Ext

istate :: State
istate = genState []
    (_{nbRows = 8, nbColumns = 8})
    (Ext {selectedPiece: Queen, selectedSquare: Nothing, allowedPieces: N.singleton Rook, multiPieces: false,
        customLocalMoves: replicate 25 false, customDirections: replicate 9 false
    })

_ext :: Lens' State Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))
_selectedPiece :: Lens' State Piece
_selectedPiece = _ext ∘ lens (_.selectedPiece) (_{selectedPiece = _})
_selectedSquare :: Lens' State (Maybe Int)
_selectedSquare = _ext ∘ lens (_.selectedSquare) (_{selectedSquare = _})
_allowedPieces :: Lens' State (N.NonEmptyArray Piece)
_allowedPieces = _ext ∘ lens (_.allowedPieces) (_{allowedPieces = _})
_multiPieces :: Lens' State Boolean
_multiPieces = _ext ∘ lens (_.multiPieces) (_{multiPieces = _})
_customLocalMoves :: Lens' State (Array Boolean)
_customLocalMoves = _ext ∘ lens (_.customLocalMoves) (_{customLocalMoves = _})
_customDirections :: Lens' State (Array Boolean)
_customDirections = _ext ∘ lens (_.customDirections) (_{customDirections = _})

-- teste si une pièce peut se déplacer de x cases horizontalement et de y cases verticalement
legalMoves :: Piece -> Int -> Int -> Boolean
legalMoves Queen x y = (x * x - y * y) * x * y == 0
legalMoves King x y = x * x + y * y <= 2
legalMoves Rook x y = x * y == 0
legalMoves Bishop x y = x * x - y * y == 0
legalMoves Knight x y = x * x + y * y == 5
legalMoves _ _ _ = false

sign :: Int -> Int
sign 0 = 0
sign x = if x > 0 then 1 else -1

-- teste si la pièce de type "piece" à la position index1 peut attquer la pièce à la position index2
-- suppose que la pièce est différent de Empty
canCapture :: State -> Piece -> Int -> Int -> Boolean
canCapture state piece index1 index2 =
    let {row, col} = dCoords (state^._nbColumns) index2 index1 in
    if piece /= Custom then 
        index1 /= index2 && legalMoves piece row col
    else
        (row * row - col * col) * row * col == 0 && (state^._customDirections) !! (3 * sign row + sign col + 4) == Just true
        || row * row + col * col <= 8 && (state^._customLocalMoves) !! (5 * row + col + 12) == Just true

-- renvoie l'ensemble des positions pouvant être attaqué par une pièce à la position index sous forme de tableau de booléens
attackedBy :: State -> Piece -> Int -> Array Boolean
attackedBy state piece index =
    tabulate (state^._nbRows * state^._nbColumns) (canCapture state piece index)

-- renvoie l'ensemble des cases pouvant être attaquées par une pièce sur le plateau
capturableSquares :: State -> Array Boolean
capturableSquares state = state^._position # mapWithIndex Tuple
    # foldr
        (\(index ~ piece) -> if piece == Empty then identity else zipWith disj (attackedBy state piece index))
        (replicate (state^._nbRows * state^._nbColumns) false)
        

attackedBySelected :: State -> Array Boolean
attackedBySelected state =
     maybe (replicate (state^._nbRows * state^._nbColumns) false)
                            (attackedBy state $ state^._selectedPiece) 
                            (state^._selectedSquare)

     
-- const isCustom = state => state.multiPieces || state.customSize || state.allowedPieces.includes('custom');

instance pathGame :: Game (Array Piece) Ext Int where 
    canPlay _ _ = true

    play state index = 
        let selectedPiece = state^._selectedPiece
        in state^._position # ix index %~ \t -> if t == selectedPiece then Empty else selectedPiece

    initialPosition state = pure $ replicate (state^._nbRows * state^._nbColumns) Empty

    isLevelFinished state = and $ map2 (capturableSquares state) (state^._position)
                            \_ captured piece -> not captured || piece == Empty
    
    onNewGame state = pure $ state # _selectedPiece .~ N.head (state^._allowedPieces)
    sizeLimit _ = SizeLimit 3 3 9 9
    computerMove _ = Nothing

    {-
      score: {
            objective: 'maximize',
            function: state => state.position |> countBy(identity),
            params: attrs('columns,rows,allowedPieces'),
        }
    }, (3，2)
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
        selectSquare: update('selectedSquare'),

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

playA :: ∀effs. Int -> Action State (rng :: RNG, delay :: DELAY | effs)
playA = playA' (_{showWin = false})

toggleAllowedPiece :: Piece ->  Boolean -> N.NonEmptyArray Piece -> N.NonEmptyArray Piece
toggleAllowedPiece piece false pieces = N.singleton piece
toggleAllowedPiece piece true pieces = N.fromArray pieces2 # fromMaybe pieces where
    pieces2 = piecesList # filter \p2 -> (p2 == piece) /= elem p2 pieces
    
selectPieceA :: ∀effs. Piece -> Action State effs
selectPieceA piece = action $ _selectedPiece .~ piece

selectSquareA :: ∀effs. Maybe Int -> Action State effs
selectSquareA a = action $ _selectedSquare .~ a

selectAllowedPieceA :: ∀effs. Piece -> Action State (rng :: RNG, delay :: DELAY | effs)
selectAllowedPieceA piece = newGame $ \state -> state # _allowedPieces %~ toggleAllowedPiece piece (state^._multiPieces)

toggleMultiPiecesA :: ∀effs. Action State effs
toggleMultiPiecesA = action $ _multiPieces %~ not

flipDirectionA :: ∀effs. Int -> Action State (rng :: RNG | effs)
flipDirectionA direction = newGame $ (_customDirections ∘ ix direction) %~ not

flipLocalMoveA :: ∀effs. Int -> Action State (rng :: RNG | effs)
flipLocalMoveA position = newGame $ (_customLocalMoves ∘ ix position) %~ not

customizeA :: ∀effs. Action State (rng :: RNG | effs)
customizeA = newGame $ (_allowedPieces .~ N.singleton Custom) ∘ (_dialog .~ CustomDialog) ∘ (_multiPieces .~ false)