module Game.Queens.Model where
import MyPrelude
import Data.FoldableWithIndex (foldrWithIndex)
import Lib.Util (dCoords, map2)
import Data.Array.NonEmpty as N
import Game.Core (GState, class MsgWithCore, class Game, class ScoreGame, 
                 CoreMsg, Objective(..), Dialog(..), SizeLimit(..), ShowWinPolicy(..),
                coreUpdate, playA, genState, newGame,  updateScore',
                _ext, _dialog, _position, _nbRows, _nbColumns)
import Lib.Update (Update)

piecesList ∷ Array Piece
piecesList = [Rook, Bishop, King, Knight, Queen]

data Piece = Rook | Bishop | King | Knight | Queen | Custom | Empty
derive instance eqPiece ∷ Eq Piece
instance showPiece ∷ Show Piece where 
    show Queen = "queen"
    show King = "king" 
    show Rook = "rook" 
    show Bishop = "bishop"
    show Knight = "knight"
    show _ = "custom"

type Position = Array Piece
type Ext' =
    {   selectedPiece ∷ Piece                 -- la pièce actuellement choisie par l'utilisateur
    ,   selectedSquare ∷ Maybe Int            -- la case sur laquelle pointe le pointeur de l'utilisateur
    ,   allowedPieces ∷ NonEmptyArray Piece -- la liste des pièces que l'utilisateur a le droit d'utiliser
    ,   multiPieces ∷ Boolean                 -- l'utilisateur peut-il utiliser plusieurs pièces différentes ou une seule
    ,   customLocalMoves ∷ Array Boolean      -- la liste des mouvements locaux autorisées pour la pièce personnalisée
    ,   customDirections ∷ Array Boolean      -- la liste des directions autorisées pour la pièce personnalisée
    }
newtype Ext = Ext Ext'
type State = GState Position Ext

-- état initial
istate ∷ State
istate = genState []
    _{  nbRows = 8
    ,   nbColumns = 8
    }
    (Ext 
        {   selectedPiece: Queen
        ,   selectedSquare: Nothing
        ,   allowedPieces: N.singleton Rook
        ,   multiPieces: false
        ,   customLocalMoves: replicate 25 false
        ,   customDirections: replicate 9 false
        }
    )

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_selectedPiece ∷ Lens' State Piece
_selectedPiece = _ext' ∘ prop (Proxy ∷ _ "selectedPiece")
_selectedSquare ∷ Lens' State (Maybe Int)
_selectedSquare = _ext' ∘ prop (Proxy ∷ _ "selectedSquare")
_allowedPieces ∷ Lens' State (NonEmptyArray Piece)
_allowedPieces = _ext' ∘ prop (Proxy ∷ _ "allowedPieces")
_multiPieces ∷ Lens' State Boolean
_multiPieces = _ext' ∘ prop (Proxy ∷ _ "multiPieces")
_customLocalMoves ∷ Lens' State (Array Boolean)
_customLocalMoves = _ext' ∘ prop (Proxy ∷ _ "customLocalMoves")
_customDirections ∷ Lens' State (Array Boolean)
_customDirections = _ext' ∘ prop (Proxy ∷ _ "customDirections")

-- | teste si une pièce peut se déplacer d'un vecteur (x, y)
legalMoves ∷ Piece → Int → Int → Boolean
legalMoves Queen x y = (x * x - y * y) * x * y == 0
legalMoves King x y = x * x + y * y <= 2
legalMoves Rook x y = x * y == 0
legalMoves Bishop x y = x * x - y * y == 0
legalMoves Knight x y = x * x + y * y == 5
legalMoves _ _ _ = false

-- contrairement à Data.Ord.signum, sign 0 = 0
sign ∷ Int → Int
sign 0 = 0
sign x | x > 0 = 1
       | otherwise = -1

-- | teste si la pièce de type "piece" à la position index1 peut attaquer la pièce à la position index2
-- | suppose que la pièce est différent de Empty
canCapture ∷ State → Piece → Int → Int → Boolean
canCapture state piece index1 index2 =
    let {row, col} = dCoords (state^._nbColumns) index2 index1 in
    if piece ≠ Custom then 
        index1 ≠ index2 && legalMoves piece row col
    else
        (row * row - col * col) * row * col == 0 && (state^._customDirections) !! (3 * sign row + sign col + 4) == Just true
        || row * row + col * col <= 8 && (state^._customLocalMoves) !! (5 * row + col + 12) == Just true

-- | renvoie l'ensemble des positions pouvant être attaquées par une pièce à la position index sous forme de tableau de booléens
attackedBy ∷ State → Piece → Int → Array Boolean
attackedBy state piece index =
    repeat (state^._nbRows * state^._nbColumns) (canCapture state piece index)

-- | renvoie l'ensemble des cases pouvant être attaquées par une pièce sur le plateau
capturableSquares ∷ State → Array Boolean
capturableSquares state = state^._position # foldrWithIndex
        (\index piece → if piece == Empty then identity else zipWith disj (attackedBy state piece index))
        (replicate (state^._nbRows * state^._nbColumns) false)

-- | renvoie l'ensemble des cases attaquées par l'endroit du pointeur de la souris
attackedBySelected ∷ State → Array Boolean
attackedBySelected state = case state^._selectedSquare of
    Nothing → replicate (state^._nbRows * state^._nbColumns) false
    Just index → attackedBy state (state^._selectedPiece) index 

-- | ajoute ou enlève une pièce à la liste des pièces autorisées.
-- | Le paramètre booléen correspond à la présence du mode multipiece
toggleAllowedPiece ∷ Piece → Boolean → NonEmptyArray Piece → NonEmptyArray Piece
toggleAllowedPiece piece false _ = N.singleton piece
toggleAllowedPiece piece true pieces = N.fromArray pieces2 # fromMaybe pieces where
    pieces2 = piecesList # filter \p2 → (p2 == piece) ≠ elem p2 pieces

instance queensGame ∷ Game Position Ext Int where 
    name _ = "queens"

    play state index = 
        let selectedPiece = state^._selectedPiece
        in state^._position # modifyAt index \t → if t == selectedPiece then Empty else selectedPiece

    initialPosition state = pure $ replicate (state^._nbRows * state^._nbColumns) Empty

    -- le nom de la fonction est trompeur ici, la partie n'est jamais finie
    -- indique si il n'y a pas de pièce posée qui en capture une autre
    isLevelFinished state = and $ map2 (capturableSquares state) (state^._position)
                            \_ captured piece → not captured || piece == Empty
    
    onNewGame state = pure $ state # set _selectedPiece (N.head $ state^._allowedPieces)
    sizeLimit _ = SizeLimit 3 3 9 9
    updateScore = updateScore' NeverShowWin
    
    -- methodes par défaut
    computerMove _ = pure Nothing
    onPositionChange = identity
    saveToJson _ = Nothing
    loadFromJson st _ = st

instance queensScoreGame ∷ ScoreGame (Array Piece) Ext Int where 
    objective _ = Maximize
    scoreFn = length ∘ filter (_ ≠ Empty) ∘ view _position
    scoreHash state = joinWith "-" [show (state^._nbRows), show (state^._nbColumns), show (N.head $ state^._allowedPieces)]
    isCustomGame state = state^._multiPieces || N.head (state^._allowedPieces) == Custom

data Msg = Core CoreMsg | Play Int | SelectPiece Piece | SelectSquare (Maybe Int)
        | SelectAllowedPiece Piece | ToggleMultiPieces | FlipDirection Int | FlipLocalMove Int | Customize
instance withcore ∷ MsgWithCore Msg where core = Core
        
update ∷ Msg → Update State Unit
update (Core msg) = coreUpdate msg
update (Play i) = playA i
update (SelectPiece piece) = _selectedPiece .= piece
update (SelectSquare a) = _selectedSquare .= a
update (SelectAllowedPiece piece) = newGame $ \state → state # over _allowedPieces (toggleAllowedPiece piece (state^._multiPieces))
update ToggleMultiPieces = _multiPieces %= not
update (FlipDirection direction) = newGame $ over (_customDirections ∘ ix direction) not
update (FlipLocalMove position) = newGame $ over (_customLocalMoves ∘ ix position) not
update Customize = newGame $ 
                        set _allowedPieces (N.singleton Custom)
                        ∘ set _dialog CustomDialog
                        ∘ set _multiPieces false