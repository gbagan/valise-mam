module Game.Labete.Model where

import MyPrelude
import Game.Common (pointerDecoder, _isoCustom)
import Game.Core (class Game, class ScoreGame, class MsgWithCore, CoreMsg, SizeLimit(..), GState, Objective(..), ShowWinPolicy(..), PointerPosition, Dialog(..),
                    playA, coreUpdate, _ext, genState, newGame, _position, _nbRows, _nbColumns, _help, _dialog, updateScore', saveToJson', loadFromJson')
import Lib.Update (UpdateMam)
import Lib.Util (coords, repeat2)
import Web.PointerEvent (PointerEvent)
import Web.PointerEvent.PointerEvent as PE
import Web.UIEvent.MouseEvent as ME

type Zone = { row1 ∷ Int, row2 ∷ Int, col1 ∷ Int, col2 ∷ Int}

data Mode = StandardMode | CylinderMode | TorusMode
derive instance Eq Mode
instance Show Mode where
    show StandardMode = "standard"
    show CylinderMode = "cylinder"
    show TorusMode    = "torus"

-- | le type Beast représente la forme d'une bête bête par l'ensemble de positions
type Beast = Array {row ∷ Int, col ∷ Int}
-- | le type Beast' représente une bête pouvant avoir plusieurs formes
type Beast' = Array Beast
type1 ∷ Beast
type1 = [{ row: 0, col: 0 }, { row: 0, col: 1 }]
type2 ∷ Beast
type2 = [{ row: 0, col: 0 }, { row: 0, col: 1 }, { row: 0, col: -1 }]
type3 ∷ Beast
type3 = [{ row: 0, col: 0 }, { row: 0, col: 1 }, { row: 1, col: 1 }]
beastTypes ∷ Array Beast'
beastTypes = [[type1], [type2], [type3], [type2, type3]]

data BeastType = Type1 | Type2 | Type3 | Type4 | CustomBeast
derive instance Eq BeastType
instance Show BeastType where
    show Type1       = "type1"
    show Type2       = "type2"
    show Type3       = "type3"
    show Type4       = "type4"
    show CustomBeast = "custom"

type Ext' =
    {   beast ∷ Beast'
    ,   beastType ∷ BeastType
    ,   mode ∷ Mode                          -- forme de la grille (normale/cylindrique/torique)
    ,   selectedColor ∷ Int                  -- couleur actuellement choisi avec les touches O/P
    ,   squareColors ∷ Array Int             -- couleur de chaque case
    ,   startSquare ∷ Maybe Int              -- case de départ lorsque l'on sélectionne une zone à colorier
    ,   startPointer ∷ Maybe PointerPosition -- position de départ lorsque l'on sélectionne une zone à colorier
}
newtype ExtState = Ext Ext'
type State = GState (Array Boolean) ExtState

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_beast ∷ Lens' State Beast'
_beast = _ext' ∘ prop (Proxy ∷ _ "beast")
_beastType ∷ Lens' State BeastType
_beastType = _ext' ∘ prop (Proxy ∷ _ "beastType")
_mode ∷ Lens' State Mode
_mode = _ext' ∘ prop (Proxy ∷ _ "mode")
_selectedColor ∷ Lens' State Int
_selectedColor = _ext' ∘ prop (Proxy ∷ _ "selectedColor")
_squareColors ∷ Lens' State (Array Int)
_squareColors = _ext' ∘ prop (Proxy ∷ _ "squareColors")
_startPointer ∷ Lens' State (Maybe PointerPosition)
_startPointer = _ext' ∘ prop (Proxy ∷ _ "startPointer")
_startSquare ∷ Lens' State (Maybe Int)
_startSquare = _ext' ∘ prop (Proxy ∷ _ "startSquare")

-- | état initial
istate ∷ State
istate = genState [] _{nbRows = 5, nbColumns = 5} 
                (Ext {
                    beast: [type1],
                    beastType: Type1,
                    mode: StandardMode, 
                    startSquare: Nothing,
                    startPointer: Nothing,
                    squareColors: [],
                    selectedColor: 0
                })

rotate90 ∷ Beast → Beast
rotate90 = map \{row, col} → { row: -col, col: row }

translate ∷ Int → Int → Beast → Beast
translate row' col' = map \{row, col} → { row: row + row', col: col + col' }
        
allRotations ∷ Beast → Array Beast
allRotations beast = [beast, beast2, beast3, beast4] where
    beast2 = rotate90 beast
    beast3 = rotate90 beast2
    beast4 = rotate90 beast3

allTranslations ∷ Int → Int → Beast → Array Beast
allTranslations n m beast = repeat2 n m \row col → translate row col beast

-- | renvoie toutes les positions possibles pour une bête à plusieurs formes en prenant
-- | en compte toutes les rotations et translations
-- | peut contenir des positions hors du plateau
allBeastPositions ∷ Int → Int → Beast' → Array Beast
allBeastPositions rows cols = (_ >>= (allRotations >=> allTranslations rows cols))

adaptatedBeast ∷ Int → Int → Mode → Beast → Beast
adaptatedBeast rows columns mode =
    map \{row, col} → case mode of
                        StandardMode → {row, col}
                        CylinderMode → {row, col: col `mod` columns}
                        TorusMode → {row: row `mod` rows, col: col `mod` columns}

-- | Fonction auxiliaire pour nonTrappedBeastOnGrid.
-- | Il n'est pas nécessaire d'avoir une vraie fonction aléatoire
pseudoRandomPick ∷ ∀t. Array t → Maybe t
pseudoRandomPick t = t !! (28921 `mod` length t)

-- | Renvoie tous les emplacement possibles évitants les pièges pour la bête
nonTrappedBeasts ∷ State → Array Beast
nonTrappedBeasts state =
    allBeastPositions rows columns (state^._beast)
        <#> adaptatedBeast rows columns (state^._mode)
        # filter isValidBeast
    where rows = state^._nbRows
          columns = state^._nbColumns
          isValidBeast = all \{row, col} → row >= 0 && row < rows && col >= 0 && col < columns && 
                    (state^._position) !! (row * columns + col) == Just false

-- | Renvoie un emplacement possible pour la bête sur le plateau sous forme d'un tableau de booléens
-- | indicé par les positions du plateau.
-- | Renvoie un tableau ne contenant que la valeur false si aucun emplacement pour la bête n'est possible
nonTrappedBeastOnGrid ∷ State → Array Boolean
nonTrappedBeastOnGrid st = 
    st # nonTrappedBeasts
    # pseudoRandomPick
    # fromMaybe []
    # foldr (\p → set (ix $ p.row * columns + p.col) true) (replicate (rows * columns) false)
    where rows = st^._nbRows
          columns = st^._nbColumns

getNewBeast ∷ State → Array Beast
getNewBeast state = case state ^. _beastType of
    Type1 → [type1]
    Type2 → [type2]
    Type3 → [type3]
    Type4 → [type2, type3]
    CustomBeast → take 1 (state^._beast)

zoneposition ∷ Int → Zone → Array Int
zoneposition columns {row1, col1, row2, col2} =
    repeat2 (abs (row1 - row2) + 1) (abs(col1 - col2) + 1) \i j →
        (i + min row1 row2) * columns + j + (min col1 col2)

colorZone ∷ State → Zone → Array Int
colorZone state zone = state^._squareColors # updateAtIndices ( 
    zoneposition (state^._nbColumns) zone <#> \i → i ∧ (state^._selectedColor)
)

instance Game (Array Boolean) ExtState Int where
    name _ = "labete"
    play state index = state^._position # modifyAt index not
    isLevelFinished = null <<< nonTrappedBeasts
    initialPosition st = pure $ replicate (st^._nbRows * st^._nbColumns) false
    onNewGame st = pure $ st
                        # set _beast (getNewBeast st)
                        # set _squareColors (replicate (st^._nbRows * st^._nbColumns) 0)

    sizeLimit _ = SizeLimit 2 2 9 9

    updateScore = updateScore' {onlyWhenFinished: true, showWin: ShowWinOnNewRecord}
    saveToJson = saveToJson'
    loadFromJson = loadFromJson'

    -- méthodes par défault
    computerMove _ = pure Nothing
    onPositionChange = identity

instance ScoreGame (Array Boolean) ExtState Int where
    objective _ = Minimize
    scoreFn = length ∘ filter identity ∘ view _position
    scoreHash state = joinWith "-"
                        [ show (state^._nbColumns)
                        , show (state^._nbRows)
                        , show (state^._mode)
                        , show (state^._beastType)
                        ]
    isCustomGame state = state^._beastType == CustomBeast
          

data Msg = 
      Core CoreMsg 
    | SetMode Mode 
    | SetHelp Boolean
    | SetBeast BeastType
    | Play Int
    | IncSelectedColor Int
    | StartZone Int 
    | StartZone2 PointerEvent
    | FinishZone Int
    | FlipCustomBeast Int
    | NoAction
instance MsgWithCore Msg where core = Core

update ∷ Msg → UpdateMam State
update (Core msg) = coreUpdate msg
update (SetMode m) = newGame $ set _mode m
update (SetHelp a) = _help .= a
update (SetBeast ttype) = newGame $ 
                            set _beastType ttype >>> 
                            if ttype == CustomBeast then
                                set _dialog CustomDialog >>> over _beast (take 1)
                            else
                                identity
update (IncSelectedColor i) = _selectedColor %= \x → (x + i) `mod` 9
-- le début d'une zone est décomposé en deux actions
-- startZoneA est activé lors  du onpointerdown sur l'élément html réprésentant le carré
update (StartZone s) = _startSquare .= Just s
-- startZone2A est appliqué lors du onpointerdown sur l'élément html réprésentant le plateu
update (StartZone2 ev) =
    if ME.shiftKey (PE.toMouseEvent ev) then do
        p ← liftEffect $ pointerDecoder ev
        case p of
            Just pos → _startPointer .= Just pos
            Nothing → pure unit
    else
        pure unit

update (FinishZone index1) = modify_ \state → case state^._startSquare of
    Nothing → state
    Just index2 →
        let {row: row1, col: col1} = coords (state^._nbColumns) index1
            {row: row2, col: col2} = coords (state^._nbColumns) index2
        in state # set _squareColors (colorZone state {row1, col1, row2, col2})
                 # set _startSquare Nothing
                 # set _startPointer Nothing
update (FlipCustomBeast i) = newGame $ over (_beast ∘ ix 0 ∘ _isoCustom ∘ ix i) not
update (Play index) = playA index
update NoAction = pure unit

onKeyDown ∷ String → Maybe Msg
onKeyDown "o" = Just (IncSelectedColor (-1))
onKeyDown "p" = Just (IncSelectedColor 1)
onKeyDown _ = Nothing
