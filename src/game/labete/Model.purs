module Game.Labete.Model where
import MyPrelude
import Lib.Util (coords, tabulate2, abs)
import Pha.Action (Action, setState)
import Game.Common (_isoCustom)
import Game.Effs (EFFS)
import Game.Core (class Game, class ScoreGame, class MsgWithCore, CoreMsg, 
                 SizeLimit(..), GState, Objective(..), ShowWinStrategy(..), PointerPosition, Dialog(..),
               playA, coreUpdate,  _ext, genState, newGame, _position, _nbRows, _nbColumns, _help, _dialog, updateScore')

type Zone = { row1 ∷ Int, row2 ∷ Int, col1 ∷ Int, col2 ∷ Int}

data Mode = StandardMode | CylinderMode | TorusMode
derive instance eqMode ∷ Eq Mode
instance showMode ∷ Show Mode where
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
derive instance eqBt ∷ Eq BeastType
instance showBt ∷ Show BeastType where
    show Type1       = "type1"
    show Type2       = "type2"
    show Type3       = "type3"
    show Type4       = "type4"
    show CustomBeast = "custom"

type Ext' =
    {   beast ∷ Beast'
    ,   beastType ∷ BeastType
    ,   mode ∷ Mode
    ,   selectedColor ∷ Int
    ,   squareColors ∷ Array Int
    ,   startSquare ∷ Maybe Int
    ,   startPointer ∷ Maybe PointerPosition
}
newtype ExtState = Ext Ext'
type State = GState (Array Boolean) ExtState


-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_beast ∷ Lens' State Beast'
_beast = _ext' ∘ lens _.beast _{beast = _}
_beastType ∷ Lens' State BeastType
_beastType = _ext' ∘ lens _.beastType _{beastType = _}
_mode ∷ Lens' State Mode
_mode = _ext' ∘ lens _.mode _{mode = _}
_selectedColor ∷ Lens' State Int
_selectedColor = _ext' ∘ lens _.selectedColor _{selectedColor = _}
_squareColors ∷ Lens' State (Array Int)
_squareColors = _ext' ∘ lens _.squareColors _{squareColors = _}
_startPointer ∷ Lens' State (Maybe PointerPosition)
_startPointer = _ext' ∘ lens _.startPointer _{startPointer = _}
_startSquare ∷ Lens' State (Maybe Int)
_startSquare = _ext' ∘ lens _.startSquare _{startSquare = _}

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
allTranslations n m beast = tabulate2 n m \row col → translate row col beast

-- | renvoie toutes les positions possibles pour une bête à plusieurs formes en prenant
-- | en compte toutes les rotations et translations
-- | peut contenir des positions hors du plateau
allBeastPositions ∷ Int → Int → Beast' → Array Beast
allBeastPositions rows cols = concatMap (allRotations >=> allTranslations rows cols)

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

-- | Renvoie un emplacement possible pour la bête sur le plateau sous forme d'un tableau de booléens indicé par les positions du plateau.
-- | Renvoie un tableau ne contenant que la valeur false si aucun emplacement pour la bête n'est possible
nonTrappedBeastOnGrid ∷ State → Array Boolean
nonTrappedBeastOnGrid st = 
    st # nonTrappedBeasts
    # pseudoRandomPick
    # fromMaybe []
    # foldr (\p → ix (p.row * columns + p.col) .~ true) (replicate (rows * columns) false)
    where rows = st^._nbRows
          columns = st^._nbColumns

getNewBeast ∷ State → Array Beast
getNewBeast state = case state^._beastType of
    Type1 → [type1]
    Type2 → [type2]
    Type3 → [type3]
    Type4 → [type2, type3]
    CustomBeast → take 1 (state^._beast)

zoneposition ∷ Int → Zone → Array Int
zoneposition columns {row1, col1, row2, col2} =
    tabulate2 (abs (row1 - row2) + 1) (abs(col1 - col2) + 1) \i j →
        (i + min row1 row2) * columns + j + (min col1 col2)

colorZone ∷ State → Zone → Array Int
colorZone state zone = state^._squareColors # updateAtIndices ( 
    zoneposition (state^._nbColumns) zone <#> \i → i ∧ (state^._selectedColor)
)

instance labeteGame ∷ Game (Array Boolean) ExtState Int where
    play state index = state^._position # modifyAt index not
    isLevelFinished = null ∘ nonTrappedBeasts
    initialPosition st = pure $ replicate (st^._nbRows * st^._nbColumns) false
    onNewGame st = pure $ st
                        # _beast .~ getNewBeast st
                        # _squareColors .~ replicate (st^._nbRows * st^._nbColumns) 0

    sizeLimit _ = SizeLimit 2 2 9 9
    computerMove _ = pure Nothing
    updateScore = updateScore' ShowWinOnNewRecord

instance scoregameLabete ∷ ScoreGame (Array Boolean) ExtState Int where
    objective _ = Minimize
    scoreFn = length ∘ filter identity ∘ view _position
    scoreHash state = joinWith "-" [show (state^._nbColumns), show (state^._nbRows), show (state^._mode), show (state^._beastType)]
    isCustomGame state = state^._beastType == CustomBeast
          

data Msg = Core CoreMsg | SetMode Mode | SetHelp Boolean | SetBeast BeastType | Play Int | IncSelectedColor Int
         | StartZone Int | StartZone2 { x ∷ Number, y ∷ Number} | FinishZone Int | FlipCustomBeast Int
instance withcore ∷ MsgWithCore Msg where core = Core

update ∷ Msg → Action State EFFS
update (Core msg) = coreUpdate msg
update (SetMode m) = newGame (_mode .~ m)
update (SetHelp a) = setState (_help .~ a)
update (SetBeast ttype) = newGame ( (_beastType .~ ttype) ∘ 
                            (if ttype == CustomBeast then
                                (_dialog .~ CustomDialog) ∘ (_beast %~ take 1)
                            else
                                identity
                            )
                        )
update (IncSelectedColor i) = setState $ _selectedColor %~ \x → (x + i) `mod` 9
-- le début d'une zone est décomposé en deux actions
-- startZoneA est activé lors  du onpointerdown sur l'élément html réprésentant le carré
update (StartZone s) = setState (_startSquare .~ Just s)
-- startZone2A est appliqué lors du onpointerdown sur l'élément html réprésentant le plateu
update (StartZone2 pos) = do
    setState (_startPointer .~ Just pos)
update (FinishZone index1) = setState \state → case state^._startSquare of
    Nothing → state
    Just index2 →
        let {row: row1, col: col1} = coords (state^._nbColumns) index1
            {row: row2, col: col2} = coords (state^._nbColumns) index2
        in state # _squareColors .~ colorZone state {row1, col1, row2, col2}
                 # _startSquare .~ Nothing
                 # _startPointer .~ Nothing
update (FlipCustomBeast i)  = newGame $ _beast ∘ ix 0 ∘ _isoCustom ∘ ix i %~ not
update (Play m) = playA m

onKeyDown ∷ String → Maybe Msg
onKeyDown "o" = Just (IncSelectedColor (-1))
onKeyDown "p" = Just (IncSelectedColor 1)
onKeyDown _ = Nothing
