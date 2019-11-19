module Game.Paths.Model where
import MyPrelude
import Data.Array (nub)
import Data.Array.NonEmpty (fromArray, head, last, init, tail) as N
import Lib.Random (randomInt)
import Lib.Util (dCoords, rangeStep)
import Game.Core (GState, class Game, SizeLimit(..), _ext, newGame', genState, _nbRows, _nbColumns, _position, playA)
import Pha.Action (Action, DELAY, RNG, getState, setState)

data Mode = Mode1 | Mode2
derive instance eqMode :: Eq Mode
instance showMode :: Show Mode where show _ = "mode"

type Position = Array Int
type Ext' = { exit :: Maybe Int, mode :: Mode }
newtype Ext = Ext Ext'
type State = GState Position Ext

istate :: State
istate = genState [] _{nbRows = 4, nbColumns = 6} (Ext { exit: Nothing, mode: Mode1 })

_ext' :: Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) -> a) Ext
_exit :: Lens' State (Maybe Int)
_exit = _ext' ∘ lens _.exit _{exit = _}
_mode :: Lens' State Mode
_mode = _ext' ∘ lens _.mode _{mode = _}

-- renvoie un chemin horizontal ou vertical entre u et v si celui ci existe (u exclus)
pathBetween :: Int -> Int -> Int -> Maybe (Array Int)
pathBetween columns u v =
    let {row, col} = dCoords columns u v in
    if row == 0 then
        Just (if u < v then rangeStep (u + 1) v 1 else rangeStep (u - 1) v (-1))
    else if col == 0 then
        Just (if u < v then rangeStep (u + columns) v columns else rangeStep (u - columns) v (-columns))
    else
        Nothing

-- teste si un chemin est valide (sans répétition de sommets sauf les extrémités si cela créé un cycle hamiltonien)
-- on ne peut pas passer par le sommet de sortie sauf si c'est le sommet final
isValidPath :: State -> Array Int -> Boolean
isValidPath state path = fromMaybe true $ do
    exit <- state^._exit
    path' <- N.fromArray path
    let path2 = N.init path'
    path2' <- N.fromArray path2
    let path3 = N.tail path2'    
    let begin = N.head path'
    let end = N.last path'
    pure $ length (nub path2) == length path2 && not (elem exit path3) && not (elem end path3) && (
        begin /= end || length path == (state^._nbRows) * (state^._nbColumns) + (if begin == exit then 1 else 0) && end == exit
    )

instance pathGame :: Game (Array Int) Ext Int where
    canPlay state v =
        case N.fromArray (state^._position) of
            Nothing -> state^._mode == Mode2
            Just path ->
                case pathBetween (state^._nbColumns) (N.last path) v of
                    Nothing -> false 
                    Just p -> not (null p) && isValidPath state (state^._position <> p)

    play state v =
        let path = state^._position in
        if null path then
            [v]
        else fromMaybe path $ do -- la sequence ne peut échouer si canPlay a réussi
            l <- last path
            p2 <- pathBetween (state^._nbColumns) l v
            pure $ path <> p2

    isLevelFinished state =
        length (state^._position) == state^._nbColumns * state^._nbRows + (if state^._exit == head (state^._position) then 1 else 0)

    initialPosition state = pure $ case state^._exit of
        Nothing -> []
        Just exit -> [exit]

    onNewGame state = flip (set _exit) state <$>
        if state^._mode == Mode1 then
            randomInt (state^._nbRows * state^._nbColumns) <#> Just
        else
            pure Nothing

    computerMove _ = Nothing
    sizeLimit _ = SizeLimit 2 2 9 9
    updateScore st = st ∧ true

selectVertexA :: ∀effs. Int -> Action State (rng :: RNG, delay :: DELAY | effs)
selectVertexA v = do
    state <- getState
    if null (state^._position) then
        setState (_position .~ [v])
    else if isNothing (state^._exit) then
        setState (_exit .~ Just v)
    else
        playA v


selectModeA :: ∀effs. Mode -> Action State (rng :: RNG | effs)
selectModeA = newGame' (set _mode)