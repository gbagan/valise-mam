module Game.Paths.Model where
import Prelude
import Data.Maybe (Maybe(..), maybe, fromMaybe, isNothing)
import Data.Array (length, nub, elem, null, head, last)
import Data.Array.NonEmpty (fromArray, head, last, init, tail) as N
import Data.Lens (Lens', lens, view, set, (^.), (.~))
import Lib.Random (randomInt)
import Lib.Util (dCoords, range)
import Game.Core (State(..), class Game, SizeLimit(..), newGame', genState, _nbRows, _nbColumns, _position, playA)
import Pha.Class (Action)
import Pha.Action (action, ifThenElseA)

data Mode = Mode1 | Mode2
derive instance eqMode :: Eq Mode

type Position = Array Int
type Ext' = { exit :: Maybe Int, mode' :: Mode }
newtype Ext = Ext Ext'
type PathsState = State Position Ext

pathsState :: PathsState
pathsState = genState [] (_{nbRows = 4, nbColumns = 6}) (Ext { exit: Nothing, mode': Mode1 })

_ext :: Lens' PathsState Ext'
_ext = lens (\(State _ (Ext a)) -> a) (\(State s _) x -> State s (Ext x))
_exit :: Lens' PathsState (Maybe Int)
_exit = _ext <<< lens (_.exit) (_{exit = _})
_mode :: Lens' PathsState Mode
_mode = _ext <<< lens (_.mode') (_{mode' = _})

-- renvoie un chemin horizontal ou vertical entre u et v si celui ci existe (u exclus)
pathBetween :: Int -> Int -> Int -> Maybe (Array Int)
pathBetween columns u v =
    let {row, col} = dCoords columns u v in
    if row == 0 then
        Just (if u < v then range (u + 1) v 1 else range (u - 1) v (-1))
    else if col == 0 then
        Just (if u < v then range (u + columns) v columns else range (u - columns) v (-columns))
    else
        Nothing

-- teste si un chemin est valide (sans répétition de sommets sauf les extrémités si cela créé un cycle hamiltonien)
-- on ne peut pas passer par le sommet de sortie sauf si c'est le sommet final
isValidPath :: PathsState -> Array Int -> Boolean
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
                pathBetween (state^._nbColumns) (N.last path) v # maybe false \p ->
                    not (null p) && isValidPath state (state^._position <> p)

    play state v =
        let path = state^._position in
        if null path then
            [v]
        else fromMaybe [] $ do -- la sequence ne peut échouer si canPlay a réussi
            l <- last path
            p2 <- pathBetween (state^._nbColumns) l v
            pure $ path <> p2

    isLevelFinished state =
        length (state^._position) == state^._nbColumns * state^._nbRows + (if state^._exit == head (state^._position) then 1 else 0)

    initialPosition = pure <<< view _position 

    onNewGame state =
        if state^._mode == Mode1 then
            randomInt (state^._nbRows * state^._nbColumns) <#>
                \begin -> state # _position .~ [begin] # _exit .~ Just begin 
        else
            pure $ state # _position .~ [] # _exit .~ Nothing

    computerMove _ = Nothing
    sizeLimit _ = SizeLimit 2 2 9 9

selectVertexA :: Int -> Action PathsState
selectVertexA v =
    ifThenElseA (\state _ -> null $ state^._position)
        (action $ _position .~ [v])
    -- else
        $ ifThenElseA (\state _ -> isNothing $ state^._exit)
            (action $ _exit .~ Just v)
            (playA v)


selectModeA :: Mode -> Action PathsState
selectModeA = newGame' $ (set _mode)