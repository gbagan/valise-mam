module Game.Paths.Model where
import MamPrelude
import Data.Array (init)
import Lib.Util (chooseInt', dCoords, rangeWithStep)
import Game.Core (GModel, class Game, class MsgWithCore, CoreMsg, SizeLimit(..), 
        coreUpdate,
        _ext, newGame, genModel, _nbRows, _nbColumns, _position, playA, defaultUpdateScore)
import Lib.Update (UpdateMam)

data Mode = Mode1 | Mode2
derive instance Eq Mode

-- | une position représente le chemin que l'on a effectué avec le héros
-- | c'est à dire la liste des positions des cases
-- | un coup (move) représente le numéro de cases où l'on souhaite aller
-- |    la case n'est pas forcemment adjacente à la position du héro
-- |    mais accessible en ligne horizontale ou verticale

type Position = Array Int
type Ext' = { exit ∷ Maybe Int, mode ∷ Mode }
newtype Ext = Ext Ext'
type Model = GModel Position Ext

imodel ∷ Model
imodel = genModel [] _{nbRows = 4, nbColumns = 6} (Ext { exit: Nothing, mode: Mode1 })

_ext' ∷ Lens' Model Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_exit ∷ Lens' Model (Maybe Int)
_exit = _ext' ∘ prop (Proxy ∷ _ "exit")
_mode ∷ Lens' Model Mode
_mode = _ext' ∘ prop (Proxy ∷ _ "mode")

-- renvoie un chemin horizontal ou vertical entre u et v si celui ci existe (u exclus du chemin)
pathBetween ∷ Int → Int → Int → Maybe (Array Int)
pathBetween columns u v =
    let {row, col} = dCoords columns u v in
    if row == 0 then
        Just (if u < v then rangeWithStep (u + 1) v 1 else rangeWithStep (u - 1) v (-1))
    else if col == 0 then
        Just (if u < v then rangeWithStep (u + columns) v columns else rangeWithStep (u - columns) v (-columns))
    else
        Nothing

-- teste si un chemin est valide (sans répétition de sommets)
-- les extrémités peuvent être identiques si le chemin forme un cycle hamiltonien)
-- on ne peut pas passer par le sommet de sortie sauf si c'est le sommet final
isValidPath ∷ Model → Array Int → Boolean
isValidPath model path = fromMaybe true $ do
    exit ← model ^. _exit
    path2 ← init path
    path3 ← tail path2
    begin ← head path
    end ← last path
    pure $ length (nub path2) == length path2 
        && not (elem exit path3)
        && not (elem end path3) 
        && (begin ≠ end 
           || length path == (model^._nbRows) * (model^._nbColumns) + (if begin == exit then 1 else 0) && end == exit
            )

instance Game Position Ext Int where
    name _ = "paths"

    play model v =
        case last (model^._position) of
            Nothing → if model^._mode == Mode2 then Just [v] else Nothing
            Just last → do
                p ← pathBetween (model^._nbColumns) last v 
                guard $ not (null p) && isValidPath model (model^._position <> p)
                Just (model^._position <> p)

    isLevelFinished model =
        length (model^._position) == model^._nbColumns * model^._nbRows 
                                    + (if model^._exit == head (model^._position) then 1 else 0)

    initialPosition model = pure $ case model^._exit of
        Nothing → []
        Just exit → [exit]

    onNewGame model = flip (set _exit) model <$> do
        if model^._mode == Mode1 then
            Just <$> chooseInt' (model^._nbRows * model^._nbColumns) 
        else
            pure Nothing

    sizeLimit _ = SizeLimit 2 2 9 9

    -- méthodes par défault
    computerMove _ = pure Nothing
    updateScore s = defaultUpdateScore s
    onPositionChange = identity
    saveToJson _ = Nothing
    loadFromJson model _ = model

data Msg = Core CoreMsg | SelectVertex Int | SelectMode Mode
instance MsgWithCore Msg where core = Core
    
update ∷ Msg → UpdateMam Model Unit
update (Core msg) = coreUpdate msg

update (SelectVertex v) = do
    model ← get
    if null (model^._position) then
        _position .= [v]
    else if isNothing (model^._exit) then
        _exit .= Just v
    else
        playA v

update (SelectMode mode) = newGame $ set _mode mode