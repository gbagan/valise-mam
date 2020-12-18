module Game.Noirblanc2.Model where

import MyPrelude

import Game.Core (class MsgWithCore, class Game, GState, SizeLimit(..), CoreMsg, _ext, coreUpdate, playA, isLevelFinished, saveToStorage, _position, _nbColumns, _nbRows, _customSize, newGame, genState)
import Lib.Random (Random)
import Lib.Random as R
import Lib.Update (Update, get, modify, randomly)
import Lib.Util (dCoords)


data Card = BlackCard | WhiteCard | EmptyCard
derive instance eqCard ∷ Eq Card

data Phase = PrepPhase | GamePhase
derive instance eqPhase ∷ Eq Phase

type Position = Array Card

reverseCard ∷ Card → Card
reverseCard BlackCard = WhiteCard
reverseCard WhiteCard = BlackCard
reverseCard EmptyCard = EmptyCard

randomCard ∷ Random Card
randomCard = R.bool <#> if _ then WhiteCard else BlackCard

type Ext' = {
    phase ∷ Phase
}

newtype ExtState = Ext Ext'
type State = GState Position ExtState

-- état initial
istate ∷ State
istate = genState [] _{nbRows=1, nbColumns=8, customSize=true}
        (Ext 
            {phase: GamePhase}
        )

-- lenses
_ext' ∷ Lens' State Ext'
_ext' = _ext ∘ iso (\(Ext a) → a) Ext
_phase ∷ Lens' State Phase
_phase = _ext' ∘ prop (SProxy ∷ _ "phase")

neighbor ∷ State → Int → Int → Boolean
neighbor state index1 index2 =
    row * row + col * col == 1
    where {row, col} = dCoords (state^._nbColumns) index1 index2
    
instance game ∷ Game (Array Card) ExtState Int where
    name _ = "noirblanc2"

    play state index =
        if state^._position !! index == Just WhiteCard then
            Just $ state^._position # mapWithIndex \i card →
                if i == index then
                    EmptyCard
                else if neighbor state index i then
                    reverseCard card
                else
                    card
        else 
            Nothing

    initialPosition state = pure (replicate (state^._nbRows * state^._nbColumns) WhiteCard)
    
    isLevelFinished state = all (_ /= WhiteCard) (state^._position)

    onNewGame = pure 

    sizeLimit _ = SizeLimit 1 1 12 12

    -- méthodes par default
    saveToJson _ = Nothing
    loadFromJson st _ = st 
    computerMove _ = pure Nothing
    updateScore st = st ∧ true
    onPositionChange = identity

sizes ∷ Array (Tuple Int Int)
sizes = [3∧3, 4∧4, 2∧10, 3∧10, 5∧5, 8∧8, 8∧8]

data Msg = Core CoreMsg | Play Int | ToggleCard Int | ToggleCustom | Shuffle
instance withcore ∷ MsgWithCore Msg where core = Core

update ∷ Msg → Update State
update (Core msg) = coreUpdate msg
update (Play move) = playA move
update (ToggleCard i) = modify $ over (_position ∘ ix i) reverseCard
update Shuffle = randomly \st → do
    pos <- replicateA (length $ st^._position) randomCard
    pure $ st # set _position pos 
update ToggleCustom = do
    state <- get
    if state^._phase == PrepPhase then
        modify $ set _phase GamePhase
    else
        newGame $ set _phase PrepPhase