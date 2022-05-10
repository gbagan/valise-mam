module Lib.Util
  ( Coord
  , chooseInt'
  , class PartialRecord
  , coords
  , dCoords
  , elements'
  , map2
  , map3
  , pairwise
  , partialUpdate
  , rangeWithStep
  , rangeWithStep'
  , repeat2
  , shuffle
  , swap
  )
  where

import MamPrelude
import Prim.Row (class Union, class Nub)
import Record as Record
import Control.Monad.Gen (chooseInt, chooseFloat)

repeat2 ∷ ∀a. Int → Int → (Int → Int → a) → Array a
repeat2 n m f = repeat (n * m) $ \i → f (i / m) (i `mod` m)

map2 ∷ ∀a b c. Array a → Array b → (Int → a → b → c) → Array c
map2 t1 t2 fn = zipWith ($) (mapWithIndex fn t1) t2

map3 ∷ ∀a b c d. Array a → Array b → Array c → (Int → a → b → c → d) → Array d
map3 t1 t2 t3 fn = zipWith ($) (zipWith ($) (mapWithIndex fn t1) t2) t3

rangeWithStep ∷ Int → Int → Int → Array Int
rangeWithStep begin end step = repeat (max 0 (1 + (end - begin) / step)) \i → begin + i * step

rangeWithStep' ∷ Number → Number → Number → Array Number
rangeWithStep' begin end step = repeat (max 0 (floor $ 1.0 + (end - begin) / step)) \i → begin + toNumber i * step

-- | [x1, x2, x3, x4] → [(x1, x2), (x2, x3), (x3, x4)]
pairwise ∷ ∀a. Array a → Array (Tuple a a)
pairwise list =  maybe [] (zip list) (tail list)

-- | échange les éléments à la position i et j
swap ∷ ∀a. Int → Int → Array a → Maybe (Array a)
swap i j array = do
    x ← array !! i
    y ← array !! j
    array # updateAt i y >>= updateAt j x

type Coord = {row ∷ Int, col ∷ Int}

coords ∷ Int → Int → Coord
coords cols i = { row: i / cols, col: i `mod` cols }
    
dCoords ∷ Int → Int → Int → Coord
dCoords cols x y = {
    row: p.row - q.row,
    col: p.col - q.col
} where
    p = coords cols x
    q = coords cols y


-- | generate a random integer in the range [0, n - 1]
chooseInt' ∷ ∀m. MonadGen m ⇒ Int → m Int
chooseInt' n = chooseInt 0 (n-1)

-- | randomly select an element from an array
elements' ∷ ∀m a. MonadGen m ⇒ Array a → m (Maybe a)
elements' t = (t !! _) <$> chooseInt' (length t)

-- | randomly shuffle an array
shuffle ∷ ∀m a. MonadGen m ⇒ Array a → m (Array a)
shuffle xs = do
    ns <- replicateA (length xs) (chooseFloat 0.0 1.0)
    pure $ map snd $ sortWith fst $ zip ns xs

class PartialRecord (r1 ∷ Row Type) (r2 ∷ Row Type) where
    partialUpdate ∷ Record r1 → Record r2 → Record r2

instance (Union r1 r2 s, Nub s r2) ⇒ PartialRecord r1 r2 where
    partialUpdate = Record.merge