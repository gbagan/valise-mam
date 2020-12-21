module Lib.Util where

import MyPrelude
import Prim.Row (class Union, class Nub)
import Record as Record


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

class PartialRecord (r1 ∷ #Type) (r2 ∷ #Type) where
    partialUpdate ∷ Record r1 → Record r2 → Record r2

instance precord ∷ (Union r1 r2 s, Nub s r2) ⇒ PartialRecord r1 r2 where
    partialUpdate = Record.merge