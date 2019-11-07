module Lib.Util where

import Prelude
import Data.Maybe (fromMaybe, maybe)
import Data.Tuple (Tuple)
import Data.Array (range, (!!), mapWithIndex, updateAt, tail, zip, zipWith)
import Data.Int (toNumber, floor)

range' :: Int -> Int -> Array Int
range' n m = if n > m then [] else range n m
infix 8 range' as ..

tabulate :: ∀a. Int -> (Int -> a) -> Array a
tabulate 0 _ = []
tabulate n f = 0 .. (n - 1) <#> f

tabulate2 :: ∀a. Int -> Int -> (Int -> Int -> a) -> Array a
tabulate2 n m f = tabulate (n * m) $ \i -> f (i / m) (i `mod` m)

map2 :: ∀a b c. Array a -> Array b -> (Int -> a -> b -> c) -> Array c
map2 t1 t2 fn = zipWith ($) (mapWithIndex fn t1) t2

map3 :: ∀a b c d. Array a -> Array b -> Array c -> (Int -> a -> b -> c -> d) -> Array d
map3 t1 t2 t3 fn = zipWith ($) (zipWith ($) (mapWithIndex fn t1) t2) t3

rangeStep :: Int -> Int -> Int -> Array Int
rangeStep begin end step = tabulate (max 0 (1 + (end - begin) `div` step)) \i -> begin + i * step

floatRange :: Number -> Number -> Number -> Array Number
floatRange begin end step = tabulate (max 0 (floor $ 1.0 + (end - begin) / step)) \i -> begin + toNumber i * step

-- [x1, x2, x3, x4] -> [(x1, x2), (x2, x3), (x3, x4)]
pairwise :: ∀a. Array a -> Array (Tuple a a)
pairwise list =  maybe [] (zip list) (tail list)

-- échange les éléments à la position i et j, retourne array si une des positions est hors du tableau  
swap :: ∀a. Int -> Int -> Array a -> Array a
swap i j array = fromMaybe array $ do
    x <- array !! i
    y <- array !! j
    array # updateAt i y >>= updateAt j x

type Coord = {row :: Int, col :: Int}

coords :: Int -> Int -> Coord
coords cols i = { row: i / cols, col: i `mod` cols }
    
dCoords :: Int -> Int -> Int -> Coord
dCoords cols x y = {
    row: p.row - q.row,
    col: p.col - q.col
} where
    p = coords cols x
    q = coords cols y

