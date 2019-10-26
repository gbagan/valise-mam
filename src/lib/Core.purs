module Lib.Core where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple (..))
import Data.Array ((..), (!!), updateAt, length, unsafeIndex)
import Data.Int (toNumber, floor)
import Partial.Unsafe (unsafePartial)

tabulate :: forall a. Int -> (Int -> a) -> Array a
tabulate 0 _ = []
tabulate n f = 0 .. (n - 1) <#> f

tabulate2 :: forall a. Int -> Int -> ({ row :: Int, col :: Int } -> a) -> Array a
tabulate2 n m f = tabulate (n * m) $ \i -> f { row: i / m, col : i `mod` m }

map2 :: forall a b c. Array a -> Array b -> (Int -> a -> b -> c) -> Array c
map2 t1 t2 fn = unsafePartial $ tabulate n \i -> fn i (unsafeIndex t1 i) (unsafeIndex t2 i)
    where n = min (t1 # length) (t2 # length)

floatRange :: Number -> Number -> Number -> Array Number
floatRange begin end step = tabulate (max 0 (floor $ 1.0 + (end - begin) / step)) \i -> begin + toNumber i * step

pairwise :: forall a. Array a -> Array (Tuple a a)
pairwise l =
    if length l <= 1 then
        []
    else
        unsafePartial $ (0 .. (length l - 2)) <#> \i -> Tuple (unsafeIndex l i) (unsafeIndex l (i+1))

swap :: forall a. Int -> Int -> Array a -> Array a
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