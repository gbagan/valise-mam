module Lib.Core where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Array ((..), (!!), updateAt)

repeat :: forall a. Int -> (Int -> a) -> Array a
repeat 0 _ = []
repeat n f = 0 .. (n - 1) # map f

repeat2 :: forall a. Int -> Int -> ({ row :: Int, col :: Int } -> a) -> Array a
repeat2 n m f = repeat (n * m) $ \i -> f { row: i / m, col : i `mod` m }

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