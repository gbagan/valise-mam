module MyPrelude ((∘), (∧), replicateA, module Exports) where
import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Monoid, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(..), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, discard, disj, eq, flap, flip, gcd, identity, ifM, join, lcm, liftA1, liftM1, map, max, mempty, min, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero, (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||)) as Exports
import Data.Int (toNumber, floor, even) as Exports
import Data.Tuple (Tuple(..), fst, snd) as Exports
import Data.Tuple.Nested ((/\)) as Exports
import Math (cos, sin, pi, sqrt) as Exports
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust, isNothing) as Exports
import Data.Foldable (class Foldable, foldl, foldr, elem, any, all, and, or, for_, minimumBy) as Exports
import Data.Traversable (sequence, for) as Exports
import Data.Array ((!!), head, last, cons, snoc, filter, find, take, null, length, mapMaybe, catMaybes, nub, elemIndex, sort, uncons,
        replicate, reverse, modifyAt, insertAt, updateAt, updateAtIndices, mapWithIndex, concat, tail, concatMap, sortWith, zip, zipWith) as Exports
import Data.Lens (Lens', lens, set, Iso', iso, view, over, (^.), (.~), (%~), (?~), (+~)) as Exports
import Data.Lens.Index (ix) as Exports
import Data.Lens.At (at) as Exports
import Data.Lens.Lens.Product (_1, _2) as Exports
import Data.String (joinWith) as Exports
import Control.Alt ((<|>)) as Exports
import Prelude ((<<<))

infixr 9 Exports.compose as ∘
infix 6 Exports.Tuple as ∧

replicateA :: forall m a. Exports.Applicative m => Int -> m a -> m (Array a)
replicateA n = Exports.sequence <<< (Exports.replicate n)