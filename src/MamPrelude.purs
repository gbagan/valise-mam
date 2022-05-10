module MamPrelude ((∘), (∧), (≠), (..), (?:), range', flipFromMaybe, repeat, module Exports) where
import Prelude
import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category,
                class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field,
                class Functor, class HeytingAlgebra, class Monad, class Monoid, class Ord, class Ring, class Semigroup,
                class Semigroupoid, class Semiring, class Show,
                type (~>), Ordering(..), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare,
                comparing, compose, conj, const, degree, discard, disj, eq, flap, flip, gcd, identity, ifM, join, lcm, liftA1,
                liftM1, map, max, mempty, min, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit,
                unless, unlessM, void, when, whenM, zero,
                (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<),
                (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||)) as Exports
import Control.Monad.State (get, modify_, put) as Exports
import Control.Monad.Trans.Class (lift) as Exports
import Data.Array (range)
import Data.Array ((!!), head, last, cons, snoc, filter, find, take, null, length, mapMaybe, catMaybes, nub, elemIndex, sort, uncons,
        replicate, reverse, modifyAt, insertAt, updateAt, updateAtIndices, deleteAt, mapWithIndex, concat, tail, concatMap, sortWith, zip, zipWith) as Exports
import Data.Either (Either(..)) as Exports
import Data.Foldable (class Foldable, foldl, foldr, elem, any, all, and, or, for_, minimumBy) as Exports
import Data.Int (toNumber, floor, even) as Exports
import Data.Lens (Lens', lens, set, Iso', iso, view, over, (^.), (^?), (.~), (%~), (?~), (+~), (.=), (%=), (+=)) as Exports
import Data.Lens.Index (ix) as Exports
import Data.Lens.At (at) as Exports
import Data.Lens.Lens.Product (_1, _2) as Exports
import Data.Lens.Record (prop) as Exports
import Data.List (List(..)) as Exports
import Data.Map (Map) as Exports
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust, isNothing) as Exports
import Data.Number (cos, sin, pi, sqrt) as Exports
import Data.Ord (abs) as Exports
import Data.Traversable (sequence, for) as Exports
import Data.Tuple (Tuple(..), fst, snd) as Exports
import Data.Tuple.Nested ((/\)) as Exports
import Data.Array.NonEmpty (NonEmptyArray) as Exports
import Data.Unfoldable (replicateA) as Exports
import Data.String (joinWith) as Exports
import Control.Alt ((<|>)) as Exports
import Control.Alternative (guard) as Exports
import Control.Monad.Gen (class MonadGen) as Exports
import Control.Monad.Rec.Class (class MonadRec) as Exports
import Control.Monad.State.Class (class MonadState) as Exports
import Data.Time.Duration (Milliseconds(..)) as Exports
import Effect.Class (class MonadEffect, liftEffect) as Exports
import Type.Proxy (Proxy(..)) as Exports

range' ∷ Int → Int → Array Int
range' n m = if n > m then [] else range n m
infix 8 range' as ..

repeat ∷ ∀a. Int → (Int → a) → Array a
repeat n f = 0 .. (n - 1) <#> f

infixr 9 Exports.compose as ∘
infix 6 Exports.Tuple as ∧
infix 4 Exports.notEq as ≠

flipFromMaybe ∷ ∀a. Exports.Maybe a → a → a
flipFromMaybe = flip Exports.fromMaybe

infix 0 flipFromMaybe as ?: