module Lib.Random (class Random, number, int, int', bool, arrayOf, shuffle, element, element') where
import Prelude
import Data.Maybe (Maybe, fromMaybe)
import Data.Unfoldable (replicateA)
import Data.Tuple (fst, snd)
import Data.Array (length, index, sortWith, zip)
import Data.Array.NonEmpty as NEA
import Effect (Effect)
import Effect.Random (randomInt, random)

class Monad m <= Random m where
    int ∷ Int → Int → m Int -- | generate a random integer in the range [n, m]
    number ∷ m Number       -- | random number in the range [0, 1[ 

instance Random Effect where
    int = randomInt
    number = random

-- | generate a random integer in the range [0, n - 1]
int' ∷ ∀m. Random m ⇒ Int → m Int
int' n = int 0 (n-1)

-- | generate a random integer in the range [n, m]

-- | generate a random boolean 
bool ∷ ∀m. Random m ⇒ m Boolean
bool = int' 2 <#> eq 0

arrayOf ∷ ∀m a. Random m ⇒ Int → m a → m (Array a)
arrayOf = replicateA 

-- | randomly shuffle an array
shuffle ∷ ∀m a. Random m ⇒ Array a → m (Array a)
shuffle xs = do
    ns <- arrayOf (length xs) number
    pure (map snd (sortWith fst (zip ns xs)))

-- | randomly select an element from a non empty array
element ∷ ∀m a. Random m ⇒ NEA.NonEmptyArray a → m a
element t = fromMaybe (NEA.head t) <$> NEA.index t <$> int' (NEA.length t)

-- | randomly select an element from an array
element' ∷ ∀m a. Random m ⇒ Array a → m (Maybe a)
element' t = index t <$> int' (length t)