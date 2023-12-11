import Data.Kind
import Control.Monad.Trans.State.Lazy

-- DON'T TOUCH THE ABOVE!
--------------------------

-----------------------------------------
-- Definitions from the tutorial section.
-----------------------------------------

class BinOp a where
    zero :: a
    (<.>) :: a -> a -> a

class Reducible t where
    reduce :: BinOp c => (a -> c) -> t a -> c

instance BinOp ([a]) where
    zero = []
    xs <.> ys = xs ++ ys

instance (BinOp a, BinOp b) => BinOp (a,b) where
    zero = (zero, zero) 
    (x,y) <.> (x',y') =  (x <.> x', y <.> y')

instance Reducible [] where
    reduce f xs = foldr (\x z -> f x <.> z) zero xs

data Add a =  Add { getAdd :: a } deriving Show
data Mul a =  Mul { getMul :: a } deriving Show

instance Num a => BinOp (Add a) where
    zero = Add 0 
    Add x <.> Add y = Add (x+y)

instance Num a => BinOp (Mul a) where
    zero = Mul 1 
    Mul x <.> Mul y = Mul (x*y)

--------------------------------------------------
-- Some more definitions, with stuff for you to do  
--------------------------------------------------

-- Two data types to use with reduce to compute Supremums (biggest elements) and
-- Infimums (smallest elements). (The terms "supremum" and "infimum" are from
-- lattice theory.) The ordering used for Bool is defined by False <
-- True.
data BoolSup = BoolSup {getBoolSup :: Bool} deriving (Eq, Show)
data BoolInf = BoolInf {getBoolInf :: Bool} deriving (Eq, Show)

-- Examples: 
-- getBoolSup (reduce BoolSup [False, True, False]) ≡ True
-- getBoolSup (reduce BoolSup [False, False, False]) ≡ False 
instance BinOp BoolSup where
    zero = BoolSup False
    (<.>) a b = BoolSup (getBoolSup a || getBoolSup b)

-- Examples: 
-- getBoolInf (reduce BoolInf [True, True, False]) ≡ False
-- getBoolInf (reduce BoolInf [True, True, True]) ≡ True 
instance BinOp BoolInf where
    zero = BoolInf True
    (<.>) a b = BoolInf (getBoolInf a && getBoolInf b)

-- We can generalize BoolSup and BoolInf from Bool to any type that has an
-- ordering and default sups/infs (see the instances below).
data Sup a =  Sup {getSup :: a} deriving (Show, Eq)
data Inf a =  Inf {getInf :: a} deriving (Show, Eq)

-- Do ":info Bounded" to see what operations are provided by the Bounded class. 
-- Example:
-- getSup (reduce Sup [10::Int, -33, 17]) ≡ 17
instance (Bounded a, Ord a) => BinOp (Inf a) where
    zero = Inf maxBound
    Inf a <.> Inf b = Inf (min a b)

-- Example:
-- getInf (reduce Inf [10::Int, -33, 17]) ≡ -33
instance (Bounded a, Ord a) => BinOp (Sup a) where
    zero = Sup minBound
    Sup a <.> Sup b = Sup (max a b)

-- Wrappers for Maybe types.
data First a =  First { getFirst :: Maybe a} deriving (Show, Eq)
data Last a =  Last { getLast :: Maybe a } deriving (Show, Eq)

-- Examples
-- getFirst (reduce First [Nothing, Just 'a', Nothing, Just 'b', Nothing])
-- ≡ Just 'a'
instance BinOp (First a) where
    zero = First Nothing
    First Nothing <.> m = m
    m1 <.> m2 = m1


-- Examples
-- getLast (reduce Last [Nothing, Just 'a', Nothing, Just 'b', Nothing])
-- ≡ Just 'b'
instance BinOp (Last a) where
    zero = Last Nothing
    a <.> Last Nothing = a
    a <.> b = b

data Tree a = Leaf a | Node a (Tree a) (Tree a) 
    deriving Show

aTree = fatTree 3

aMap :: [(Int,Int)]
aMap = map (\x -> (x, 50-x)) [0..50]

-- Examples: 
-- getSup (reduce  Sup aTree) 
-- ≡ 99
-- getFirst (reduce (First . flip lookup aMap) aTree)
-- ≡ Just 8
-- getLast (reduce (Last . flip lookup aMap) aTree)
-- ≡ Just 42
instance Reducible Tree where
    reduce f (Leaf x) = f x
    reduce f (Node x t1 t2) = f x <.> reduce f t1 <.> reduce f t2

--------------------------------------------------------------------------------
-- No need to look further. No more stuff to for you to do. 
--
-- Below is an implementation of Blum Blum Shub pseudo-random sequence
-- generator. We use the State monad to create a Tree with "random keys". We use
-- all of this only to generate a single small Tree (sadly named aTree).
--------------------------------------------------------------------------------

-- BBS takes two primes p,q, and a number relatively prime to both p and q
-- called the "seed", and generates the sequence defined by x_0 = seed and
-- x_{i+1} = (x_i)^2 mod N, where N = pq. For the two primes below, the gcd of
-- p-1 and q-1 is 2.
bbsPrime1 = 47055833443  -- "p"
bbsPrime2 = 47055833459  -- "q"
bbsMod = bbsPrime1*bbsPrime2 

-- Seed can be any number relatively prime to p and q, so any number less
-- than p-1 will do. 
bbsSeed = bbsPrime1 - 1

bbsNext x = mod (x^2) bbsMod  -- "N" 

-- The BBS sequence starting at an arbitrary integer, represented as an infinite
-- list.
bbsStartingWith x = x : bbsStartingWith (bbsNext x) 

-- The BBS sequence starting at the seed.
bbs = bbsStartingWith bbsSeed 

-- (pseudoRandoms n) is an infinite sequence of pseudo-random n-digit numbers.
-- They are the last n digits from the Blum Blum Shub sequence using p, q and
-- seed as above. I haven't looked that the theory behind it, but it's said that
-- the last few digits of the SSB sequence numbers are more "random" than the
-- entire numbers. 
pseudoRandoms :: Int -> [Int]
pseudoRandoms n = map (flip mod $ 10^n) bbs

-- Generate a "fat" tree of the given depth using pseudorandom numbers for the
-- keys.  A fat tree is one with the maximum number of nodes for its depth. Use
-- the State monad to "store" the unused part of pseudorandom sequence. Nothing
-- is actually stored in the usual sense. The monad just passes around the
-- sequence implicitly.
fatTreeGen :: Int -> State [Int] (Tree Int)
fatTreeGen 0 = do
    randos <- get      -- get the stored sequence
    put $ tail randos  -- replace the stored sequence with its tail
    return $ Leaf $ head randos
fatTreeGen n | n>0 = do
    randos <- get
    put $ tail randos
    t1 <- fatTreeGen (n-1)
    t2 <- fatTreeGen (n-1)
    return $ Node (head randos) t1 t2

-- Run the tree generate using the BBS sequence as the starting "State".
fatTree :: Int -> Tree Int
fatTree n = evalState (fatTreeGen n) (pseudoRandoms 2) 
