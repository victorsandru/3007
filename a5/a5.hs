import Prelude hiding (all)

-- DON'T TOUCH THE ABOVE!
--------------------------

{-
 
ASSIGNMENT 5 

DUE: Thursday Oct 17 23:59

WELCOME to The Procrustean Cave of Folding. You will be presented with a number
of small tasks to complete. The tasks are designed to force you to use folds.

As usual, do not change anything in the file except for definitions whose
right-hand-side is "undefined". You can change such defintions as you like, e.g.
adding arguments, as long as you don't change the type of the function. You can
add whatever extra code you like.

For each of the functions below, you will be given a definition of a function
that uses foldr. You need to supply foldr's arguments by completing the
indicated definitions.

-}

data List a = Nil | Cons a (List a) deriving Show

-- all bs ≡ True iff (if and only if) every member of xs is True.
all :: [Bool] -> Bool

zAll :: Bool
zAll =
    True


fAll :: Bool -> Bool -> Bool
fAll x z =
    x && z


all = 
    foldr fAll zAll


-- all bs ≡ True if and only f at least one member of xs is True.
some :: [Bool] -> Bool

zSome :: Bool
zSome =
    False   

fSome :: Bool -> Bool -> Bool
fSome x z =
    x || z


some = 
    foldr fSome zSome


-- toList [x1,x2,...,xn] ≡ x1 `Cons`  (x2 `Cons` ... (xn `Cons` Nil)...)
toList :: [a] -> List a
 
zToList :: List a
zToList =
    Nil

fToList :: a -> List a -> List a
fToList x z =
    Cons x z

toList = foldr fToList zToList
-- (tagify p xs) is the list where each element x of xs is replaced by (x, b)
-- where b is True iff p x is. 
tagify :: (a -> Bool) -> [a] -> [(a,Bool)]
zTagify :: [(a,Bool)]

zTagify = []


fTagify :: (a -> Bool) -> a -> [(a,Bool)] -> [(a,Bool)]
fTagify p x z = 
    (x, p x) : z


tagify p = foldr (fTagify p) zTagify

-- (indexify [x0,...,xn]) is the list where for each i, 0 ≤ i < length xs, xi is
-- replaced by (xi,i). 
-- Note: testing if l is an empty list using l==[] only works if == is defined 
-- for the element type. Use null instead.
-- Example: indexify [1,2,3,4,5] ≡ [(1,0),(2,1),(3,2),(4,3),(5,4)]
indexify :: [a] -> [(a,Int)]
 
zIndexify :: [(a,Int)]
zIndexify = 
    []


fIndexify :: Int -> a -> [(a,Int)] -> [(a,Int)]
fIndexify n x z =
    if null z then [(x,n-1)]
    else (x, snd (head z) - 1) : z


indexify xs =
    foldr (fIndexify (length xs)) zIndexify xs

-- If eq :: a -> a -> Bool is an equivalence relation, and l :: [a], then an
-- equivalence class of l is a maximal subsequence of l where all members are
-- equivalence under p. For example, if 
--   eq s1 s2 = if null s1 then null s2 else not (null s2) && head s1 == head s2
-- l is ["foo", "bar", "baz", "fu", "garbonzo"] then the equivalence classes
-- of l are: ["foo", "fu"], ["bar, baz"] and ["garbonzo"]. 
-- A partition of l with respect to eq is a list of its equivalence classes.

-- partitionInsert inserts a new element into a partition. I.e. if l is a
-- partition with respect to eq, then (partitionInsert eq x l) is a partition of
-- x:l.
partitionInsert :: (a -> a -> Bool) -> a -> [[a]] -> [[a]]

zPartitionInsert :: Either [[a]] [[a]]
zPartitionInsert = 
    Left []


fPartitionInsert :: (a -> a -> Bool) -> a 
                    -> [a] -> Either [[a]] [[a]] -> Either [[a]] [[a]]
fPartitionInsert eq x c z =
    case z of
        Left cs  -> if any (eq x) c then Right ((x:c) : cs) else Left (c : cs)
        Right cs -> Right (c : cs)


partitionInsert eq x cs =
    case foldr (fPartitionInsert eq x) zPartitionInsert cs of
        Left cs'  -> [x] : cs'
        Right cs' -> cs'

-- Compute a partition of a list partition of a list.
partition :: (a -> a -> Bool) -> [a] -> [[a]]

zPartition :: [[a]]
zPartition = 
    []


fPartition :: (a -> a -> Bool) -> a -> [[a]] -> [[a]]
fPartition eq x cs = 
    partitionInsert eq x cs


partition eq = foldr (fPartition eq) zPartition
    
    

