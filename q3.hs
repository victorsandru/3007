------------------------------
-- Don't touch the line above.



-- QUIZ 3 INSTRUCTIONS
--
-- 1. Submit in Gradescope. The autograder will be running.
--
-- 2. You can use whatever you have on your laptop or on paper, as long as you are
--    not using AI tools, or making any network connections except for the
--    officially allowed ones (CoMaS, gradescope, brightspace, learnyouahaskell.com
--    and the course website).
--
-- 3. You can use any function available (i.e. from the Prelude), as long as you do 
--    not add any "import" lines.
--
-- 4. No list comprehensions allowed!
--
-- 4. Carefully read everything in this file.
--
--
-- NOTE: CODE RESTRICTIONS 
--
-- The usual restrictions from assignments apply. To be precise, any code you
-- write should be one of the following.
-- 
-- 1. Replacing an "undefined".
--
-- 2. A comment.
--
-- 3. A new top-level definition (e.g. a "helper function).
--
-- 4. A new definition inside a function. E.g. if the function to complete is
--
--       f l = 
--           foldr undefined undefined undefined
--
--    then you can, in addition to replacing the "undefined" parts, add bindings 
--    using "let" and/or "where", as in
--
--       f l = let foo = 37 in
--             foldr (...) (...) (...) 
--             where bar x = x+17
--
-- 5. Replacing a definition (but not its type declaration), if the
--    definition is of the form
--
--       f = undefined
--
-- The above restrictions will checked by course staff after the quiz, and
-- non-conforming functions will be given a zero. 

-- Points in a plane. 
data Point = Point Int Int
    deriving (Eq, Show)

-- The "origin", or centre, of the plane, i.e. (0,0)..
origin :: Point
origin = Point 0 0

-- square of the distance from the origin; all that matters is that d p gets
-- bigger the farther the point p is from the origin 
d :: Point -> Int 
d (Point x y) = x^2 + y^2

-- The first point is as close or closer to the origin than the second.
-- Note: p1 |<=| p2 can also be written as (|<=|) p1 p2.
p1 |<=| p2 = d p1 <= d p2

-- Some points for testing.
points :: [Point]
points = 
    [ Point 6 3
    , Point 4 9
    , Point 1 1
    , Point 2 3
    , Point 4 1
    ]

orderedPoints =
    [ Point 1 1
    , Point 2 3
    , Point 4 1
    , Point 6 3
    , Point 4 9
    ]    

-- FYI. See function "insert" for the definition of ordered.
-- orderedPoints =


-- points are added component-wise
addP :: Point -> Point -> Point
addP (Point x1 y1) (Point x2 y2) = 
    Point (x1 + x2) (y1 + y2)

-- Use foldr to add a list of points.
sumP :: [Point] -> Point
sumP ps = foldr (\p x -> addP p x) origin ps

-- Use foldr to compute the point in a non-empty list that is nearest the origin
-- (with respect to |<=|). If more than one point is the same distance, any of
-- them can be chosen.
nearest :: [Point] -> Point
nearest [] = origin 
nearest (z : ps) = 
    foldr (\a b -> if a |<=| b then a else b)
        z
        (ps :: [Point])

-- Insert a point into an ordered list so that the resulting list is ordered,
-- A list of points is ordered if it is in non-decreasing order with respect to
-- |<=|.
insert :: Point -> [Point] -> [Point]
-- insert p [] = [p]
insert p (x:xs) = 
    if p |<=| x then p : xs 
    else x : insert p xs
    
-- Put the given list of points in order, where "ordered" is defined in the
-- comment before "insert". 
order :: [Point] -> [Point]
order [] = []
order ps = 
    foldr 
        undefined
        undefined
        (undefined :: [Point])

--  From Assignment 6.
class BinOp a where
    zero :: a
    (<.>) :: a -> a -> a

--  From Assignment 6.
instance BinOp Int where
    zero = 0
    x <.> y = x + y

--  From Assignment 6.
data Mul a = Mul { getMul :: a } deriving Show

--  From Assignment 6.
instance Num a => BinOp (Mul a) where
    zero = Mul 1 
    Mul x <.> Mul y = Mul (x*y)

-- A simplification of the "reduce" from Assignment 6.
reduce :: BinOp a => [a] -> a
reduce xs = foldr (\x z -> x <.> z) zero xs

data Array a = Array [a] deriving (Eq, Show)

arrays :: [Array Int]
arrays = [Array [1,2,3], Array [2,3,4], Array [3,4,5]] 

-- If n > length l then add as many copies to the end of l as needed to make the
-- list length n. Otherwise, return l. 
-- E.g. pad 5 [1,2,3] = [1,2,3,0,0]
-- Helpful Prelude function: replicate :: Int -> a -> [a]
pad :: Int -> a -> [a] -> [a]
pad n x l = 
    undefined
    -- if n > length l then replicate (n - length l)
    -- else l

-- zipOp op [x1,...,xn] [y1,...,yn] = [op x1 y1, ..., op xn yn]
-- If the lists are of different lengths, the extra elements of the longer list
-- are ignored and the result is the length of the shorter list.
zipOp ::  (a -> a -> a) -> [a] -> [a] -> [a]
zipOp= undefined

-- Pad the shortest list to make it the length of the longest list, then apply
-- zipOp.
-- Example:
-- zipOpWithPadding 0 (+) [1,2,3] [1,1,1,1,1] = [2,3,4,1,1]
zipOpWithPadding :: a -> (a -> a -> a) -> [a] -> [a] -> [a]
zipOpWithPadding padElement op xs ys =
    let n = max (length xs) (length ys) in
    zipOp op (pad n padElement xs) (pad n padElement ys)

-- Define <.> for arrays to 1) pad the shortest argument to the length of the
-- longest using type a's zero for the padding element, then 2) use type a's <.>
-- to add the corresponding elements of the two arrays. Define zero so that for
-- any array x, zero <.> x ≡ x <.> zero ≡ x.
-- Example: Array [1::Int,2,3] <.> Array [1,1,1,1,1] = Array [2,3,4,1,1]
instance BinOp a => BinOp (Array a) where
    zero = zero
    Array xs <.> Array ys = 
        undefined

-- Apply f to every element of the array.
arrayMap :: (a -> b) -> Array a -> Array b
arrayMap f (Array l) = undefined

-- Add the corresponding elements of the input arrays.
-- Example: addArrays arrays ≡ Array [6,9,12]
addArrays :: BinOp a => [Array a] -> Array a 
addArrays = 
    undefined

-- Multiply the corresponding elements of the input arrays using the '*'
-- provided by Num a. Hint: use arrayMap to change the types of the array
-- elements. 
-- mulArrays arrays ≡ Array [6,24,60]
mulArrays :: (Num a, BinOp a) => [Array a] -> Array a
mulArrays l = 
    undefined
