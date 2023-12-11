import Prelude hiding (Nothing, Just) 
import Data.List
import ExamBase

-- *****************************************************************************
-- DO NOT TOUCH ANYTHING ABOVE THIS LINE




--------------------------------------------------------------------------------
--
-- INSTRUCTIONS
--
-- The type definitions you will need, and some examples, are in the
-- accompanying file ExamBase.hs. Carefully read that file before doing anything
-- else. Only submit this file.
--
-- Supply the missing implementations (functions with "undefined" on the
-- right-hand side). You can implement them however you like, using any imported
-- functions (see the top of the file) or auxiliary/helper functions, as long as
-- you respect the following.
--
-- 1. Don't change any types. 
--
-- 2. Follow any extra instructions given for the question (be sure to read all
--    the supplied comments).
--
-- 3. Don't use list comprehension (if you don't know what that is, good).
--
-- You do not need to use any of the Prelude or Data.List functions except for a
-- handful of obvious ones, like boolean not/and/or, map, foldr and filter.
--
-- Please note that the autgrader won't detect violations of 2 or 3 above. The
-- submissions will be inspected after the exam and the autograder's marks will
-- be adjusted accordingly.
--
-- The exam has four sections. Some sections have more questions than others.
-- Each question has the same weight. 
--
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--
-- BASIC QUESTIONS (recursion, pattern matching)
--
--------------------------------------------------------------------------------

-- These questions use the parts database defined in ExamBase.hs. If you haven't
-- done so already, read that file carefully (except for the parser stuff at the
-- end).

-- Is the database empty
isEmpty :: PartsDB -> Bool
isEmpty Empty = True
isEmpty parts = False

-- The name of the first entry in the parts database
entryName :: PartsDB -> String
entryName (Entry name desc price db) = name

-- True if the prices are all non-negative.
validPrices :: PartsDB -> Bool
validPrices Empty = True
validPrices (Entry name desc price db) = 
    if price < 0 then False
    else validPrices db

-- (inDB name db) is true means the named part is in the database.
inDB :: String -> PartsDB -> Bool
inDB _ Empty = False
inDB n (Entry name desc price db) = 
    if name == n then True
    else inDB n db

-- Each entry has a unique part name.
hasNoDups :: PartsDB -> Bool
hasNoDups Empty = True
hasNoDups (Entry name desc price db) = 
    if inDB name db then False
    else hasNoDups db



-- 5/5

--------------------------------------------------------------------------------
--
-- MAP, FILTER, FOLD
--
-- RECURSION PROHIBITED. You are not allowed to use recursive definitions in
-- this section. Instead, use map, filter and foldr. 
--
--------------------------------------------------------------------------------

-- (nmtMap f ls) is the list of non-empty elements of ls, with f applied to each
-- of them. For example, nmtMap head [[], [1], [2,3], [], [4,5,6]] = [1,2,4].
-- Use map and/or filter for this function. 
nmtMap :: ([a] -> b) -> [[a]] -> [b]
nmt _ [] = []
nmtMap f ls = 
    map (\e -> (f e)) (filter (\e -> length e > 0) ls)


-- Use foldr and max to compute the largest number in the given list. Return
-- minBound if the list is empty (minBound is a constant defined in the
-- Prelude). Note: solutions not based on foldr will be graded zero.
listMax :: [Int] -> Int
listMax [] = minBound
listMax list =  
    foldr (\acc l -> if (max acc l) == l then l else acc) minBound list

-- Use foldr to convert list of (name, description, price) tuples into a
-- PartsDB. Example: makeDB dbTuples == db.
makeDB :: [(String, String, Int)] -> PartsDB
makeDB ts = 
    let
        t = map makeDB' ts
    in
        foldr (\acc e -> makeDB'' e acc) Empty t

makeDB' :: (String, String, Int) -> PartsDB
makeDB' (n, d, p) = Entry n d p Empty

-- makeDB'' :: PartsDB -> Entry
makeDB'' (Entry n d p db) = Entry n d p

-- 2/3
--------------------------------------------------------------------------------
--
-- TYPE CLASSES AND MONADS 
--
---------------------c-----------------------------------------------------------

-- Give an instance of the type class Named for the type Part using the partName
-- as the name (see ExamBase.hs for definitions).
instance Named Part where
    name (Part n d p) = Name n

-- The list of all elements in the given list whose name is the given name.
named :: Named a => Name -> [a] -> [a]
named n es = 
    filter (\x -> name x == n) es

-- Sample list of parts.
partsList = 
    map (\(x,y,z) -> Part x y z) dbTuples

-- The supervisor of the supervisor of an employee, if one exists. See
-- ExamBase.hs for an explanation.
supsSup :: String -> SupervisedBy -> Maybe String
supsSup name sups = 
    do
        s <- lookup name sups
        boss <- lookup s sups
        s


-- /3
--------------------------------------------------------------------------------
--
-- Lambda Calculus
--
--------------------------------------------------------------------------------

-- A useful function for this section. See FinalBase.hs for the definition of
-- Occ.
occAdd :: Occ -> Occ -> Occ
occAdd None None = None
occAdd None x  = x
occAdd x None  = x
occAdd _ _   = Many

-- The "number" (0, 1 or many) of free occurrences of the given variable in 
-- the given term.
-- For examples, x occurs free
-- - no times in y(%x.x)
-- - one time in  (%x.x)(%z.x)
-- - many times in (%x.x)x(%z.x)
occ :: String -> L -> Occ
occ = undefined

-- A term is "linear" if every subterm of the form (\x.e) in it has the property
-- that x occurs free exactly once in e.
-- Linear: (%x.x)(%x.x)
-- Linear: %x.%y.x(y (%x. x w w))
-- Not linear: %y. (%y.y)
-- Not linear: %y. (%x. y y)
isLinear :: L -> Bool
isLinear = undefined

-- /2


-- EXTRA CREDIT QUESTION. Partial marks will not be given for this question, so
-- don't attempt this unless you're done with all the other questions.

-- Two terms are "equivalent" if they are the same up to consistent renaming of
-- bound variables. "Consistent" means that variable binding structure has to
-- remain the same under the renaming, i.e. a bound variable is bound by the
-- same "lambda" after the renaming. So, %x.%y.x(%x. y x) is equivalent to
-- %x.%z.x(%x. z x), %w.%y.w(%x. y x) and %x.%z.x(%w. z w), but not to
-- %x.%y.x(%y. y y). Write a function that determines whether two terms are
-- equivalent. 
equiv = undefined

        