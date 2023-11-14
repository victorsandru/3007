{-

Instructions/notes:

1. Do all your work in this file, giving definitions for "undefined" functions.
   You can add extra functions, but do *not* change any of the existing types or
   modify anything above the "DON'T TOUCH THE ABOVE" line.

2. Upload this file to Gradescope. The autograder will do its magic in real time
   (i.e. while-u-wait). There will be no manual grading for this quiz except to
   check the restrictions specified in the last two questions.  The autograder may
   give partial marks for partially working code. Submissions that can't be
   autograded, e.g.  they don't typecheck/compile, will get a grade of zero.

3. You can submit as many times as you like. Please make sure the final version
   you submit compiles (i.e. can be autograded)! If your final file doesn't
   compile, we'll have the autgrader grade the most recent version that does.

4. You can use whatever you have on your laptop or on paper, as long as you are
   not using AI tools, or making any network connections except for the
   officially allowed ones (CoMaS, gradescope, brightspace, learnyouahaskell.com
   and the course website).

5. Don't panic. The quiz might look long, but that's because the explanations
   and instructions are rather verbose and include examples and some code from
   the assignments.  There are only five questions and the amount of code to write
   is *very* small.

-}


-- Verbatim from Assignment 3.
-- A data type representing a mapping from keys k::a to values v::b. A key can
-- occur at most once in a member of Map a b. 
data Map a b = Empty | Add a b (Map a b) deriving (Show,Eq)

-- A variant of Assignment 3's Backpack type.
-- A totebag is a bag of items. Items are named by a string. The data type Totebag
-- represents a totebag as a mapping from the name of an item to the number of
-- that item in the bag.
type TotebagItems = String
data Totebag = Totebag (Map TotebagItems Int) deriving (Eq, Show)

-- A variant of Assignment 4's Rating type.  A representation of a bank account,
-- including the owner's id, the balances in the chequing and savings parts of
-- the account, and a flag saying whether the client has enriched the bank by
-- purchasing a generously overpriced "VIP" upgrade.
data Ac = Ac 
    { acId :: String
    , acSavings :: Int
    , acChequing :: Int
    , acVIP :: Bool
    }

-- Verbatim from Assignment 3.
apply :: Eq a => Map a b -> a -> Maybe b
apply Empty x = 
    Nothing
apply (Add u v m) x = 
    if x==u then Just v 
    else apply m x

-- Verbatim from Assignment 3.
applyElse :: Eq a => Map a b -> a -> b -> b
applyElse m x d =
    case apply m x of
        Nothing -> d
        Just v  -> v

-- Verbatim from Assignment 3.
update :: Eq a => a -> b -> Map a b -> Map a b 
update x y Empty = 
    Add x y Empty
update x y (Add u v m) = 
    if x==u then Add x y m
    else Add u v (update x y m)

-- EXAMPLES FOR TESTING

sampleMap1 :: Map String Int
sampleMap1 = Add "headphones" 1 (Add "shawarma" 17 (Add "phaser" 1 Empty))

sampleMap2 :: Map String Int
sampleMap2 = Add "earbuds" 1 (Add "shawarma" 4 (Add "BFG9000" 2 Empty))

sampleTotebag1 :: Totebag
sampleTotebag1 = Totebag sampleMap1

sampleTotebag2 :: Totebag
sampleTotebag2 = Totebag sampleMap2

-- Sample list of accounts for the last two questions.
acs :: [Ac]
acs = 
    [ Ac "Archibald" 1700 0 False
    , Ac "Barnaby" 0 1700 False
    , Ac "Beatrix" 1700 1700 False
    , Ac "Bertram" 1800 1900 False
    , Ac "Genevieve" 0 0 True
    , Ac "Ludo" 0 0 False
    , Ac "Octavia" 323 444 False
    , Ac "Ottilie" 2 34 False
    , Ac "Sebastian" 0 13 False
    , Ac "Theodore" 0 17000000 False
    , Ac "Violet" 0 0 False
    , Ac "Xanthe" 555 777 False
    ]

-- DON'T TOUCH THE ABOVE! THE AUTOGRADER WILL KNOW!
---------------------------------------------------

-- 7003

-- Specification: (member k m) ≡ False if and only if (apply m k) ≡ Nothing
-- Examples:
--   member "shawarma" sampleMap1 ≡ True
--   member "car keys" sampleMap1 ≡ False
member :: Eq a => a -> Map a b -> Bool
member memb m = 
    case apply m memb of
        Nothing -> False
        Just v -> True

-- Specification: (modify f k m) is a map m' such that 
--   1. (apply m' k) ≡ f (apply m k) and
--   2. (apply m' k') ≡ apply m k if k' ≠ k.
-- Examples:
--   modify (+1) "shawarma" sampleMap1 ≡ Add "headphones" 1 (Add "shawarma" 18 (Add "phaser" 1 Empty))
--   modify (+1) "car keys" sampleMap1 ≡ Add "headphones" 1 (Add "shawarma" 17 (Add "phaser" 1 Empty))
modify :: Eq a => (b -> b) -> a -> Map a b -> Map a b
modify f key map = 
    undefined
    -- if member key map == False then map
    -- else update key (f key) map

-- Combine the contents of two totebags into one.  
-- You should probably leave this question until the end unless you've
-- successfully implemented the previous functions.
-- Example:
--   combineTotebags sampleTotebag1 sampleTotebag2
--   ≡ Totebag 
--       (Add "headphones" 
--             1 
--             (Add "phaser" 
--                  1 
--                  (Add "earbuds" 
--                       1 
--                       (Add "shawarma" 
--                            21 
--                            (Add "BFG9000" 
--                                  2 
--                                  Empty)))))
--   
combineTotebags :: Totebag -> Totebag -> Totebag 
combineTotebags = undefined
 
-- Returns a list of pairs (id,amount) where id is an account id and amount is
-- the sum of the values in the checking and savings part of the account. Each
-- account in the input list should have a unique corresponding pair in the
-- output.  
-- RESTRICTION: no recursion. The defintion can't be recursive, nor cqn you
-- define a recursive "helper" function (non-recursive helpers are fine).  Also,
-- no list comprehension! Your definition should use map and or filter.
totalBalances :: [Ac] -> [(String, Int)]
totalBalances acs = 
    map eachTotalBalance acs
eachTotalBalance :: Ac -> (String, Int)
eachTotalBalance ac = 
    (acId ac, acSavings ac + acChequing ac)





-- a list of the deadbeats in an account list. A deadbeat is someone who has
-- no money in either savings or chequing *and* is *not* a VIP.
-- Example: deadbeats acs ≡ ["Ludo","Violet"].
-- RESTRICTION: no recursion. The defintion can't be recursive, nor cqn you
-- define a recursive "helper" function (non-recursive helpers are fine). Also,
-- no list comprehension! Your definition should use map and or filter.
deadbeats :: [Ac] -> [String]
deadbeats acs = 
    map acId (getDeadbeats acs)
    -- map acId acs

getDeadbeats :: [Ac] -> [Ac]
getDeadbeats acs =
    filter (\each -> acChequing each == 0 && acSavings each == 0 && acVIP each == False) acs
