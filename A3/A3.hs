import Prelude 
    (String
    ,Int, (<), (<=), max, min, (+), (-), (*), rem
    ,Char
    ,Bool(..), (==), (&&), (||), not
    ,Eq, Show
    ,length, error
    ,(.)
    ,IO, (>>), (>>=), return, ($), flip, mapM, id, print
    ,Real, toRational, floor, replicate
    ,undefined
    )

import Data.Time.Clock
import GHC.Real

{-^!#!^-}

--------------------------
-- DON'T TOUCH THE ABOVE!
--------------------------

-- Assignment 3
--
-- Due: Thursday Oct 5 23:59
--
-- Submission instructions: you can modify anything you like, *except* for the
-- existing type declarations. You don't have to just replace the "undefined"
-- parts, as in previous assignments. I.e. you can change what you like about
-- the functions below as long as you don't change their types. Also, you can
-- add whatever you want. Note the "import" statement: most of the standard
-- library is hidden.

-- Haskell package nuisance: you need a ghci config file to "unhide" the
-- packages this file imports. Create a file called ".ghci" with the following
-- two lines as its contents
--
-- :set -package time
-- :set -package base
--
-- and put it either in your home directory (cd $HOME) or in the same directory
-- where you'll be running ghci.

-- This assignment is to write a simulator for a garbage-packing wombat. The
-- simulator uses randomness which requires use of the "IO monad". However, you
-- don't need to know anything about that. You'll just be writing the purely
-- functional parts.
--
-- The wombat wanders randomly up and down a path. The path is divided into
-- "steps". The wombat goes from step to step. A step may have a piece of
-- garbage in it.  The kind of garbage a wombat likes (we assume): bug
-- carcasses, pizza crusts, and bottle caps. Call these "goodies" in deference
-- to the wombat. At each step the wombat picks up garbage and puts it in their
-- backpack. 
--
-- The wombat goes in the same direction until they get poked. They reverse
-- direction when they get poked. If they try to move off the end of the path,
-- they get stuck there until poked.

-- The simulation starts with a randomly generated path of a specified length.
-- The simulation unfolds as a sequence of (attempted) moves. Before each move,
-- there is a chance of a poke, and then the wombat is moved to the next square
-- (unless stuck) in the direction it's moving in, where it picks up any garbage
-- there.
--
-- The path is modelled as a list. Each element represents a step. The Maybe
-- type constructor is used to model the possible presence of an item of
-- garbage. The wombat's backpack is modeled as a mapping that gives, for each
-- goody, the number of that goody collected so far. The main data type is
-- "State". This models the state of the simulation. Your main job is to
-- implement the transition function, i.e. the operation that performs a move,
-- updating the state accordingly.

-- The simulator creates a random state then repeatedly applies the transition
-- function, each time giving the wombat a poke with probability
-- 1/pokeProbability.  To run it, do 
--
-- ghci>  simulate n k
--
-- where n is the size of the path you want, and k is the number of steps to
-- take.
--
-- Of course, the simulator won't work until you've finished the assignment. For
-- debugging and testing, you can do
--
-- ghci> randomPath 10
--
-- to get a random path of length 10 (or whatever size you want). You can
-- cut-and paste this and name it; it's a Haskell data value. Note: this
-- function only works at the ghci prompt. You will get a type error if you try
-- to use it inside another function.
--
---------------------------------------------------------------------------------


-- Map a b is a representation of a mapping from type a to type b. This
-- structure is called a dictionary in other languages, with the keys being of
-- type a and the values being of type b. The operations must be defined so that
-- no key appears twice (as the first argument to an Add). Example:
--
--      Add "x" 2 (Add "y" 3 Empty) 
--
-- is a value representing the mapping where key x has value 2
-- and key y has value 3.
data Map a b = Empty | Add a b (Map a b) deriving (Show,Eq)

-- Your code shouldn't mention these constructors. I.e. your code should be
-- abstract with respect to what's in the Goody type, so that adding more
-- goodies won't require any change to your code.
data Goody = BugCarcass | PizzaCrust | BottleCap 
    deriving (Show, Eq) -- automatically derive implementations of show
                        -- and == for this data type

-- Same as Haskell's built-in type of the same name. 
data Maybe a = Nothing | Just a
    deriving (Show, Eq)

-- The wombat's backpack is represented as a mapping that gives the number of
-- each kind of goody stored there. 
data Backpack = Backpack (Map Goody Int)
    deriving (Show, Eq)

-- The State type represents the state of the simulation, which consists of the
-- wombat's position on the path, the direction it is travelling, and its
-- backpack contents. The path is represented a list where each element
-- corresponds to a "step" on the path and says what kind of goody is there
-- (Nothing if none). The steps are numbered starting at 0.
--
-- State pathWithMaybeGoodies wombatPosition wombatIsGoingRight wombatBackpack 
data State = State [Maybe Goody] Int Bool Backpack deriving (Show, Eq)

---------------------------------

-- (apply m x) is Just of the value associated with key x in m, Nothing if there is no
-- such value.
-- Example: apply (Add "x" 2 (Add "y" 3 Empty)) "x" ==> Just 3
-- Example: apply (Add "x" 2 (Add "y" 3 Empty)) "z" ==> Nothing
-- The "Eq a =>" means apply has the given type for any type a for which "==" is
-- defined.
apply :: Eq a => Map a b -> a -> Maybe b
apply Empty _ = Nothing
apply (Add k v rest) x = 
    if k == x then Just v
    else apply rest x



-- (applyElse m x d) is the value associated with key x in m, d if there is no
-- such value.
applyElse :: Eq a => Map a b -> a -> b -> b
applyElse Empty _ d = d
applyElse (Add k v rest) x d =
    if k == x then v
    else applyElse rest x d

-- (update x y m) is a map m' such that
-- apply m' x ==> y
-- apply m' z ==> apply m z   if z is different than x
update :: Eq a => a -> b -> Map a b -> Map a b
update _ _ Empty = Empty 
update key nValue (Add k v rest) =
    if k == key then Add key nValue rest
    else Add k v (update key nValue rest)

-- (deleteGoody gs n) is the same as gs except the element at position n is
-- replaced by Nothing. Note: the first element of a list is at postion 0.
deleteGoody :: [Maybe Goody] -> Int -> [Maybe Goody]
deleteGoody [] _ = [Nothing]
deleteGoody (g:gs) n = 
    if n == 0 then gs
    else g : deleteGoody gs (n - 1)

-- (stowGoody gs n bp) is bp if the goody at position n in gs is Nothing, else it
-- is bp with the value of the corresponding key in bp incremented by 1.
stowGoody :: [Maybe Goody] -> Int -> Backpack -> Backpack
stowGoody [] _ _ = [Nothing]
stowGoody (g:gs) n bp =
    if n == 0 then
        if g == Nothing then Nothing
        else 

-- (transition poke state) is the state resulting from moving the wombat to a new
-- position and collecting the goody there, if any. "Collecting the goody"
-- involves updating the path to show the goody is gone, and updating the
-- wombat's backpack to show it's there. Before the move, the wombat's direction
-- is changed exactly if "poke" is true. The move is 1 position to the right,
-- e.g. 2 -> 3, if the move direction is true, otherwise it is 1 to the left,
-- e.g. 3 -> 2. If the move would take the wombat past one of the ends of the
-- path (<0 or > length of the path) then the wombat doesn't change position.
transition :: Bool -> State -> State
transition = undefined



-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
    
-- Random number k, 0 <= k < n 
rand :: Int -> IO Int
rand n = 
    if 1000000 < n then error "rand: n too big"
    else do 
        t <- getCurrentTime
        let k = flip rem 100000000 . fromIntegral . numerator . toRational . utctDayTime $ t
        return $ rem (div k 100) n

-- probabality of coin k == True is 1/k (roughly)
coin :: Int -> IO Bool
coin k =
    if k < 2 then error "coin: k too small"
    else do {n <- rand (100*k); return $ n<100 }

randomGoody :: IO (Maybe Goody)
randomGoody = do
    k <- rand 4
    case k of
        0 -> return Nothing
        1 -> return $ Just BugCarcass 
        2 -> return $ Just PizzaCrust 
        3 -> return $ Just BottleCap

randomPath :: Int -> IO ([Maybe Goody])
randomPath n = 
    mapM id (replicate n randomGoody) 

simulate :: Int -> Int -> IO () 
simulate pathSize bound = do
    path <- randomPath pathSize
    let initialState = State path (div pathSize 2) True (Backpack Empty)
    print initialState 
    step bound 0 initialState
    return ()

pokeProbability = 5 -- 1/

step :: Int -> Int -> State -> IO State
step bound i s | i==bound =
    return s
step bound i s = do
    poke <- coin pokeProbability 
    let next = transition poke s
    print next
    step bound (i+1) next
    
     
  
