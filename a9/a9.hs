{-# LANGUAGE DeriveGeneric #-}

-- Assignment 9
--
-- Due Sunday Dec 2. However, try to do it earlier since there will be overlap with
-- Assignment 10.
--
-- Do the "undefined" parts.
--
-- There's some code to read. Be sure to read *all* the comments. Some introduce some new
-- terminology, or give some essential examples.
--
-- You might need the Tuesday Nov 28 lecture to complete the assignment, but a big chunk
-- can be done without it.
--
-- The assignment uses lenses to implement a reduction strategy called "leftmost
-- innermost" (defined below), which roughly corresponds to the call-by-value evaluation
-- (evaluate function arguments before doing the function call) used in most programming
-- languages. 

-- You don't need anything from these libraries. The comments show what's used in this
-- file or sample solution.
import Data.List                      -- delete, union
import Data.Maybe                     -- isJust
import Text.ParserCombinators.ReadP   -- ignore


-- ***********************************************************************
-- DO NOT TOUCH ANYTHING ABOVE THIS LINE ---------------------------------


----------------------------------------------------------------
-- Data types 

-- Terms of the lambda-calculus
-- This file includes a custom "show" method for L that displays the term as an unparsed
-- string. If you want to see the constructors, change "deriving Eq" to "deriving 
-- (Eq, Show) and delete the Show L instance later in the file.
data L =   
      Var String 
    | App L  L 
    | Lam String  L  
    deriving Eq

-- Addresses within a term: False is first subterm, True is second (if it exists)
type Addr = [Bool]

-- A lens just packages up a term with an address within it
data Lens = Lens { lensL :: L, lensAddr :: Addr } deriving Eq

-- end of data types
----------------------------------------------------------------


----------------------------------------------------------------
-- A bunch of examples of lambda terms

e1 = 
    parse "λx.λy.(y (λw. x y (z w)) (w (z x) (λz. z)))"
e2 = 
    parse "(λx. (λy.x y)(x(λx. x z))) w"
e3 = 
    parse "x (λx. (λy. y x)(λx.x))"
fp = 
    let e = parse "λx.f(x x)" in App e e 
addrExample =
    parse "f g ((λx. (λx. x v) z) h)"

numL :: Int -> L
numL i = Lam "f" $ Lam "x" (foldr App (Var "x") (replicate i (Var "f")))

lsucc = parse "λm. λf.λx. f (m f x)"

ladd = subst (parse "λm.λn. m s n") lsucc "s"

l18Exercise =
    parse "(λx.λy.λz. x z (y (z z) z)) (λx.λy.y) (λx.λy.x) (λw.w)"

-- For building lambda-term examples.
-- E.g. appStar [ladd, e1, e2] is a term that, when normalized, gives (the representation
-- of) the sum of e1 and e2, assuming e1 and e2 represent numbers.
lapp :: [L] -> L
lapp = foldl1 App

additionExample = lapp [ladd, numL 3, numL 4]

-- end of examples
----------------------------------------------------------------


-- The free variables of a term
freeVars :: L -> [String]
freeVars (Var x) = 
    [x]
freeVars (App e1 e2) = 
    union (freeVars e1) (freeVars e2)
freeVars (Lam x e) =
    delete x $ freeVars e

isClosed :: L -> Bool
isClosed =
    null . freeVars

-- (subst e e' x) is the result of substituting e' for x in e
subst :: L -> L -> String -> L
subst (Var x) e1 y | y==x =
    e1
subst (Var x) _ _ =
    Var x
subst (App e1 e2) e y = 
    App (subst e1 e y) (subst e2 e y)
subst (Lam x e1) e2 y | y==x = 
    Lam x e1 
subst (Lam x e1) e2 y =
    Lam x (subst e1 e2 y)

-- Is a term a beta-redex? 
isBeta :: L -> Bool
isBeta (App (Lam _ _) _) = True
isBeta _ = False

-- Reduce a beta-redex
doBeta :: L -> L
doBeta (App (Lam x e1) e2) =
    subst e1 e2 x
doBeta e = 
    e

-- Can the address be followed to its end in the term?
legalAddr :: L -> Addr -> Bool
legalAddr _ [] = True
legalAddr (App e _) (False:bs) = legalAddr e bs
legalAddr (App _ e) (True:bs) = legalAddr e bs
legalAddr (Lam _ e) (False:bs) = legalAddr e bs
legalAddr _ _ = False


-- Is the address in the lens legal for its term.
legalLens :: Lens -> Bool
legalLens (Lens e addr) = legalAddr e addr

-- Apply the given function to the addressed subterm, returning the modified term.
mapAddr :: (L -> L) -> L -> Addr -> L
mapAddr f e [] = f e
mapAddr f (App e1 e2) (False:bs) = App (mapAddr f e1 bs) e2
mapAddr f (App e1 e2) (True:bs) = App e1 (mapAddr f e2 bs)
mapAddr f (Lam x e) (False:bs) = Lam x (mapAddr f e bs)
mapAddr _ e _ = e

-- Lens version of mapAddr
-- Example: if f x = App x x then mapping f on the lens λx.x[[y]] returns λx.x[[yy]]
mapLens :: (L -> L) -> Lens -> Lens
mapLens f (Lens e addr) = Lens (mapAddr f e addr) addr

-- Reduce the beta-redex at the lens.
reduceLens :: Lens -> L
reduceLens =
    lensL . mapLens doBeta

-- Compute a lens that gives the location of the next "leftmost-outermost" redex.
-- Terminology:
--     - a redex is "outermost" if it is not contained in another redex
--     - a redex is "innermost" if it does not contain another redex
--     - the "leftmost" of a set of redexes is the one that's furthest to the left when
--       the term is written out as string. 
-- So the "leftmost-outermost" redex is the leftmost of all the outermost redexes.
-- Example:
-- (λz. (λx.x)y) ((λx.x)y) ((λx.x)y)
--                          ^^^^^^^--> redex 4
--                ^^^^^^^--> redex 3
--      ^^^^^^^--> redex 2
-- ^^^^^^^^^^^^^^^^^^^^^^^--> redex 1
-- Redexes 1 and 4 are outermost.
-- Redex 1 is leftmost-outermost.
-- Redexes 2,3 and 4 are innermost. 
-- Redex 2 is leftmost-innermost.
findLORedex :: L -> Maybe Lens
findLORedex e = do
    addr <- findLORedex' e
    return $ Lens e addr

-- Lens-less helper for findLORedex
findLORedex' :: L -> Maybe Addr 
findLORedex' (App (Lam _ _) _) = Just []
findLORedex' (App e1 e2) | isJust (findLORedex' e1) = 
    fmap (False :) $ findLORedex' e1
findLORedex' (App e1 e2) =
    fmap (True :) $ findLORedex' e2
findLORedex' (Lam x e) =
    fmap (False :) $ findLORedex' e
findLORedex' _ = Nothing

-- Instead of digging through the term to pick a redex, we're going to collect all the
-- redexes as lenses and compare them. The comparisons will be on addresses and will 
-- correspond to the notions of "innermost", "leftmost" etc from above.
--
-- (belowAddr addr1 addr2) is true if addr1 addresses a tree node below the one addressed by
-- addr2. By "tree node" here we mean a node in some tree that addr1 and addr2 are both
-- legal addresses for. 
belowAddr :: Addr -> Addr -> Bool 
belowAddr [] _ = False
belowAddr _ [] = True
belowAddr (x:xs) (y:ys) = 
    if x == y then belowAddr xs ys
    else False

-- lens version of belowAddr
below :: Lens -> Lens -> Bool
below (Lens e1 a1) (Lens e2 a2) =
    e1 == e2 && belowAddr a1 a2

-- leftAddr addr1 addr2 is true if addr1 addresses a node to the left of the one addressed
-- by addr2. In any tree T, we say that node n1 is to the "left" of node2 if the paths
-- from the root of the tree to n1 and n2 are the same up to some node x, at which point the
-- path to n1 goes to a child of x that's to the left of the one that the path to n2 goes to. 
leftAddr :: Addr -> Addr -> Bool 
leftAddr (x:xs) [] = 
    if x == False then True
    else False
leftAddr [] _ = False
leftAddr (x:xs) (y:ys) = 
    if x == y then leftAddr xs ys
    else if x == False then True
    else False

left :: Lens -> Lens -> Bool
left (Lens e1 a1) (Lens e2 a2) =
    e1 == e2 && leftAddr a1 a2

-- Find all the lenses who addressed term satifies the predicate. 
-- Hint: use a helper function that just computes the *addresses* of all the subterms satisfying 
-- the predicate.
findAll :: (L -> Bool) -> L -> [Lens]
findAll ped e = map (Lens e) (findAll' ped e [])

findAll' :: (L -> Bool) -> L -> Addr -> [Addr]
findAll' ped (Var _) _ = []
findAll' ped (App e1 e2) addr =
    let
        leftSubterm = findAll' ped e1 (addr ++ [False])
        rightSubterm = findAll' ped e2 (addr ++ [True])
    in
        if ped (App e1 e2) then addr : leftSubterm ++ rightSubterm
        else leftSubterm ++ rightSubterm

findAll' ped (Lam _ e) addr =
    let
        subterm = findAll' ped e (addr ++ [False])
    in
        if ped (Lam "" e) then addr : subterm
        else subterm



findAllRedexes :: L -> [Lens]
findAllRedexes = findAll isBeta

-- Return all the members of l that are minimal with respect to lessThan. A member x of l
-- is minimal if there is no smaller member of l, i.e. there is no y /= x in l with
-- (lessThan y x).
getMinima :: Eq a => (a -> a -> Bool) -> [a] -> [a] 
getMinima lessThan l =
    filter (\x -> all (\y -> not (lessThan y x)) l) l

-- Find the leftmost-innermost redex in the term, if it exists. Hint: use findMinima with left
-- and below to choose from the list given by findAllRedexes.
findLIRedex :: L -> Maybe Lens
findLIRedex term = do
    let redexes = findAllRedexes term
    let minima = getMinima (\l1 l2 -> left l1 l2 || below l1 l2) redexes
    case minima of
        []      -> Nothing
        (x : _) -> Just x

-- Compute a sequence of pairs (lens(i), e(i)), 1 ≤ i such that the term of lens(1) is e,
-- and for each i, e(i) is the term of lens(i+1) and is the result of reducing lens(i).
-- Example: 
-- ghci> printReductionPairs (reductionPairs findLORedex (parse "(λz. (λx.x)y) ((λx.x)y) ((λx.x)y)" ))
-- [[(λz.(λx.x) y) ((λx.x) y)]] ((λx.x) y)
-- ==> (λx.x) y ((λx.x) y)

-- [[(λx.x) y]] ((λx.x) y)
-- ==> y ((λx.x) y)

-- y ([[(λx.x) y]])
-- ==> y y
-- NOTE: if reduction doesn't terminate, that's ok! The list will be infinite, so don't
-- try to see it all in ghci, but you can use the "take" function to get as many elements
-- as you want.
reductionPairs :: (L -> Maybe Lens) -> L -> [(Lens, L)]
reductionPairs redexFinder e = undefined
    -- case redexFinder e of
    --     Nothing -> []
    --     Just lens -> (lens, reduceLens lens) : reductionPairs redexFinder (reduceLens lens)


-- The input term followed by the second elements of the reduction pairs.
reductions :: (L -> Maybe Lens) -> L -> [L]
reductions redexFinder e =     undefined
    -- e : map snd (reductionPairs redexFinder e)

-- May fail to terminate.
normalize :: (L -> Maybe Lens) -> L -> L
normalize findRedex =
    last . reductions findRedex

printReductionPair :: (Lens, L) -> IO()
printReductionPair (lens, e) = do
    putStrLn $ show lens
    putStrLn $ "==> " ++ show e
    putStrLn ""

printReductionPairs :: [(Lens, L)] -> IO()
printReductionPairs =
    mapM_ printReductionPair

-- Prints all the reduction pairs, but requires user input to do each step.
traceReductions :: (L -> Maybe Lens) -> L -> IO()
traceReductions findRedex e = 
    let step []     = putStrLn "No more beta-redexes found, exiting."
        step (r:rs) = do
            printReductionPair r
            putStrLn "Type enter/return for next step"
            getLine
            step rs
    in
    step $ reductionPairs findRedex e

-- Normalize a term using the leftmost-innermost strategy
-- ghci> normalizeLI (lapp [ladd, numL 3, numL 4])
-- λf.λx.f (f (f (f (f (f (f x))))))
normalizeLI :: L -> L
normalizeLI =
    normalize findLIRedex

traceLI :: L -> IO()
traceLI =
    traceReductions findLIRedex

-- The number i, for i ≥ 0, is represented in L by (numL i).  Try a few examples in ghci
-- and to see the pattern. 
-- lNum is the inverse: it should return i on (numL i), and 0 on any term that does not
-- represent a number.
lNum :: L  -> Int
lNum (Lam "f" (Lam "x" (App (Var "f") e))) =
    1 + lNum (Lam "f" (Lam "x"  e))
lNum _ = 0

-- Convert args to their representations in L, apply e to them, normalize the application
-- term, then convert the result back to a number. 
-- Example: runLambdaProgram findLIRedex ladd [3,4] = 7
runLambdaProgram :: (L -> Maybe Lens) -> L -> [Int] -> Int
runLambdaProgram redexFinder e args =
    let
        argTerms = map numL args
        appTerm = foldl App e argTerms
        normalizedTerm = normalize redexFinder appTerm
        result = lNum normalizedTerm
    in
        result

runLI = runLambdaProgram findLIRedex
runLO = runLambdaProgram findLORedex

---------------------------------------------------------------------------------------
-- Parsing and pretty-printing "module" after this point. No need to look at the code. 
--
-- Useful exported functions:
--
-- parse :: String -> L
---------------------------------------------------------------------------------------

-- Add a "box" constructor for L for pretty-printing purposes.
data LB =   
      VarB String 
    | AppB LB  LB 
    | LamB String LB  
    | Box LB
    deriving Eq

class Rep a where
    rep :: a -> L
    unrep :: L -> Maybe a

instance Show L where

    show e = show (nobox e)  -- uses the "show" for a variant of L with "boxes"

-- Show a lens by showing the term with some bracketing (a "box") around the addressed
-- term.
instance Show Lens where
    show = show . box

instance Rep Bool where
    rep False = Lam "x" (Lam "y" (Var "x"))
    rep True = Lam "x" (Lam "y" (Var "x"))
    unrep e | e == rep False = Just False
    unrep e | e == rep  True = Just   True
    unrep _ = Nothing


isVar :: L -> Bool
isVar (Var _) = True
isVar _ = False

isApp :: L -> Bool
isApp (App _ _) = True
isApp _ = False

isLam :: L -> Bool
isLam (Lam _ _) = True
isLam _ = False

isNonVar :: L -> Bool
isNonVar = not . isVar

isVarB :: LB -> Bool
isVarB (VarB _) = True
isVarB _ = False

isAppB :: LB -> Bool
isAppB (AppB _ _) = True
isAppB _ = False

isLamB :: LB -> Bool
isLamB (LamB _ _) = True
isLamB _ = False

isNonVarB :: LB -> Bool
isNonVarB = not . isVarB

debox :: LB -> L
debox (VarB x) = Var x
debox (AppB e1 e2) = App (debox e1) (debox e2)
debox (LamB x e) = Lam x (debox e)
debox (Box e) = debox e

nobox :: L -> LB
nobox (Var x) = VarB x
nobox (App e1 e2) = AppB (nobox e1) (nobox e2)
nobox (Lam x e) = LamB x (nobox e)

box :: Lens -> LB
box (Lens e []) = Box $ nobox e
box (Lens (Var x) _) = VarB x
box (Lens (App e1 e2) (False:bs)) = AppB (box (Lens e1 bs)) (nobox e2)
box (Lens (App e1 e2) (True:bs)) = AppB (nobox e1) (box (Lens e2 bs))
box (Lens (Lam x e) (False:bs)) = LamB x (box (Lens e bs))
box (Lens e _) = nobox e

parens :: String -> String
parens x = "(" ++ x ++ ")"

instance Show LB where
    show (VarB x) = x
    show (AppB e1 e2) | isLamB e1 && isNonVarB e2 = 
        parens (show e1) ++ " " ++ parens (show e2)
    show (AppB e1 e2) | isLamB e1  = 
        parens (show e1) ++ " " ++ show e2
    show (AppB e1 e2) | isNonVarB e2 = 
        show e1 ++ " " ++ parens (show e2)
    show (AppB e1 e2) = 
        show e1 ++ " " ++ show e2
    show (LamB x e) = 
        "λ" ++ x ++ "." ++ show e
    show (Box e) = 
        "[[" ++ show e ++ "]]"


pfailIf :: Bool -> ReadP ()
pfailIf b = if b then pfail else return ()

nextChar :: Char -> ReadP ()
nextChar c = skipSpaces >> char c >> return ()

isAlpha :: Char -> Bool
isAlpha c = c `elem` "abcdefghijklmnopqrstuvwxyz"

isNum :: Char -> Bool
isNum c = c `elem` "0123456789"

isAlphanum :: Char -> Bool
isAlphanum c =  isNum c || isAlpha c
             
idParser :: ReadP String
idParser = do
  skipSpaces
  c1 <- satisfy isAlpha
  rest <- munch isAlphanum
  return $ c1:rest

varParser :: ReadP L
varParser = do
  id <- idParser
  pfailIf $ id == "lambda" 
  return $ Var id

lamLambdaToken :: ReadP ()
lamLambdaToken =
  (idParser >>= pfailIf . (/= "lambda"))
  +++ nextChar '%'
  +++ nextChar 'λ'

lamDot :: ReadP ()
lamDot = nextChar ':' +++ nextChar '.'

lamParser :: ReadP L
lamParser = do
    lamLambdaToken
    ids <- sepBy1 idParser (nextChar ',')
    lamDot 
    body <- expParser
    return $ foldr Lam body ids
 
parenParser :: ReadP L
parenParser =
  between (nextChar '(') (nextChar ')') expParser

headParser :: ReadP L
headParser = varParser +++ parenParser

argParser :: ReadP L
argParser = varParser +++ lamParser +++ parenParser
  
argsParser :: ReadP [L]
argsParser =
  args1Parser <++ return []

args1Parser :: ReadP [L]
args1Parser =
  do e <- argParser
     es <- argsParser
     return $ e:es

appParser :: ReadP L
appParser =
  do e <- headParser
     args <- args1Parser
     return $ foldl1 App $ e : args
               
expParser :: ReadP L
expParser = appParser <++ varParser +++ lamParser +++ parenParser 

parseWith :: ReadP a -> String -> a
parseWith p = fst . head . readP_to_S p

parse :: String -> L
parse = parseWith expParser



