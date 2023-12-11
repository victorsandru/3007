-- Assignment 8
-- Due Friday Nov 24

import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

-- Terms of the lambda-calculus
data L =   
      Var String 
    | App L  L 
    | Lam String  L  
    deriving Eq

example = parse "%x.%y. y w (%z. y u)"

addMap :: (Eq a) => [(a,Int)] -> [(a,Int)] -> [(a,Int)]
addMap m1 m2 =
    let 
        addPair (x,y) m | lookup x m == Nothing = 
            (x,y): m
        addPair (x,y) m = map (\(x',y') -> if x==x' then (x,y+y') else (x',y')) m
    in
    foldr addPair m2 m1

---------------------------------------------------------------------
-- Do not touch anything above --------------------------------------
---------------------------------------------------------------------

-- free variables of a term, as a set (i.e. list with no duplicates)
free :: L -> [String]
free (Var x) = [x]
free (App e1 e2) = union (free e1) (free e2)
free (Lam x e) = filter (/= x) (free e)

-- multiLam [x1,...,xn] e ≡ Lam x1 (Lam x2 ... (Lam xn e)...)
-- Example:
-- multiLam (words "x y w z") (parse "x y w z")
-- ≡ λx.λy.λw.λz.x y w z ≡ λx.(λy.(λw.(λz.(x (y (w z))))))
multiLam :: [String] -> L -> L
multiLam [] e = e
multiLam (x:xs) e = Lam x (multiLam xs e)

-- multiApp [e1,...en] ≡ (App ... (App (App e1 e2) e3) ... en)
-- Example:
-- multiApp (map Var (words "x y w z"))
-- ≡ x y w z ≡ ((x y) w) z
multiApp :: [L] -> L
multiApp = foldl1 App

-- Requirements:
-- 1) multiApp (explodeApp e) = e
-- 2) head (explodeApp e) is not an App (i.e. is not of the form (App e1 e2))
-- No examples. Part of the exercise is figuring out how to satisfy the above
-- requirements.
explodeApp :: L -> [L]
explodeApp (App e1 e2) = explodeApp e1 ++ [e2]
explodeApp e = [e]

-- Requirements:
-- 1) uncurry multiLam (explodeLam e) = e
-- 2) snd (explodeLam e) is not a Lam
explodeLam :: L -> ([String], L)
explodeLam (Lam x e) = let (vars, body) = explodeLam e in (x : vars, body)
explodeLam e = ([], e)

-- For each variable that occurs free in the given term, the number of times it occurs free.
-- Example:
-- freeCount (parse "%x.(%y.(y (%w. w z z))) (w z u (%z. z)))")
-- ≡ freeCount (parse "%x.(%y.(y (%w. w z z))) (w z u (%z. z)))")
-- freeCount :: L -> [(String, Int)]
-- freeCount = undefined
freeCount :: L -> [(String, Int)]
freeCount (Var x) = [(x, 1)]
freeCount (App e1 e2) = addMap (freeCount e1) (freeCount e2)
freeCount (Lam x e) = filter (\(y, _) -> y /= x) (freeCount e)

bound :: L -> [String]
bound (Var _) = []  -- Variables that occur free are not bound
bound (App e1 e2) = bound e1 ++ bound e2
bound (Lam x e) = x : bound e


































 
------------------------------------------------------
-- Just parsing and pretty-printing after this point.
------------------------------------------------------

-- Addresses within a term: False is first subterm, True is second (if it exists)
type Addr = [Bool]
--
-- A lens just packages up a term with an address within it
data Lens = Lens { lensL :: L, lensAddr :: Addr }

class Rep a where
    rep :: a -> L
    unrep :: L -> Maybe a

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

-- Show a lens by showing the term with some bracketing (a "box") around the addressed
-- term.
instance Show Lens where
    show = show . box

instance Show L where
    show e = show (nobox e)  -- uses the "show" for a variant of L with "boxes"

data LB =   
      VarB String 
    | AppB LB  LB 
    | LamB String LB  
    | Box LB
    deriving Eq

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


