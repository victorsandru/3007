{-# LANGUAGE DeriveGeneric #-}

module A10Lambda where

import Data.List                      -- delete, union
import Data.Maybe                     -- isJust
import Text.ParserCombinators.ReadP   -- ignore
import Text.PrettyPrint.GenericPretty -- ignore
import Control.Monad.Trans.State.Lazy -- ignore
import System.IO.Unsafe (unsafePerformIO)

data L =   
      Var String 
    | App L  L 
    | Lam String  L  
    | Name String -- new constructor for defined names
    deriving (Eq, Generic)

-- A "definition", associating a term with a name.
data Def = Def
    {defName :: String
    ,defRHS  :: L
    }
    deriving (Eq, Generic)

-- A program is a list of definitions followed by a term to evaluate.
data Prog = Prog
    {progDefs :: [Def]
    ,progTerm :: L
    }
    deriving (Eq, Generic)

-- A custom "show" that pretty-prints a term. To see the constructors in a
-- term, use "pp".
instance Show L where
    show (Var x) = x
    show (Name x) = x
    show (App e1 e2) | isLam e1 && isNonVar e2 = 
        parens (show e1) ++ " " ++ parens (show e2)
    show (App e1 e2) | isLam e1  = 
        parens (show e1) ++ " " ++ show e2
    show (App e1 e2) | isNonVar e2 = 
        show e1 ++ " " ++ parens (show e2)
    show (App e1 e2) = 
        show e1 ++ " " ++ show e2
    show (Lam x e) = 
        "λ" ++ x ++ "." ++ show e

-- A definition (Def name e) prints as:
--    <| name = e |>
instance Show Def where
    show (Def name e) = "<| " ++ name ++ " = " ++ show e ++ " |>"

instance Show Prog where
    show (Prog defs e) = concatMap ((++ "\n") . show) defs ++ show e 

-- magic lines to enable pp for L, Def and Prog
instance Out L
instance Out Def 
instance Out Prog 

freeVars :: L -> [String]
freeVars (Var x) = 
    [x]
freeVars (Name _) =
    []  -- names aren't variables!
freeVars (App e1 e2) = 
    union (freeVars e1) (freeVars e2)
freeVars (Lam x e) =
    delete x $ freeVars e

isClosed :: L -> Bool
isClosed =
    null . freeVars

isCapitalized str =
    not (null str) 
    && head str `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- (subst e e' x) is the result of substituting e' for x in e
subst :: L -> L -> String -> L
subst (Name x) e1 y = Name x
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

-- (maybeTrace b): if b is True, then print out the string, wait for user input,
-- then continue. In either case, return x.
-- 
-- Unsafe. Very bad. Get-fired bad. Function used purely for
-- debugging/illustration purposes. 
maybeTrace :: Bool -> a -> String -> a
maybeTrace b x str =
    if b then unsafePerformIO $ do 
                putStrLn str
                getLine
                return x 
    else x

---------------------------------------------------------------------------------------
-- Parsing and pretty-printing "module" after this point. No need to look at the code. 
--
-- Useful exported functions:
--
-- parse :: String -> L
---------------------------------------------------------------------------------------


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

parens :: String -> String
parens x = "(" ++ x ++ ")"

----------------

pfailIf :: Bool -> ReadP ()
pfailIf b = if b then pfail else return ()

nextChar :: Char -> ReadP ()
nextChar c = skipSpaces >> char c >> return ()

nextChars :: String -> ReadP ()
nextChars s = skipSpaces >> mapM_ char s

isAlpha :: Char -> Bool
isAlpha c = c `elem` "abcdefghijklmnopqrstuvwxyz"

isCapAlpha :: Char -> Bool
isCapAlpha c = c `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

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

capIdParser :: ReadP String
capIdParser = do
  skipSpaces
  c1 <- satisfy isCapAlpha
  rest <- munch isAlphanum
  return $ c1:rest

nameParser :: ReadP L
nameParser = do
  id <- capIdParser
  return $ Name id

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
headParser = varParser +++ nameParser +++ parenParser

argParser :: ReadP L
argParser = varParser +++ nameParser +++ lamParser +++ parenParser
  
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
expParser = appParser <++ varParser +++ nameParser +++ lamParser +++ parenParser 

defParser :: ReadP Def
defParser = do
    nextChars "<|"
    name <- capIdParser
    nextChar '='
    rhs  <- expParser
    nextChars "|>"
    return $ Def name rhs

progParser :: ReadP Prog
progParser = do
    defs <- many defParser
    e <- expParser
    return $ Prog defs e

parseWith :: ReadP a -> String -> a
parseWith p s = 
    let result = readP_to_S p s in
    if null result then error $ "parse failed: " ++ show s
    else fst $ head result

parseL :: String -> L
parseL = parseWith expParser

parseDef :: String -> Def 
parseDef = parseWith defParser

parseProg :: String -> Prog 
parseProg = parseWith progParser


