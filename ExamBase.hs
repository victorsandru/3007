module ExamBase where
import Data.List
import Text.ParserCombinators.ReadP


-- A parts database is either empty or it consists of entry containing a
-- part name, a description, a price, and the rest of the database.
data PartsDB = Empty | Entry String String Int PartsDB
    deriving (Eq, Show)

db = 
    Entry "TRD-011" "Axiomator" 45 $
    Entry "TRD-998" "Randomiser" 0 $
    -- Entry "TRD-001" "Vortex drive" 3000 $
    -- Entry "TRD-342" "Gravitic anomaliser" 342 $
    -- Entry "TRD-534" "Holoprobe" 43 $
    -- Entry "TRD-522" "Tachyon beam collimator" 43 $
    -- Entry "TRD-349" "Tremulator" 98 $
    Empty 


dbTuples :: [(String, String, Int)]
dbTuples =
    [("TRD-011","Axiomator",45)
    ,("TRD-998","Randomiser",0)
    ,("TRD-001","Vortex drive",3000)
    ,("TRD-342","Gravitic anomaliser",342)
    ,("TRD-534","Holoprobe",43)
    ,("TRD-522","Tachyon beam collimator",43)
    ,("TRD-349","Tremulator",98)
    ]

data Name = Name String deriving (Eq, Show)

-- The class of types where each member has a name that can be obtained using
-- the "Name" function.
class Named a where
    name :: a -> Name

-- Part partName description price 
data Part = Part String String Int 
    deriving (Eq, Show)

-- Example.
partsList = 
    map (\(x,y,z) -> Part x y z) dbTuples

-- An "association list", or map, giving the supervisor of each employee in the
-- company. The supervisor is Nothing if the employee doesn't have a supervisor. 
-- Note that (lookup name supervisors) is of type Maybe (Maybe String) since the
-- supervisors map gives a maybe value for name's supervisor, and the lookup
-- itself will fail if name is not in the map.
type SupervisedBy = [(String, Maybe String)]

sups :: SupervisedBy
sups = 
    [("alice", Just "bob")
    ,("bob", Just "carol")
    ,("dan", Just "carol")
    ,("carol", Nothing)
    ]


data L =   
      Var String 
    | App L  L 
    | Lam String  L  
    deriving (Show, Eq)

-- Enumerated type for classifying the number of occurrences of something: No
-- occurrences, One occurrence, Many (more than one) occurrences.
data Occ = None | One | Many
    deriving (Show,Eq)

---------------------------------------------------------------------------------------
-- Parsing and pretty-printing "module" after this point. No need to look at the code. 
--
-- Exported function:
--
--   parse :: String -> L
---------------------------------------------------------------------------------------

pfailIf :: Bool -> ReadP ()
pfailIf b = if b then pfail else return ()

nextChar :: Char -> ReadP ()
nextChar c = skipSpaces >> char c >> return ()

isAlpha :: Char -> Bool
isAlpha c = c `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

isNum :: Char -> Bool
isNum c = c `elem` "0123456789"

isAlphanum :: Char -> Bool
isAlphanum c =  isNum c || isAlpha c
             
idParser :: ReadP String
idParser = do
  skipSpaces
  c1 <- satisfy isAlpha
  rest <- munch isAlpha
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



