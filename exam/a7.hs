import Data.List

{-

ASSIGNMENT 7

DUE: end of Friday Nov 17.

This assignment uses a small database example to give some experience working
with "do". Recall that "do" syntax is available for any monad, but we are
interested only in two particular monad instances: Maybe and IO. 

The functions you will write will mostly have the form

f ... = do
    line1
    line2
    ...
    linen

where each line is one of special "do" lines
 
    let x=e
    x <- e 

or is any expression of the right type, as in the lectures. 

The data types in this assignment are based on lists. There are some handy
functions in Data.List that could make your life easier. Here are ones that are
used in the sample solution: intercalate, groupBy, nubBy, find, deleteBy, lines,
unlines, words.

You will also need the following.

- readFile, writeFile: read/write a string to/from a file. The contents of a
  file is just a single string, with newlines "\n" between the lines.

- read: this turns a string into another Haskell value. You should always
  specify the result type, as in 
      (read "[1,2,3]" :: [Int])
  or you might get weird unexpected typechecking errors. 

-}


-- For this assignment, a database is a list of tables. A table consists of a
-- name for the table, a schema (just the names of the columns in the table),
-- and a list of the rows in the table. Each row has to have the number of
-- columns given by the schema. 
-- After the data type definitions below, we define some functions you might
-- find useful. Please read them carefully.

data Schema = Schema { schemaCols :: [String] }

data T = T 
    { tName :: String
    , tSchema :: Schema
    , tRows :: [[Int]] 
    }

data DB = DB { dbTs :: [T] }

-- The derived show method for DB and T doesn't put in any line breaks, making a
-- printout unreadable. We give a better one to help with debugging.
instance Show T where
    show (T name (Schema cols) rows) =
        "\n" ++ name ++ "\n" ++ intercalate "," cols 
        ++ "\n" ++ intercalate "\n" (map show rows)

instance Show DB where
    show (DB tables) = intercalate "\n" (map show tables)

validRow :: Schema -> [Int] -> Bool
validRow (Schema cols) l = length cols == length l

eqKey :: [Int] -> [Int] -> Bool 
eqKey row1 row2 = head row1 == head row2

sameName :: T -> T -> Bool
sameName t1 t2 = tName t1 == tName t2 

-- The first column (i.e. the first elements of the rows), is special. In DB
-- parlance it's the "primary key", or just "key". No rows in the same table can
-- have the same key.
validT :: T -> Bool
validT (T _ schema rows) = 
    all (validRow schema) rows 
    && length rows == length (groupBy eqKey rows)

validDB :: DB -> Bool
validDB (DB tables) = 
    all validT tables
    && length tables == length (nubBy sameName tables)

-- All MonadFail mean here is that "fail" is defined for m, and the only m's we
-- care about are Maybe and IO. For Maybe, (fail "foo") is Nothing (so the "foo"
-- is tossed); for IO, it throws an error with message "foo".
failWhen :: MonadFail m => Bool -> String -> m () 
failWhen b message = if b then fail message else return () 

-- When writing a "do" block, it has to either be all IO or all Maybe. However,
-- you can convert a Maybe into an IO action: Nothing turns into an error, and
-- Just x turns into an IO action that, when it runs, returns x.
maybeIO :: Maybe a -> IO a
maybeIO Nothing = fail "maybeIO: received Nothing"
maybeIO (Just x) = return x

-- Get a list member at a given index (the first element is position 0)
-- Like the built-in !! except it uses Maybe instead of throwing an error when
-- the index is out of range.
(!?) :: [a] -> Int -> Maybe a
[] !? n | n<=0 = Nothing
(x:_) !? 0 = Just x
(_:xs) !? n = xs !? (n-1)


-------------------------------------------------------------------------------
-- Your code goes below.
--
-- NOTE:
--
-- 1. You CANNOT MENTION Just and Nothing in your code. The point here is use
--    "do" to handle the Just/Nothing bookkeeping.
--
-- 2. The code you replace "undefined" with should all be within the "do". You
--    can add any additional definitions you like.
-- ----------------------------------------------------------------------------

-- Find the row in the named table that has the given key, and return the
-- element in the named column. You can assume the db is valid.
query :: DB -> String -> Int -> String -> Maybe Int
query (DB tables) table key col = 
    do
        table <- find ((== table) . tName) tables
        row <- find ((== key) . head) (tRows table)
        colIndex <- findIndex (== col) (schemaCols (tSchema table))
        row !? colIndex


-- Add a row to the named table. Check to make sure the new row fits, i.e. it has the
-- correct length and doesn't have the same key as an existing row in the table,
-- failing (returning Nothing) otherwise.
addRow :: DB -> String -> [Int] -> Maybe DB
addRow (DB tables) name row = do
    undefined





-- Read a "simple" DB from a file.
-- A simple DB is one with a single table. A simple DB is represented as a file as follows.
--   1. The name of the file is the name of the table.
--   2. The line of the file is the list of the strings in the table's schema, separated by ",".
--   3. The remaining lines of the file are the rows of the table, represented as the
--      integers in the row separated by ",".
-- Note: you can test this in ghci directly. When you enter something of type 
-- IO DB, ghci runs the IO action and displays the DB result.
readSimpleDB :: String -> IO DB
readSimpleDB fileName = do
    undefined

-- Write a simple DB to a file.
writeSimpleDB :: DB -> IO ()  
writeSimpleDB (DB [T name (Schema cols) rows]) = do
    undefined

-- Get the simple DB from a file and do the query on it.
-- Note: FilePath is just another name for String
queryFile :: String -> Int -> String -> IO()
queryFile name key col = do
    undefined

-- Get a simple DB from a file, add a row to it using addRow, and write it back
-- to the file.
addRowFile :: String -> [Int] -> IO() 
addRowFile name row = do
    undefined


-----------------------------------------------------------------------------
-- Below is the same data as the accompanying files table1, table2 and table3
-----------------------------------------------------------------------------

table1 = T
    "table1"
    (Schema ["StudentID", "Quiz1", "Quiz2", "Quiz3"])
    [[28452,50,26,24]
    ,[22900,16,39,68]
    ,[14494,27,39,22]
    ,[23631,87,90,79]
    ,[14788,79,57,91]
    ]
    
table2 = T
    "table2"
    (Schema ["StudentID", "Quiz1", "Quiz2", "Quiz3"])
    [[20221,92,98,63]
    ,[21840,12,95,53]
    ,[29931,17,79,27]
    ,[10408,29,90,77]
    ,[32600,72,78,45]
    ]

table3 = T
    "table3"
    (Schema ["StudentID", "A1", "A2", "A3"])
    [[28452,05,62,42]
    ,[22900,61,93,86]
    ,[14494,72,93,22]
    ,[23631,78,09,97]
    ,[14788,97,75,19]
    ,[20221,29,89,36]
    ,[21840,21,59,35]
    ,[29931,71,97,72]
    ,[10408,92,09,77]
    ,[32600,27,87,54]
    ]

db = DB [table1, table2, table3]
