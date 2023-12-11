lastElement :: [a] -> a
lastElement [x] = x
lastElement (_:xs) = lastElement xs

sLastElement :: [a] -> a
sLastElement (x:xs) =
    if length xs == 1 then x
    else sLastElement xs

kElement :: [a] -> Int -> a
kElement (x:xs) n =
    if n == 0 then x
    else kElement xs (n - 1)

lenList :: [a] -> Int
lenList [] = 0
lenList (x:xs) = 1 + lenList xs

reverseList :: [a] -> [a]
reverseList = foldl (\a x -> x:a) []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = 
    if reverseList list == list then True
    else False

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x ) = [x]
flatten (List xs) = foldl (++) [] $ map flatten xs

compress :: Eq a => [a] -> [a]
compress x = foldr (\a b -> if a == (head b) then b else a:b) [last x] x

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch a Empty Empty) = [a]
leaves (Branch a l r) = leaves l ++ leaves r

digitToIntChar :: Char -> String
digitToIntChar '0' = "zero"
digitToIntChar '1' = "one"
digitToIntChar '2' = "two"
digitToIntChar '3' = "three"
digitToIntChar '4' = "four"
digitToIntChar '5' = "five"
digitToIntChar '6' = "six"
digitToIntChar '7' = "seven"
digitToIntChar '8' = "eight"
digitToIntChar '9' = "nine"

stringtoList :: Int -> [Char]
stringtoList s = show s


-- fullWords :: Integer -> String
-- fullWords n = concat map (\d -> digitToIntChar d ++ "-") stringtoList n
--   where digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]