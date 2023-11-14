f xs == tail xs == ["foo", "bar"]

Type inference operation:

f :: a 
xs :: b 
tail :: [c] -> [c]
(==) :: Eq d => d -> d -> Bool
"foo" :: String
["foo", "bar"] :: [String]
d = [String]
tail xs :: [String]
[c] = [String]
c = String