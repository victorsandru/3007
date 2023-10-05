import Prelude 
    (String
    ,Int, (+), (-), (*),  (<)
    ,(^)       -- integer exponentiation; 2^4 = 16 
    ,div, rem  -- integer division and remainder; div 11 3 = 3, rem 11 3 = 2
    ,Bool(..), (==), (&&), (||), not
    ,foldl1
    ,fst, snd
    ,undefined
    )

{-^!#!^-}

-- DON'T TOUCH THE ABOVE!!!!!!!!!!!!!!!!!!!!!!!!!!

--------------------------------------------------



{-
 
GENERAL INSTRUCTIONS

The test ends at 11:20 (except for PMC students).

All the questions have equal weight.

If the question doesn't specify what the result should be for certain inputs,
then your function can do anything on those inputs, including failing to
terminate.

IMPORTANT: See the import statement above for functions you are allowed to use.

A number of questions refer to the "digits" of a number. These are the usual
decimal digits we use to write down integers. E.g. 3007 has digits 3, 0, 0, 7 in
that order.

The code will be autograded, but offline (i.e. after the quiz). Partial credit
may be given for functions that work only on some of the inputs. Code that
doesn't compile will be given zero. Use ghci to debug your code.

If you get more than 100% on the quiz because of the extra credit questions, the
extra marks will be added to your total quiz mark.

As usual, don't change any of the existing code except for "undefined". You can
define additional functions if you like.

MOST IMPORTANT: Submit your file to Gradescope before the test ends!! You can
submit as many times as you like, so to be safe, submit something near the end
before your final copy. 

-
-}



-- E.g. lastDigit 3007 = 7. n>=0.
lastDigit :: Int -> Int
lastDigit n = 
    if n < 10 && -1 < n then n
    else lastDigit (n - 10)

-- E.g. firstDigit 3007 = 3. n>=0.
-- Hint: div 3007 10 = 300.
firstDigit :: Int -> Int
firstDigit n =
    if n < 10 && -1 < n then n
    else firstDigit (div n 10)

-- fact n = n! = 1*2*...*n. n>=0.
-- For use in sumFact.
fact n = foldl1 (*) [1..n] 

-- For n>0, 
-- sumFact n = 1! + 2! + ... + n!
sumFact n = 
    if n == 1 then 1
    else fact n + sumFact (n - 1)

-- If a,b >= 1 then (log a b) is the largest k such that a^k <= b.
-- For example, (log 3 11) = 2 since 3^2 = 9 <= 11 < 27 = 3^3 
log :: Int -> Int -> Int
log a b = 
    logHelper a b 1

logHelper :: Int -> Int -> Int -> Int
logHelper a b n = 
    if b < a^n then n - 1
    else logHelper a b (n + 1)

-- E.g. numDigits 3007 = 4. n>=0.
numDigits n =
    if n < 10 && -1 < n then 1
    else 1 + numDigits (div n 10)

-- E.g. reverseDigits 3007 = 7003. n>=0.
reverseDigits :: Int -> Int
reverseDigits n =
    reverseDigitsHelper n 10 ^ numDigits n

reverseDigitsHelper :: Int -> Int -> Int
reverseDigitsHelper n b =
    if n < 10 && -1 < n then n
    else (lastDigit n * 10 ^ numDigits n) + reverseDigits (div n 10)


-- END OF QUIZ
--
-- EXTRA CREDIT QUESTION BELOW. Only attempt this if you've finished and
-- thoroughly tested the above. It's quite a bit more difficult.

-- This is a "type class" definition. You don't need to already know what this
-- is in order to do the question below. The definition can be read as "The type
-- class S is all those types where there are definitions of the functions z, s
-- and iterfun with the specified types.
class S a where
    z :: a
    s :: a -> a
    iterfun :: (b -> b) -> a -> b -> b

-- This declaration says that Int is one of the types in the class S because we 
-- define the three required functions as shown. Because of this declaration, if
-- the compiler sees (s x) where x is of type Int, it will use the s defined
-- here.  
instance S Int where
    z = 0
    s = (+1)        
    iterfun f n x =
        if n == 0 then x
        else iterfun f (n-1) (f x)

-- Define the "minus one" function for types in the class S. Use pTest to test
-- your function. Since you only have z, s and iterfun to work with in defining
-- p, it's not straightforward. You might consider using pairing ( (,), fst,
-- snd).
p :: S a => a -> a
p x = 
    undefined

pTest :: Int -> Bool
pTest x = 
    x==0 || p x == x - 1
