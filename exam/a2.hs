import Prelude 
    (Int, (+), (-), (*) 
    ,(==) 
    ,Bool(..), (&&), (!!), not
    ,fst, snd
    ,error, undefined
    )

{-^!#!^-}

-- Don't touch anything above this line
-- If you do, the autograder will reject your file without explanation.

{-

COMP 3007 Fall 2023
Assignment 2

Submission instructions
1. Put all your code in this file.
2. Do not rename the file.
3. Submit the file to Gradescope.
4. Do not import any libraries; added "import" statements will be removed by the 
   autograder

Late assignments will be accepted by the autograder but a score of zero will
be recorded.

The questions below ask you to write some simple Haskell functions. Part of the
definition of each function is given. You need to replace the "undefined" part
with a Haskell expression. Only replace "undefined"; don't change anything else.
In the specifications for the functions, all integers are assumed to be
non-negative. All questions have equal weight.

Don't worry about efficiency. If your program gives the right answer on small
numbers, yay. If it takes forever on a 10-digit number, the autograder will not
know or care.  

Most of the standard Haskell library has been removed for this assignment. See
the "import Prelude" statement at the top of the file for a list of what you can
use.

-}

-- fact n = 1 * 2 * ... * n
fact n = 
    if n == 1 then 1
    else fact (n - 1) * n

-- exp x n = x^n = x*x*...*x 
--                 ^^^^^^^^^- n times
exp :: Int -> Int -> Int
exp x 1 = x
exp x n =
    x * exp x (n - 1)

-- sum_power x n = x^0 + x^1 + ... + x^n
sumPower :: Int -> Int -> Int
sumPower x 0 = 1
sumPower x n =
    exp x n + sumPower x (n - 1)

-- lte x y iff x <= y (i.e. x is less than or equal to y)
-- Note. Recall that you can assume x and y are always nonnegative.
lte :: Int -> Int -> Bool
lte x y = 
    if x == 0 && y == 0 then True
    else if x == 0 then True
    else if y == 0 then False
    else lte (x - 1) (y - 1)

-- quotient x  y is the largest integer k with k*y <= x
-- Assume y > 0.
quotient :: Int -> Int -> Int
quotient x y =
    quotient' x y x

quotient' x y k =
    if (k * y) `lte` x then k
        else quotient' x y (k - 1)

-- remainder x y is a number k such that k < y and x-k is divisible by y.
-- E.g. remainder 13 5 = 3.
-- Assume y > 0.
remainder :: Int -> Int -> Int
remainder x y =
    x - (quotient x y) * y

-- loopyFact n 1 = 1 * 2 * ... * n
-- This is an odd specification since it only specifies what happens
-- if the second argument is 1. This means you can do what you want
-- in the other cases.
loopyFact :: Int -> Int -> Int
loopyFact n v = 
    fact n 

-- for testing iterfun below
s :: Int -> Int
s n = n + 1

-- iterfun f n x = f (f (...f(x)))
--                 ^^^^^^^^^^^^^^^ n applications of f.
-- E.g. iterfun s 10 3 = 13
-- Note that the type of iterfun below shows that the first argument is
-- a function from integers to integers. You can use this argument just
-- like any other function.
iterfun :: (Int -> Int) -> Int -> Int -> Int
iterfun f 1 x = x 
iterfun f n x =
    iterfun f (n-1) (f x)