module A1 where -- DON"T TOUCH THIS LINE! 

--
-- COMP 3007 Fall 2023
-- Assignment 1

-- Submission instructions
-- 1. Put all your code in this file.
-- 2. Do not rename the file; the name is "A1.hs", not e.g. "a1.hs".
-- 2. Submit the file to Gradescope.
-- 3. Gradescope will immediately autograde it.
-- 4. Edit and resubmit as many times as you like.

-- Late assignments will be accepted by the autograder but a score of zero will
-- be recorded.

-- To learn how to install Haskell and run Haskell files see the Help page on
-- the course website.

-- The questions below ask you to write some simple Haskell functions. Part of
-- the definition of each function is given. You need to replace the "undefined"
-- part with a Haskell expression. Only replace "undefined"; don't change
-- anything else. 

-- Here are some basic functions/features you might find useful.
-- - Integer arithmetic operations: +, -, *
-- - Boolean values/operations True, False, &&, ||, not
-- - Integer comparison: ==, <, <=, <, >=
-- - if-then-else: this is an expression, i.e. returns a value, e.g.
--                     if 3 == 2 then 1 else 2
--                 is an expression with value 2 
-- - Pairing: (1,2) is the pair of 1 and 2; fst and snd get a pair's components
--
-- To see the type of a defined function, use the :type command in ghci. If the
-- function is infix, you need to put parens around it. E.g.
--     ghci> :type not
--     not :: Bool -> Bool 
--     ghci> :type (&&)
--     (&&) :: Bool -> Bool -> Bool
-- Some types will have "type variables" with constraints. We'll cover types
-- like these later in the course. An example should suffice for now.
--     ghci> :type (+)
--     (+) :: Num b => b -> b -> b
-- This means that + can takes arguments of any type b meeting the Num
-- constraint, i.e. any type b that has arithmetic operators defined for it. In
-- Haskell, constraints like this are similar to Java interfaces.

-- Question 1.
-- p x is the square of x, plus x, plus 1
p :: Int -> Int
p x = 
    (x ^ x) + x + 1

-- Question 2.
-- nat_minus x y is x-y unless x-y is negative, in which case it returns 0
m :: Int -> Int -> Int
m x y =
    if (x - y) < 0
        then 0
        else x - y

-- Question 3.
-- Return any value you like, as long as it typechecks (i.e. it doesn't cause an
-- error when you load this file into ghci). 
d :: Int -> (Int, Int)
d x = 
    (10, 10)

-- Question 4.
-- See Question 3.
t :: Int -> Int -> Int -> (Int, (Int,Int))
t x y z = 
    (10, (10, 10))

-- Question 5.
-- See Question 3.
l :: (Int, (Int, Int)) -> Int
l p = 
    10

-- Question 6.
-- See Question 3.
c :: Int -> Int
c x = 
    10

