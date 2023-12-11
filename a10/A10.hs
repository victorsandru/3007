import A10Lambda
import Data.List (find, union, nub)

-- ***********************************************************************
--
-- DO NOT TOUCH ANYTHING ABOVE THIS LINE!
--
-- YES, THIS INCLUDES THE LINE WITH ALL THE ***'s. 
--
-- The autograder does not appreciate false accusations of bugs when a 
-- problem is caused by foolish, high-handed tampering with the forbidden 
-- area of this file.
--
-- ***********************************************************************

----------------------------------------------------------------
-- Data types 

-- Assignment 10 
-- Due Friday Dec 8 23:59 -- HARD DEADLINE
-- Submissions: submit only this file (not Lambda.hs)

-- This file contains a very lean lambda-calculus interpreter. The
-- lambda-calculus here has an addition though: in addition to variables, it
-- has "names" that signify definitions. We extend the L data type, and add two
-- new data types: Def, for definitions, and Prog, for complete programs
-- (definitions and a term to evaluate).

-- All three of these types have a special Show method implemented, so 
-- objects get printed out in a readable string representation that doesn't
-- show the constructors. To see the constructors, use "pp".
--
-- Each data type has a parser, one of parseL, parseDef and parseProg, that converts a
-- string representation into a member of the appropriate data type.

-- Example program (before parsing):

egProg :: Prog
egProg = 
    (parseProg . unwords)
    [ "<| True = λx.λy.x |>"     
    , "<| False = λx.λy.y |>"
    , "<| If = λb.λx.λy. b x y |>"
    , "<| Zero = λf.λx.x |>"
    , "<| One = λf.λx. f x |>"
    , "If True Zero One"
    ]

-- The "definitions" have the usual meaning: the name on the left-hand side of
-- the equation stands for the term on the right-hand side of its defining
-- equation. To evaluate a term with names, the names need to be replaced by the
-- terms they're defined to be.
--
-- Restrictions on programs:
--
-- 1. The definition names must be non-empty and start with a capital letter.
--
-- 2. The right-hand side of each definition must be a closed term.
--
-- 3. The definitions have to have distinct names, i.e. a name can't have more
--    than one definition.
--
-- Evaluating ("running") lambda-terms: we use a restricted version of
-- leftmost-outermost normalization. The restriction is that reduction doesn't
-- take place inside λ's. Here is an example.
--
--   (λx.x)(λw.w)(λy.(λz.z)y) => (λw.w)(λy.(λz.z)y) => λy.(λz.z)y
--
-- Reduction stops at λy.(λz.z)y, even though it contains a redex, because it's
-- a λ and we don't reduce  under λs. 
--
-- The advantage of stopping "early" like this is that we no longer have to
-- worry about variable capture. This is because of the following property
-- (exercise: prove it): if a term e is closed then so is any subterm of e that
-- is not inside a λ (i.e. in the body of a term of the form λx. ...). This
-- means that in this version of reduction we're only substituting *closed*
-- terms for variables, so no capture is possible. 
--
-- The disadvantage to doing this is that the end result of reducing is no
-- longer a normal form. So, for example, if we're expecting an integer
-- representation like λf.λx. f(f(x)) (which represents 2), we will probably get
-- some huge term that starts with λf but has a body with unreduced stuff (lots
-- of remaining redexes), and so we won't be able to just read off the number of
-- f occurrences to determine the number that's represented.
--
-- To address this disadvantage, the evaluator uses a restriction and a hack.
--
-- 1. The restriction. A term to be evaluated should normalize to either λx.λy.x
--    or λx.λy.y. (These are the representations in L of true and false).
--
-- 2. The hack. Terms are allowed to have names that have no corresponding
--    definition. Reduction treats them as values. E.g. a term 
--
--         ((λx.x) Ihavenodef) 
--
--    after one step of reduction is the term Ihavenodef, which is just a name
--    with no definition. No more reductions are possible so it's the "value"
--    obtained by reduction. So, to run a term e satisfying 1 above, we give it
--    two different undefined names as arguments, reduce until a value is
--    obtained, and see which of the two names the value is. This will determine
--    which of the two possible normal forms e has. 
--
-- This kind of reduction, where reduction doesn't proceed inside λs, is called
-- "weak head reduction", or "weak head normalization", and the values it
-- produces (names, or terms of the form λx.e), are called weak head-normal
-- forms. This is the form of evaluation used by Haskell.
--
-- Most of the code for this lambda-calculus "interpreter" is given in the file
-- Lambda.hs and below. Your job is to write a couple of utilities, modify a few
-- functions to add a case for names, and write a very small program in the
-- lambda calculus.


-- e is closed and all names in it are capitalized
validTerm :: L -> Bool
validTerm e =
    isClosed e && all isCapitalized (names e)


-- See 1 and 2 of "Restrictions on programs" above.
validDef :: [Def] -> Def -> Bool
validDef defs (Def name rhs) =
    isCapitalized name && isClosed rhs && name `notElem` map defName defs

-- See 1, 2 and 3 of "Restrictions on programs" above.
validDefs :: [Def] -> Bool
validDefs defs =
     all (\def -> validDef defs def) defs && distinctNames (map defName defs)
  where
    distinctNames :: [String] -> Bool
    distinctNames names = length names == length (nub names)

-- valid defs and term e
validProg :: Prog -> Bool
validProg (Prog defs e) =
    undefined

-- a list of all the names occurring in L, duplicates ok
names :: L -> [String]
names (Var _) = []
names (Name x) = [x]
names (App e1 e2) = union (names e1) (names e2)
names (Lam _ e) = names e


-- (whr doTrace defs e) assumes e is closed and computes the weak-head-normal
-- form of e. If doTrace is True then each redex step is printed out. The
-- definition is incomplete. You need to add a case (or cases), for Name.
-- Examples (see below for definition of "defs"):
--      ghci> whr False defs (parseL "If True One Ihavenodef")
--      λf.λx.f x
--      ghci> whr False defs (parseL "If False One Ihavenodef")
--      Ihavenodef
whr :: Bool -> [Def] -> L -> L
whr doTrace defs (Var _) =
    error "whr: found free variable"
whr doTrace defs (Name x) =
     case lookup x [(defName def, def) | def <- defs] of
        Just def -> whr doTrace defs (defRHS def)
        Nothing  -> Name x
whr doTrace defs (Lam x e) = 
    Lam x e
whr doTrace defs redex@(App (Lam x e1) e2) = 
    let reduct = subst e1 e2 x -- e2 closed so no capture possible
    in
    whr doTrace defs $ maybeTrace doTrace reduct $ show redex ++ " ==> " ++ show reduct
whr doTrace defs (App e1 e2) =
    whr doTrace defs (App (whr doTrace defs e1) e2)


runBool :: Bool -> Prog -> Bool
runBool doTrace prog@(Prog defs e) =
    if validProg prog then
       whr doTrace defs (App (App e (Name "Trooooo")) (Name "Faaaahhhlse")) == (Name "Trooooo")
    else error "runBool: invalid program"

-- The function to use for testing your lambda code. If it takes more than a
-- second to return, it probably means that your expression does not return a
-- lambda boolean.
-- Change the False to True below to step through the computation.
run str = 
    runBool False (Prog defs (parseL str))


defs =
    map parseDef $ 
    [ 
    -- Boolean stuff
      "<| True = λx.λy.x |>"
    , "<| False = λx.λy.y |>"
    , "<| If = λb.λx.λy. b x y |>"
    , "<| And = λb. λc. If b c False |>"
    , "<| Not = λb. If b False True |>"

    -- Natural numbers
    , "<| Zero = λf.λx.x |>"
    , "<| One = λf.λx. f x |>"
    , "<| Two = λf.λx. f (f x) |>"
    , "<| Three = λf.λx. f (f (f x)) |>"
    , "<| Succ = λn. λf.λx. f (n f x) |>"
    , "<| Eqzero = λn. n (λw. λx.λy.y) (λx.λy.x) |>"
    , "<| Addviasucc = λm.λn. m Succ n |>"
    , "<| Pred = λn. Fst (n (λp. Pair (Snd p) (Succ (Snd p))) (Pair Zero Zero)) |>"
    , "<| Eqint = λm.λn. And (Eqzero (Sub m n)) (Eqzero (Sub n m)) |>"
    , "<| Add = λm. λn. If (Eqzero m) n (Succ (Add (Pred m) n)) |>"
    , "<| Sub = λm. λn. n Pred m |>"
    , "<| Mul = λm.λn. m (Add n) Zero |>"

    -- Pairing
    , "<| Pair = λx.λy. λf. f x y |>"
    , "<| Fst = λp. p (λx.λy.x) |>"
    , "<| Snd = λp. p (λx.λy.y) |>"

    -- The Y combinator (not needed, FYI only)
    , "<| Y = λf. (λx. f (x x)) (λx. f (x x)) |>"

    -- Lists
    , "<| Nil = λf. Pair True False f |>"
    , "<| Cons = λx. λl. Pair False (Pair x l) |>"
    , "<| Null = λl. Fst l |>"
    , "<| Head = λl. Fst (Snd l) |>"
    , "<| Tail = λl. Snd (Snd l) |>"
    , "<| Length = λl. If (Null l) Zero (Succ (Length (Tail l))) |>"
    , "<| List1 = Cons Zero (Cons One Nil) |>"
    , "<| List2 = Cons One (Cons Zero Nil) |>"
    , "<| List3 = Cons Zero (Cons One (Cons One (Cons Zero Nil))) |>"
    , "<| Eqintlist = λla. λlb. " ++ eqintlistRHS ++ " |>"
    , "<| Append = " ++ appendRHS ++ " |>"
    ]

-- Implement the right-hand-side of the definition for Append (see defs), which
-- appends/concatenates two lists. You can test your lambda-code using run,
-- List1, List2, List3 and Eqintlist. You have to use Eqintlist since the run
-- function can only handle terms that compute to λx.λy.x or λx.λy.y (the
-- representation of booleans in L).
appendRHS :: String
appendRHS = 
    undefined
eqintlistRHS = unwords 
    ["If (Null la)"
    ,"   (Null lb)"
    ,"   (And (Not (Null lb))"
    ,"        (And (Eqint (Head la) (Head lb))"
    ,"             (Eqintlist (Tail la) (Tail lb))))"
    ]
