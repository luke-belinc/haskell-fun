module SimpleFunctions where

-- can declare function type before definition, otherwise type is inferred
firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) 
                   then head lst 
                   else "empty"

-- usage of functions with symbolic names (i.e. +++)
-- must wrap type definitions with parentheses
-- must use operator infix name as function definition
--(+++) :: [a] -> [a] -> [a]
--lst1 +++ lst2 = if null lst1    {- check emptyness -}
--                then lst2       -- base case
--                else (head lst1) : (tail lst1 +++ lst2)
-- function rewritten at line 111
--
-- reuse of +++ function within a different funciton
reverse2 lst = if null lst 
               then [] 
               else reverse2 (tail lst) +++ [head lst]

-- usage of let, in, and where clauses
-- rewrite of maxmin at line 130
{-
maxmin lst = 
  let h = head lst in 
    if null (tail lst)
    then (h, h)
    else (if h > t_max then h else t_max, if h < t_min then h else t_min) 
      where 
        t = maxmin (tail lst) 
        t_max = fst t
        t_min = snd t
-}

-- Algebraic Data Types (ADTs) examples: 
-- Many constructors for the same type
-- Possible to create constructor with same name as data type
-- data types and constructors names live in different worlds
-- Consturctor names must all be unique (i.e. another type cannot have GovOrg constructor, etc)
-- all types inside another one must be Showable (e.g. Person constructor in Person type)
-- Constructors using other types must derive the same types (e.g. Gender and Person deriving Show)
data Client = GovOrg     String
            | Company    String Integer Person String
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Nonbinary | Trans | OtherGender
            deriving Show

----------------------

-- PATTERN MATCHING --
-- pattern matching is order dependent
-- pattern matching does NOT backtrack to alternative cases

-- you can use underscores to avoid unused bindings (i.e. nonwarning pattern)
clientName :: Client -> String
clientName client = case client of
                      GovOrg name -> name
                      -- Company name id person resp -> 
                      Company name _ _ _ -> name
                      -- Individual person ads ->
                      Individual person _ -> 
                        case person of
                          -- Person fName lName Gender -> 
                          Person fName lName _ -> fName ++ " " ++ lName

-- pattern matching for non exhaustive patterns using Maybe, Just, and Nothing bindings
companyName :: Client -> Maybe String
companyName client = case client of
                Company name _ _ _ -> Just name
                _ -> Nothing

-- pattern matching on let and where bindings
-- in this case, name1 and name2 are equivilent, they remove the Just binding
-- name1 is a more concise version of name2
-- in these cases you can only handle 1 pattern
-- let Just name = companyName client
-- let name = case companyName client of
--              Just n -> n
Just name1 = companyName (Company "nasa" 1 (Person "john" "john" Male) "ceo")
name2 = case companyName (Company "nasa" 1 (Person "john" "john" Male) "ceo") of
          Just n -> n

-- constants are also patterns
fibonacci :: Integer -> Integer
fibonacci n = case n of
                0 -> 0
                1 -> 1
                _ -> fibonacci (n - 1) + fibonacci (n - 2)

-- once the pattern has been matched it completely stops trying other alternatives
-- even if a further match raises an error
-- the following two functions are not equivilent
f :: Client -> String
f client = case client of
             Company _ _ (Person name _ _) "Boss" -> name ++ " is the boss"
             _ -> "There is no boss"

-- in this case once the system enters the subcase after matching Company and forgets about
-- other alternatives, then in the subcase the inner match fales and raises an exception
g :: Client -> String
g client = case client of
             Company _ _ (Person name _ _) pos ->
               case pos of "Boss" -> name ++ " is the boss"
             _ -> "There is no boss"

-- rewrite of previous +++ function
(+++) :: [a] -> [a] -> [a]
list1 +++ list2 = case list1 of
                    [] -> list2
                    x:xs -> x:(xs +++ list2)
-- directly matching looks like:
-- [] +++ list2 = list2
-- (x:xs) +++ list2 = x:(xs +++ list2)

sorted :: [Integer] -> Bool
sorted []            = True
sorted [_]           = True
-- sorted (x:y:zs)   = x <= y && sorted (y:zs)
sorted (x : r@(y:_)) = x <= y && sorted r

-- rewrite of maxmin using tuples instead of list notations
maxmin [x] = (x,x)
maxmin (x:xs) = (if x > xs_max then x else xs_max,
                 if x < xs_min then x else xs_min)
                 where (xs_max, xs_min) = maxmin xs

-------------------------------
-- GUARDS --
-- a guard is part of the pattern-matching syntax that allows you to refine a pattern using
-- using boolean coniditons that must be fulfilled bound by the values after a successful match
-- useful for writing clearer code and avoiding certain problems

-- two examples: 
-- extention of the fibonacci function to any integer value:
--      wrap the value on a Just if you are asked to get the finbonacci number of a 
--      nonnegative number and return nothing otherwise
ifibonacci :: Integer -> Maybe Integer
{-
ifibonacci n = if n < 0 
               then Nothing
               else case n of
                 0  -> Just 0
                 1  -> Just 1
                 n' -> let Just f1 = ifibonacci (n'-1)
                           Just f2 = ifibonacci (n'-2)
                       in Just (f1 + f2)
-}
-- instead a more concise example using guards below
-- using guards allows you to backtrack if the condition following the '|' fails
ifibonacci n | n < 0     = Nothing
ifibonacci 0             = Just 0
ifibonacci 1             = Just 1
ifibonacci n | otherwise = let Just f1 = ifibonacci (n - 1)
                               Just f2 = ifibonacci (n - 2)
                           in Just (f1 + f2)

-- example for binomial coefficient for n and k. 
-- (n k) = (1 if k = 0 or n = k) or ((n-1 k-1) + (n-1 k))
-- using this definition one could write
{-
binom _ 0 = 1
binom x x = 1
binom n k = (binom (n-1) (k-1)) + (binom (n-1) k)
-}
-- but this will cause an error bc "Conflicting definitions for 'x'"
-- a useful example using guards is
-- binom :: Integer -> Integer -> Maybe Integer
binom n k | n < k = Nothing
binom n k | n == k || k == 0 = Just 1
binom n k | otherwise = let 
                          Just x = (binom (n-1) (k-1))
                          Just y = (binom (n-1) k)
                        in
                          Just (x + y)

Just x = binom 3 2
y = binom 3 2
