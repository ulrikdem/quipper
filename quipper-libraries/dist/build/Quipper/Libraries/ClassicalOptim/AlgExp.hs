{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "Quipper/Libraries/ClassicalOptim/AlgExp.hs" #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
  {-# LANGUAGE OverlappingInstances #-}
#endif

-- | This module contains an efficient representation of algebraic
-- boolean formulas.
module Quipper.Libraries.ClassicalOptim.AlgExp where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM {- containers-0.5.2.1 -}

import Quipper.Utils.Auxiliary (bool_xor)

import Quipper.Libraries.ClassicalOptim.Circuit

-- ----------------------------------------------------------------------
-- * Auxiliary functions

-- | Build the characteristic function of a set.
mapOfSet :: Ord a => S.Set a -> M.Map a Int
mapOfSet s = S.foldl' (\m x -> M.insert x 1 m) M.empty s

-- | Get the set of elements whose images are odd.
setOfMap :: Ord a => M.Map a Int -> S.Set a
setOfMap m = 
    M.foldlWithKey' (\s x _ -> S.insert x s) S.empty $ 
          M.filter (\x -> mod x 2 == 1) m

-- | Split a list in the middle.
split_even :: [a] -> ([a],[a])
split_even a = splitAt (div (length a) 2) a

-- ----------------------------------------------------------------------
-- * Expressions

-- | The type of algebraic boolean expressions. 
-- 
-- We represent boolean expressions using \"and\" and \"xor\" as the
-- primitive connectives. Equivalently, we can regard booleans as the
-- elements of the two-element field /F/[sub 2], with operations \"*\"
-- (times) and \"+\" (plus). 
-- 
-- An algebraic expression
--      @x1*x2*x3 + y1*y2*y3 + z1*z2@
-- is encoded as
--      @{{x1,x2,x3},{y1,y2,y3},{z1,z2}}@.
-- 
-- In particular,
--     @{}   == False == 0@ and 
--     @{{}} == True  == 1@.
type Exp = S.Set IS.IntSet

instance {-# OVERLAPPING #-} Show Exp where
    show e = if (S.null e) then "F"
             else if (e == S.singleton (IS.empty)) then "T"
             else L.concat $ L.intersperse "+" (L.map (\e -> L.concat $ L.map (\x -> "x" ++ (show x)) $ IS.toList e) $ S.toList e)

-- | Turn an @Exp@ into a list of lists.
listOfExp :: Exp -> [[Int]]
listOfExp e = S.toList $ S.map IS.toList e

-- | Turn a list of lists into an @Exp@.
expOfList :: [[Int]] -> Exp
expOfList l = S.fromList $ L.map IS.fromList l

-- | The conjunction of two expression.
exp_and :: Exp -> Exp -> Exp
exp_and a b = 
    setOfMap $
    S.foldl (\exp monomial -> M.unionWith (+) exp $ exp_and_aux monomial $ mapOfSet a) M.empty b
  where
    exp_and_aux :: IS.IntSet -> M.Map IS.IntSet Int -> M.Map IS.IntSet Int
    exp_and_aux monomial exp =  M.mapKeysWith (+) (IS.union monomial) exp

-- | The xor of two expressions.
exp_xor :: Exp -> Exp -> Exp
exp_xor a b = setOfMap $ M.unionWith (+) (mapOfSet a) (mapOfSet b)

-- | The expression \"False\".
exp_false :: Exp
exp_false = S.empty

-- | The expression \"True\".
exp_true :: Exp
exp_true = S.singleton IS.empty

-- | The negation of an expression.
exp_not :: Exp -> Exp 
exp_not e = exp_xor e exp_true

-- | The expression /x/[sub /n/].
exp_var :: Int -> Exp
exp_var x = S.singleton $ IS.singleton x

-- ----------------------------------------------------------------------
-- * Properties of expressions

-- $ The important property of expressions is that two formulas have
-- the same truth table iff they are syntactically equal. This makes
-- the equality test of wires theoretically straightforward.

-- ----------------------------------------------------------------------
-- ** Truth tables

-- $ A /valuation/ on a set of variables is a map from variables to
-- booleans. This can be thought of as a row in a truth table. A
-- /truth table/ is a map from valuations to booleans, but we just
-- represent this as a list of booleans, listed in lexicographically
-- increasing order of valuations.

-- | Get the variables used in an expression.
vars_of_exp :: Exp -> [Int]
vars_of_exp e = IS.toList $ S.foldl (\a b -> IS.union a b) IS.empty e

-- | Evaluate the expression with respect to the given valuation. A
-- /valuation/ is a map from variables to booleans, i.e., a row in a
-- truth table.
exp_eval :: Exp -> M.Map Int Bool -> Bool
exp_eval e m = L.foldl bool_xor False $ L.map (L.foldl (&&) True) $ L.map (L.map (m M.!)) $ L.map (IS.toList) $ S.toList e

-- | Construct the list of all 2[super /n/] valuations for a given
-- list of /n/ variables.
valuations_of_vars :: [Int] -> [M.Map Int Bool]
valuations_of_vars [] = [M.empty]
valuations_of_vars (h:t) = l
  where
    l = (L.map (M.insert h False) v) ++ (L.map (M.insert h True) v)
    v = valuations_of_vars t 

-- | Build the truth table for the given expression, on the given list
-- of variables. The truth table is returned as a list of booleans in
-- lexicographic order of valuations. For example, if
-- 
-- >  1 2 | exp
-- >  F F | f1
-- >  F T | f2
-- >  T F | f3
-- >  T T | f4
-- 
-- then the output of the function is @[f1,f2,f3,f4]@.
truth_table_of_exp :: [Int] -> Exp -> [Bool]
truth_table_of_exp vars e = L.map (exp_eval e) (valuations_of_vars vars)

-- | Return an expression realizing the given truth table. Uses
-- variables starting with the given number.
exp_of_truth_table :: Int -> [Bool] -> Exp
exp_of_truth_table i [] = exp_true
exp_of_truth_table i [False] = exp_false
exp_of_truth_table i [True] = exp_true
exp_of_truth_table i t = ((exp_not (exp_var i)) `exp_and` e1) `exp_xor` ((exp_var i) `exp_and` e2)
  where
    (t1,t2) = split_even t
    e1 = exp_of_truth_table (i+1) t1
    e2 = exp_of_truth_table (i+1) t2

