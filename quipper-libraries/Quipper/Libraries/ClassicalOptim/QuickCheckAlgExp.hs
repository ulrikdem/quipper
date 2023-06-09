module Quipper.Libraries.ClassicalOptim.QuickCheckAlgExp where

import qualified Test.QuickCheck as Test

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM {- containers-0.5.2.1 -}

import Quipper.Utils.Auxiliary (bool_xor)

import Quipper.Libraries.ClassicalOptim.Circuit

import Quipper.Libraries.ClassicalOptim.AlgExp

-- ----------------------------------------------------------------------
-- ** Quick-checking

-- | Compute 2[sup /n/].
twoExp :: (Integral a) => a -> Int
twoExp 0 = 1
twoExp n | mod n 2 == 0 = let a = twoExp (div n 2) in a * a
         | otherwise = 2 * (twoExp (n-1))

-- | Generate a list of 'Bool'.
genBoolList :: Integral a => a -> Test.Gen [Bool]
genBoolList n = Test.vectorOf (twoExp n) $ Test.oneof [return True, return False]

-- | Arguments for QuickCheck.
test_args :: Test.Args
test_args = Test.stdArgs { Test.maxSize = 100, 
                           Test.maxSuccess = 100, 
                           Test.maxDiscardRatio = 20 }

-- | First test: truth table to expression to truth table is the identity.
test_truth1 :: Int -> IO ()
test_truth1 n = Test.quickCheckWith test_args  $ aux
  where
    aux = Test.forAll (genBoolList n) $ \x ->
      x == (truth_table_of_exp [1..n] $ exp_of_truth_table 1 x)

-- | Generate a random list of @Int@s.
genIntList :: [Int] -> Int -> Test.Gen [Int]
genIntList vars size = do
  s <- Test.choose (0,size)
  Test.vectorOf s $ Test.elements vars

-- | Generate a random expression out of the given variables.
genExp :: [Int] -> Test.Gen Exp
genExp vars = do
  nber <- Test.choose (0, twoExp (length vars))
  v <- Test.vectorOf nber $ Test.sized $ genIntList vars
  return $ expOfList v

-- | Second test: expression to truth table to expression is the
-- identity.
test_truth2 :: Int -> IO Test.Result
test_truth2 n = Test.quickCheckWithResult test_args $ aux
  where
    aux = Test.forAll (genExp [1..n]) $ \x ->
      x == (exp_of_truth_table 1 $ truth_table_of_exp [1..n] x)


