module Main where

import Test.QuickCheck
import System.Exit

import Quipper.Libraries.ClassicalOptim.QuickCheck
import Quipper.Libraries.ClassicalOptim.QuickCheckAlgExp
import Quipper.Libraries.ClassicalOptim.QuickCheckArith

try :: String -> IO Result -> IO ()
try name body = do
  putStrLn name
  r <- body
  if not (isSuccess r) then exitFailure else return ()

main :: IO ()
main = do
  try "test_truth2 2" $ test_truth2 2
  try "test_truth2 5" $ test_truth2 5
  try "test_truth2 10" $ test_truth2 10
  try "test_multiplier" $ test_multiplier
  try "testCircSimpl" $ myQuickTest testCircSimpl
  try "testCircSwap" $ myQuickTest testCircSwap
