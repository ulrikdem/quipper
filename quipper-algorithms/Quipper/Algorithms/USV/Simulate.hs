-- | This module contains functions for simulating and 
-- debugging the USV algorithm and some of its subroutines.

module Quipper.Algorithms.USV.Simulate where

import Quipper

import Quipper.Libraries.Arith
import Quipper.Libraries.Simulation

import Quipper.Algorithms.USV.USV
import Quipper.Algorithms.USV.Definitions

import Quipper.Utils.Sampling

-- $ For the coherent arithmetic subroutines defined 
-- in "Quipper.Algorithms.USV.USV", compare the classical 
-- implementation (implemented using Haskell 
-- functions) and the quantum one (simulated).


-- =====================================================================================
-- * Testing for subroutine /h/

-- | Given an integer /n/, print the table comparing the output values 
-- of the functions 'h_classical' and 'h_quantum' on every possible 
-- vector in (ℤ[sub 2[sup 4/n/]])[sup /n/].
h_test :: Int -> IO ()
h_test n = do

-- Compute a table containing:
-- Intput | Output of h_classical | Output of (simulated) h_quantum 
  let h_table_n = [ "h table for n = " ++ (show n) ++ ":"
                      , ""
                      , "v   h(v)_C         h(v)_Q      "]
                    ++
                     [ (show (map integer_of_intm_unsigned w)) ++ "   " ++ (show y) ++ "   " ++ (show z) ++ flag
                      | w <- (sample_all0 (replicate n (intm (4*n) (2^(4*n)-1))))
                      , let y = integer_of_intm_unsigned $ h_classical w 
                      , let z = integer_of_intm_unsigned $ (run_classical_generic h_quantum) w
                      , let flag = if y /= z then "  **MISMATCH**" else ""]  
                    ++
                     ["",""]

--Print the table
  mapM putStrLn $ h_table_n
  return ()

