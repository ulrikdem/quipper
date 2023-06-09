-- | Demonstrates Quipper's decomposition of multiply-controlled
-- not-gates into the Clifford+/T/ gate set.

import Quipper
import Quipper.Libraries.Decompose

multi_cnot :: [Qubit] -> Qubit -> Circ ()
multi_cnot controls target = do
  qnot_at target `controlled` controls

multi_cnot_decomposed :: [Qubit] -> Qubit -> Circ ()
multi_cnot_decomposed = decompose_generic Exact multi_cnot

main_n :: Int -> IO ()
main_n n = do
  putStrLn ("Gate counts for " ++ show n ++ " controls:")
  putStrLn ""
  print_generic GateCount multi_cnot_decomposed (replicate n qubit) qubit
  putStrLn ""
  print_generic Preview multi_cnot_decomposed (replicate n qubit) qubit
 
main :: IO ()
main = do
  main_n 0
  main_n 1
  main_n 2
  main_n 3
  main_n 4
  main_n 5
  main_n 6  
