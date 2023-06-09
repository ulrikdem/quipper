{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "Quipper/Demos/Reverse.hs" #-}
import Quipper

my_circuit :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
my_circuit (a,b) = do
  qnot_at a `controlled` b
  hadamard b `controlled` a
  return (a,b) 
  
rev_circuit :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
rev_circuit = reverse_simple my_circuit

main =
  print_simple Preview rev_circuit
