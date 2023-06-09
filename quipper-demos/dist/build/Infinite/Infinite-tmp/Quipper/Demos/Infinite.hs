{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "Quipper/Demos/Infinite.hs" #-}
import Quipper
import Quipper.Libraries.Decompose

-- Outputs an infinite circuit. This illustrates the use of laziness
-- in the Quipper circuit generation code. 

-- Please note that currently only the ASCII backend can make actual
-- use of laziness; the graphics-based backends must generate the
-- whole circuit before it can be printed.

-- We also apply a transformer, to illustrate the laziness of
-- transformers as well.

infinite_circuit :: Qubit -> Qubit -> Qubit -> Circ (Qubit, Qubit, Qubit)
infinite_circuit q r s = do
  qnot_at q `controlled` [r,s]
  hadamard_at r
  infinite_circuit s q r
  
infinite_circuit_transformed = decompose_generic Binary infinite_circuit

main1 = 
  print_simple ASCII infinite_circuit

main = 
  print_simple ASCII infinite_circuit_transformed
