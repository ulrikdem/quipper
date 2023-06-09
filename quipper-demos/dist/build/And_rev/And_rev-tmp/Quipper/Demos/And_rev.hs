{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "Quipper/Demos/And_rev.hs" #-}
import Quipper

and_gate :: (Qubit, Qubit) -> Circ (Qubit)
and_gate (a, b) = do
  c <- qinit False
  qnot_at c `controlled` [a, b]
  return c

and_list :: [Qubit] -> Circ Qubit
and_list [] = do
  c <- qinit True
  return c
and_list [q] = do
  return q
and_list (q:t) = do
  d <- and_list t
  e <- and_gate (d, q)
  return e

and_rev :: ([Qubit], Qubit) -> Circ ([Qubit], Qubit)
and_rev = classical_to_reversible and_list

main =
  print_generic Preview and_rev (replicate 10 qubit, qubit)


