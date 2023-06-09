{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "Quipper/Demos/Test.hs" #-}
import Quipper

circuit :: (Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
circuit (a, b, c) = do
  qnot_at a `controlled` [b]
  qnot_at b `controlled` [c]
  hadamard c `controlled` [a,b]
  return (a, b, c)

hadamard2 :: (Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
hadamard2 (h, a, b) = do
  with_ancilla $ \c -> do
    qnot_at c `controlled` [a, b]
    hadamard h `controlled` [c]
    qnot_at c `controlled` [a, b]
  return (h, a, b)

example :: (Qubit, Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit, Qubit)
example (a, b, c, d, e) = do
  circuit (a, b, c)
  circuit (b, c, a)
  with_controls (d .==. 1 .&&. e .==. 0) $ do {
    circuit (a, b, c);
    circuit (b, c, a);
  }
  circuit (a, b, c)
  circuit (b, c, a)
  return (a, b, c, d, e)

main =
  print_simple Preview example
