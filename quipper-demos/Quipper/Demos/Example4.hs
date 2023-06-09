import Quipper

example4 :: (Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
example4(q, a, b) = do
  with_ancilla $ \c -> do
    qnot_at c `controlled` [a, b]
    hadamard q `controlled` [c]
    qnot_at c `controlled` [a, b]
  return (q, a, b)

main = print_simple Preview example4
