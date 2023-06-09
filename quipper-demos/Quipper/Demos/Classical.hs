import Quipper

circuit :: (Bit, Qubit) -> Circ (Qubit, Qubit)
circuit (b,q) = do
  qnot_at q `controlled` b
  r <- prepare b
  gate_W_at q r
  return (q,r)
  
main =   
  print_simple Preview circuit
  
