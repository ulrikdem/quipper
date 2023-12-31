import Quipper
import Quipper.Libraries.Unboxing

rep :: Integer
rep = 3

myfunc :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
myfunc (a,b) = do
  hadamard_at a `controlled` b
  return (b,a)
  
circuit :: (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit)
circuit (a,b,c,d,e,f) = do
  label (a,b,c,d,e,f) ("a","b","c","d","e","f")
  (a,b) <- loopM rep (a,b) myfunc
  (c,d) <- box_loopM "box1" rep (c,d) myfunc
  (e,f) <- unbox (\x -> box_loopM "box1" rep x myfunc) (e,f)
  label (a,b,c,d,e,f) ("a","b","c","d","e","f") 
  return (a,b,c,d,e,f)
  
main = 
  print_simple Preview circuit
