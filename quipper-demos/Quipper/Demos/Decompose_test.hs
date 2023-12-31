import Quipper
import Quipper.Libraries.Decompose

toffoli :: (Qubit,Qubit,Qubit) -> Circ (Qubit,Qubit,Qubit)
toffoli (q1,q2,q3) = do
 qnot_at q3 `controlled` (q1,q2)
 return (q1,q2,q3)

boxed :: (Qubit,Qubit,Qubit) -> Circ (Qubit,Qubit,Qubit)
boxed = box "Toffoli" toffoli


tof :: (Qubit,Qubit,Qubit,Qubit,Qubit) -> Circ ()
tof (q1,q2,q3,q4,q5) = do
 boxed (q3,q4,q5) `controlled` (q1,q2)
 return ()
 
main :: IO ()
main = do
  print_simple Preview tof
  print_simple Preview (decompose_generic Logical tof)
  print_simple Preview (decompose_generic Toffoli tof)
  print_simple Preview (decompose_generic Binary tof)
