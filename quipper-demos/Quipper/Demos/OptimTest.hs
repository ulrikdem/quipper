-- | A test for the 'simplify_classical' function. As an example, we use
-- a simple adder defined using the @build_circuit@ keyword. 'main1'
-- outputs the unoptimized circuit, and 'main2' outputs the optimized
-- version.

import Quipper
import Quipper.Libraries.ClassicalOptim
import Quipper.Utils.Auxiliary

-- | Return the majority of three booleans.
build_circuit
majority :: Bool -> Bool -> Bool -> Bool
majority a b c = if (a `bool_xor` b) then c else a

-- | Bit adder. The first input is 'False' for adding, and 'True' for
-- subtracting. The second input is a triple consisting of a carry,
-- and two bits to be added. The output consists of the new carry and
-- the sum.
build_circuit
bit_adder :: Bool -> (Bool,Bool,Bool) -> (Bool,Bool)
bit_adder sign (carry, x,y) =
      let y' = y `bool_xor` sign in
      let z = carry `bool_xor` x `bool_xor` y' in
      let carry' = majority carry x y' in
      (carry', z)

-- | Multi-bit adder. Add two /n/-bit integers, represented as
-- little-tailian bit lists.
build_circuit
adder :: [Bool] -> [Bool] -> [Bool]
adder f l = 
  reverse $ snd $ fold_right_zip (bit_adder False) (False, reverse l, reverse f)

-- | Wrapper around 'template_adder'.
myAdder :: ([Qubit],[Qubit]) -> Circ [Qubit]
myAdder (x,y) = do
  label (x,y) ("x","y")
  z <- unpack template_adder x y
  label z "z"
  return (z)

main1 :: IO()
main1 = do
  print_generic Preview myAdder (replicate 3 qubit, replicate 3 qubit)

main2 :: IO()
main2 = do
  print_generic Preview (simplify_classical myAdder) (replicate 3 qubit, replicate 3 qubit)

main :: IO()
main = main2


