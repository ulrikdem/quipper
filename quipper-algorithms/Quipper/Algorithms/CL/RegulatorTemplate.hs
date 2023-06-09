{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | An experimental adaptation of the code from "Quipper.Algorithms.CL.Types", "Quipper.Algorithms.CL.Math", etc. to build circuits automatically using Template Haskell and "Quipper.Internal.CircLifting".
--
-- Extremely incomplete.

module Quipper.Algorithms.CL.RegulatorTemplate where

import Quipper
import Text.Printf
import Quipper.Libraries.FPReal
import Quipper.Algorithms.CL.Types
import Quipper.Libraries.Arith
import Control.Monad

-- * Auxiliary functions specific to the Class Number algorithm

d_of_ideal :: Ideal -> Integer
d_of_ideal (Ideal bigD m l a b) = d_of_bigD bigD

d_of_idealQ :: IdealQ -> Integer
d_of_idealQ (Ideal bigD m l a b) = d_of_bigD bigD

equal_d_of_ideals :: Ideal -> Ideal -> BoolParam
equal_d_of_ideals i j = if ((d_of_ideal i) == (d_of_ideal j)) then PTrue else PFalse

equal_d_of_idealQs :: IdealQ -> IdealQ -> BoolParam
equal_d_of_idealQs i j = if ((d_of_idealQ i) == (d_of_idealQ j)) then PTrue else PFalse

template_equal_d_of_ideals :: Circ (IdealQ -> Circ (IdealQ -> Circ BoolParam))
template_equal_d_of_ideals = return $ \i -> return $ \j -> return (equal_d_of_idealQs i j)

template_and :: Circ ([Qubit] -> Circ Qubit)
template_and = return $ \qs -> do
  result_qubit <- qinit False
  qnot result_qubit `controlled` qs

m_of_ideal :: Ideal -> IntM
m_of_ideal (Ideal bigD m l a b) = m

l_of_ideal :: Ideal -> IntM
l_of_ideal (Ideal bigD m l a b) = l

a_of_ideal :: Ideal -> IntM
a_of_ideal (Ideal bigD m l a b) = a

b_of_ideal :: Ideal -> IntM
b_of_ideal (Ideal bigD m l a b) = b

template_m_of_ideal :: Circ (IdealQ -> Circ QDInt)
template_m_of_ideal = return $ \(Ideal bigD m l a b) -> return m 

template_l_of_ideal :: Circ (IdealQ -> Circ QDInt)
template_l_of_ideal = return $ \(Ideal bigD m l a b) -> return l 

template_a_of_ideal :: Circ (IdealQ -> Circ QDInt)
template_a_of_ideal = return $ \(Ideal bigD m l a b) -> return a 

template_b_of_ideal :: Circ (IdealQ -> Circ QDInt)
template_b_of_ideal = return $ \(Ideal bigD m l a b) -> return b

one_of_size :: IntM -> IntM
one_of_size n = 
  case intm_length n of
    Just m -> intm m 1
    Nothing -> error "one_of_size: indeterminate size"

template_one_of_size :: Circ (QDInt -> Circ QDInt)
template_one_of_size = return (\qx -> qinit $ one_of_size $ qc_false qx)

-- * Functions from "Quipper.Algorithms.CL.Types"

-- | Return 'True' if the given ideal is reduced.
build_circuit
isReduced :: Ideal -> Bool
isReduced i = (m_of_ideal i == (one_of_size (m_of_ideal i)) && (l_of_ideal i == a_of_ideal i))

-- assertReduced can't really be implemented as a quantum circuit. The closest we could
-- define is an asserted termination, that would throw an error at circuit run-time if
-- the assertion doesn't hold.
-- | An assert function will throw an error if the assertion is False.
assert :: Bool -> a -> a
assert True a = a
assert False _ = error "False Assertion"
 
-- | A hand-lifted version of assert, that will produce a circuit run-time error
-- if the assertion doesn't hold. This is done by using an asserted discard.
template_assert :: Circ (Qubit -> Circ (a -> Circ a))
template_assert = return $ \assertion -> return $ \a -> do
  qterm True assertion
  return a

-- | This will throw an error if the given ideal isn't reduced. 
-- The corresponding circuit will use an asserted termination, that will throw an 
-- error at circuit run-time if the assertion doesn't hold.
build_circuit
assertReduced :: Ideal -> a -> a
assertReduced i rest = 
  let assertion = isReduced i in
  assert assertion rest 

-- | Check if a given ideal equals another ideal.
build_circuit
idealEquals :: Ideal -> Ideal -> Bool
idealEquals i j = case equal_d_of_ideals i j of
  PFalse -> error "Comparing two ideals of different d"
  PTrue -> and [(m_of_ideal i == m_of_ideal j),
                (l_of_ideal i == l_of_ideal j),
                (a_of_ideal i == a_of_ideal j),
                (b_of_ideal i == b_of_ideal j)]

-- * Functions from "Quipper.Algorithms.CL.CL"

-- * Some dummy functions for testing

data BoolPair = BoolPair Bool Bool

data QubitPair = QubitPair Qubit Qubit

boolPair = BoolPair
template_boolPair = return $ \x -> return $ \y -> return $ QubitPair x y
template_BoolPair = QubitPair

build_circuit
truePair :: BoolPair
truePair = boolPair True True

{-
build_circuit
myAnd :: BoolPair -> Bool
myAnd (BoolPair x y) = x && y
-}

build_circuit
myPlus :: Int -> Int -> Int
myPlus x y = x + y

-- * Some test functions
test_is_reduced :: IO ()
test_is_reduced = let zero = qdint_shape 4
                      ideal = Ideal 17 zero zero zero zero
                  in print_generic ASCII (unpack template_isReduced) ideal

-- * A main function
main :: IO ()
main = test_is_reduced

{-
Notes:

- it’s not quite clear to me what assumptions CircLifting makes about lingering arguments, linearity, etc. e.g.: should 'template_m_of_ideal' and the like return copies of the components, not the components themselves?  Or is this kosher as currently written? 

- I guess at the moment, it *does* assume all input arguments are left lingering, and moreover unmodified (in the sense of the computational basis).  A version not assuming this could be written by using the alternate type translation

#(a -> b) = a -> Circ (a,#b)

- linearity is much more of an issue!  if we write something like 

build_circuit
twice x = x + x

this may well (depending on implementation of 'template_symb_plus_') build to a non-linear circuit (so, runtime error).  With a little more effort we can write examples which will *not* produce runtime errors, but *will* produce incorrect circuit behaviour.

Solution?  One can imagine writing safe circuits like

add_safe qx qy = if (qd_disjoint qx qy) then add qx qy
  else do
    ((qx,qy),qz) <- with_computed_fun (qx,qy)
      (\(qx,qy) -> do
        (qx,qx') <- qc_copy_fun qx
        (qy,qy') <- qc_copy_fun qy
        return (qx,qy,qx',qy'))
      (\(qx,qy,qx',qy') -> 
        (qx',qy',qz) <- add qx' qy'
        return ((qx,qy,qx',qy'),qz))
    return (qx,qy,qz)

One can even imagine doing this generically, so that add_safe = make_safe q_add.  The necessary assumption is just, I think, that q_add leaves qx, qy unmodified.  Given a QData method qd_disjoint, this would not I think be hard.

This may be the wrong solution to the problem, though.  Thoughts?

- It would be really nice if the lifting could deal better with (a) pattern-matching; (b) numeric literals.  (e.g. how to write an algebraic operation like (p x = 17*x + 5)?  Can write this for 'IntM', but can’t translate it correctly.

-}
