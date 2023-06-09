-- | This module provides some automated tests for the algebraic
-- optimization procedure. The main function are:
-- 
-- * 'testArith5': runs the optimization on a reversible adder
-- generated with Template Haskell and exhaustively tests all the
-- values.
-- 
-- * 'testCircSimpl' and 'testCircSwap': using the "Test.QuickCheck"
-- library, perform optimization on randomly generated circuits and
-- test validity for some random input.
module Quipper.Libraries.ClassicalOptim.QuickCheck where

import qualified Data.Map as M
import qualified Data.List as L

import qualified Test.QuickCheck as Test

import qualified Quipper as Q
import qualified Quipper.Internal.Circuit as Q
import qualified Quipper.Libraries.Simulation.ClassicalSimulation as Q
import qualified Quipper.Utils.Auxiliary as Q

import Quipper.Libraries.ClassicalOptim.Circuit
import Quipper.Libraries.ClassicalOptim.Simplification
import Quipper.Libraries.ClassicalOptim.QuipperInterface
import Quipper.Libraries.ClassicalOptim.QuickCheckArith

-- ----------------------------------------------------------------------
-- * Testing an adder

-- | Wrapper around 'template_adder'.
myAdder :: ([Q.Qubit],[Q.Qubit]) -> Q.Circ [Q.Qubit]
myAdder (x,y) = do
  Q.label (x,y) ("x","y")
  z <- Q.unpack template_adder x y
  Q.label z "z"
  return (z)

-- | A synonym for 'myAdder'.
myArith :: ([Q.Qubit], [Q.Qubit]) -> Q.Circ [Q.Qubit]
myArith = myAdder

-- | The optimized version of 'myArith'.
myCircArith :: Int -> (CircState, [Wire])
myCircArith n = quipperFunToMyCirc myArith (replicate n Q.qubit, replicate n Q.qubit)

-- | The 'Q.BCircuit' of the optimized version of 'myArith'.
bCircArith :: Int -> Q.BCircuit
bCircArith n = 
  fst $ simplify_classical' myArith (replicate n Q.qubit, replicate n Q.qubit) 

-- | Extract the underlying 'Wire' from a 'Q.B_Endpoint'.
wire_of_endpoint :: Q.B_Endpoint t t -> t
wire_of_endpoint (Q.Endpoint_Qubit x) = x
wire_of_endpoint (Q.Endpoint_Bit x) = x

-- | Run the reversible adder, non-optimized.
runArithDry :: Int -> Int -> Int -> Int
runArithDry n x y = 
  Q.int_of_boollist_unsigned_bh $ reverse $ Q.run_classical_unary myAdder (Q.boollist_of_int_lh n x, Q.boollist_of_int_lh n y)

-- | Run the reversible adder, optimized.
runArith :: Int -> Int -> Int -> Int
runArith n x y = Q.int_of_boollist_unsigned_bh $ reverse $ map wire_of_endpoint $ map (mymap M.!) out
  where
    list = (Q.boollist_of_int_lh n x) ++ (Q.boollist_of_int_lh n y)
    (simpl_bc,out) = simplify_classical' myAdder (replicate n Q.qubit, replicate n Q.qubit) 
    mymap = 
      Q.run_classical simpl_bc $ 
                      M.fromList $ 
                      zip [0..(length list - 1)] $ map Q.Endpoint_Qubit list

-- | Exhaustively test all the inputs of the optimized adder.
testArith5 :: Bool
testArith5 = foldl (&&) True is_good
  where
    op = runArith 5
    inputs = concat [ map (\x -> (i,x)) [0..31] | i <- [0..31]]
    results = map (uncurry $ op) inputs
    testcase = map (\(x,y) -> mod (x+y) 32) inputs
    is_good = map (\(x,y) -> x == y) $ zip results testcase

-- ----------------------------------------------------------------------
-- * Randomized tests

-- $ The following functions use the "Test.QuickCheck" library.  They
-- generate random circuits, and run them classically before and after
-- optimization.

-- | Return all sublists of the given size.
choose :: [b] -> Int -> [[b]]
choose _  0     = [[]]
choose [] _     = []
choose (x:xs) k = map (x:) (choose xs (k-1)) ++ (choose xs k)

-- | Generate a random circuit with the given number of inputs and
-- ancillas.
genCirc :: Int -> Int -> Test.Gen ([Gate],[Wire])
genCirc size_input nber_ancillas = do
    -- random wire
    let randWire = Test.choose (0, size_input + nber_ancillas - 1)
    -- random gate
    let randGate = do
           type_gate <- Test.choose (1,3) -- say if it is not, cnot or ccnot
           list <- Test.oneof $ map return $ map reverse $ choose [0..(size_input + nber_ancillas - 1)] type_gate
           let (w:ws) = list
           ctls <- mapM (\x -> do
                               b <- Test.oneof $ map return [False,True]
                               return (x,b)) ws
           return (Cnot w ctls)
    -- create output wires
    size_outputs <- Test.choose (1,size_input + nber_ancillas)
    outputs <- Test.oneof $ map return $ choose [0..(size_input + nber_ancillas - 1)] size_outputs
    -- create gates
    nber_gates <- Test.choose (10,50)
    -- let nber_gates = 6
    cnots <- Test.vectorOf nber_gates randGate
    let ancillas = map (\w -> Init True w) [size_input..size_input+nber_ancillas-1]
    return (ancillas ++ cnots, outputs)


-- | Print some sample circuits generated by 'genCirc'.
printSampleCirc :: Int -> Int -> IO ()
printSampleCirc i a = do
     list <- Test.sample' (genCirc i a)
     mapM_ (\(a,ws) -> do
       putStrLn "\n\n"
       putStrLn $ show (a,ws)
       putStrLn "goes into\n"
       putStrLn $ show $ simplRec (a,ws)) list
       
-- | Restrict a map to the given domain.
restrict :: Ord k => [k] -> M.Map k a -> M.Map k a
restrict l m = L.foldl (\a b -> M.insert b (m M.! b) a) M.empty l

-- | Test of 'simplRec'.
testCircSimpl :: Test.Property
testCircSimpl = Test.forAll (genCirc 2 3) $ \(f,outs) -> 
                Test.forAll (Test.vectorOf 2 $ Test.oneof [return False,return True]) $ \b -> 
                let m = M.fromList (zip [0..10] b) in
                let (f',o') = simplRec (f,outs) in
                L.map ((evalCirc m f) M.!) outs == L.map ((evalCirc m f') M.!) o'

-- | Test of 'alg_swap'.
testCircSwap :: Test.Property
testCircSwap = Test.forAll (genCirc 2 3) $ \(f,outs) -> 
                Test.forAll (Test.vectorOf 2 $ Test.oneof [return False,return True]) $ \b -> 
                let m = M.fromList (zip [0..10] b) in
                let (f',o') = alg_swap (f,outs) in
                L.map ((evalCirc m f) M.!) outs == L.map ((evalCirc m f') M.!) o'

-- | A wrapper function to easily run a test.
-- Within the interpreter @quipperi@, use, e.g., as
-- 
-- > myQuickTest testCircSimpl
myQuickTest :: Test.Property -> IO Test.Result
myQuickTest x = Test.quickCheckWithResult (Test.stdArgs {Test.maxSuccess = 1000}) x

