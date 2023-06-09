{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "Quipper/Demos/QRAM.hs" #-}

import Quipper
import Quipper.Libraries.Qram
import Quipper.Libraries.Arith

test_qram :: [Qubit] -> QDInt -> Circ ()
test_qram list index = do
  label (list,index) ("list","index")
  with_computed (indexed_access list index) $ \element -> do
    label element "element"
    hadamard_at element


main :: IO ()
main = print_generic Preview test_qram (replicate 8 qubit) (qdint_shape 3)
