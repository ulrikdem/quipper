{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "Quipper/Demos/Example1.hs" #-}
  import Quipper

  example1 (q, a, b, c) = do
    hadamard a
    qnot_at c `controlled` [a, b]
    hadamard q `controlled` [c]
    qnot_at c `controlled` [a, b]
    hadamard a

  main = print_simple Preview example1
