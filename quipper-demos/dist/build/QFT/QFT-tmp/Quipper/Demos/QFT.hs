{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "Quipper/Demos/QFT.hs" #-}
import Quipper
import Quipper.Libraries.QFT

main :: IO ()
main = print_generic Preview qft_little_endian (replicate 5 qubit)
