{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "programs/bf.hs" #-}
-- This is a stub main file. It only purpose is to re-export the
-- "real" main function, which is Quipper.Algorithms.BF.Main. The
-- reason for putting the "real" main into a library is that we want
-- documentation to be generated for it.

import qualified Quipper.Algorithms.BF.Main
main = Quipper.Algorithms.BF.Main.main
