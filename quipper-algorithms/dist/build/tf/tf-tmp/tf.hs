{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "programs/tf.hs" #-}
-- This is a stub main file. It only purpose is to re-export the
-- "real" main function, which is Quipper.Algorithms.TF.Main. The
-- reason for putting the "real" main into a library is that we want
-- documentation to be generated for it.

import qualified Quipper.Algorithms.TF.Main
main = Quipper.Algorithms.TF.Main.main
