{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "programs/gse.hs" #-}
-- This is a stub main file. It only purpose is to re-export the
-- "real" main function, which is Quipper.Algorithms.GSE.Main. The
-- reason for putting the "real" main into a library is that we want
-- documentation to be generated for it.

import qualified Quipper.Algorithms.GSE.Main
main = Quipper.Algorithms.GSE.Main.main
