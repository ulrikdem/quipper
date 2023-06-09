{-# LANGUAGE CPP #-}

import Distribution.Simple
import Distribution.Superdoc
import Quipper.Distribution.Preprocessor

-- The following is needed because of an incompatible change in Cabal 2.
#if MIN_VERSION_Cabal(2,0,0)
wrap x = \_ _ _ -> x
#else
wrap x = \_ _ -> x
#endif

main = defaultMainWithHooks superdocHooks {
  hookedPreProcessors = [("hs", wrap ppQuipper)]
  }
