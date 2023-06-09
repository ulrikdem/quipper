{-# LANGUAGE CPP #-}

-- | This module provides access to the Quipper preprocessor, in a
-- format that can be used in custom Setup scripts to teach Cabal to
-- compile Quipper programs.

module Quipper.Distribution.Preprocessor where

import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import System.Process

-- To prevent CPP from expanding a macro in a Haskell comment.
#define literal(x) x

-- | The Quipper preprocessor, in the format required by Cabal. This
-- can be used in the @Setup.hs@ file of Cabal packages, for example
-- like this:
--
-- > {-# LANGUAGE CPP #-}
-- >
-- > import Distribution.Simple
-- > import Quipper.Distribution.Preprocessor
-- >
-- > -- The following is needed because of an incompatible change in Cabal 2.
-- > #if literal(MIN_VERSION)_Cabal(2,0,0)
-- > wrap x = \_ _ _ -> x
-- > #else
-- > wrap x = \_ _ -> x
-- > #endif
-- > 
-- > main = defaultMainWithHooks simpleUserHooks {
-- >   hookedPreProcessors = [("hs", wrap ppQuipper)]
-- >   }
ppQuipper :: PreProcessor
ppQuipper =
  PreProcessor {
    platformIndependent = True,
    runPreProcessor = mkSimplePreProcessor f
    }
  where
    f inFile outFile verbosity = catchIOError body handler
      where
        body = do
          info verbosity ("Preprocessing " ++ inFile ++ " to " ++ outFile)
          (exitcode, out, err) <- readProcessWithExitCode "quipper-pp" [inFile, inFile, outFile] ""
          case exitcode of
            ExitSuccess -> return ()
            ExitFailure n -> quipper_pp_error err
        handler e = do
          name <- getProgName
          hPutStrLn stdout ""
          hPutStrLn stdout $ name ++ ": unable to run the Quipper preprocessor. Please make sure that quipper-pp is installed and on your PATH. Note that quipper-pp can be found in the package quipper-language."
          hPutStrLn stdout ""
          exitFailure
        quipper_pp_error err = do
          hPutStrLn stdout ""
          hPutStrLn stdout err
          exitFailure
