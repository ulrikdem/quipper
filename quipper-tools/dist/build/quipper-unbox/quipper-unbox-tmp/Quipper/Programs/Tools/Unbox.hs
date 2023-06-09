{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "Quipper/Programs/Tools/Unbox.hs" #-}
-- ----------------------------------------------------------------------
-- | This tool unwinds a circuit by inlining all top-level boxed
-- subroutines.  It can sometimes be used to pre-process circuits for
-- use by other tools that may not yet work correctly with boxed
-- subroutines. Note, however, that this can substantially increase
-- the size of the circuit representation.
-- 
-- Note that only top-level boxed subroutines are inlined; the tool
-- may have to be applied several times to remove all levels of boxes.
-- 
-- This is only an illustration for how to write such tools; another
-- tool could be written that does \"deep\" unboxing.

module Main where

import Quipper
import Quipper.Libraries.QuipperASCIIParser
import Quipper.Libraries.Unboxing
import System.Environment
import System.Exit
import System.IO

-- | Print a usage message to 'stdout'.
usage :: IO ()
usage = do
  name <- getProgName
  putStr (header name)
    where header name =
            name ++ ": unwind a circuit by inlining all top-level boxed\n"
            ++ "subroutines. Only top-level boxed subroutines are inlined;\n"
            ++ "the tool may have to be applied several times to remove all\n"
            ++ "levels of boxes. This is only an illustration for how to write\n"
            ++ "such tools; another tool could be written that does \"deep\"\n"
            ++ "unboxing.\n"


-- | Main function: read from 'stdin', do the decomposition, and write
-- to 'stdout'.
main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [] -> return ()
    "-h" : _ -> do
      usage
      exitSuccess
    "--help" : _ -> do
      usage
      exitSuccess
    o : _ -> do
      hPutStrLn stderr ("Bad argument or option: '" ++ o ++ "'. Try --help for more info.")
      exitFailure

  (ins,circuit) <- parse_from_stdin  
  let unboxed_circuit = unbox circuit
  print_generic ASCII unboxed_circuit ins
