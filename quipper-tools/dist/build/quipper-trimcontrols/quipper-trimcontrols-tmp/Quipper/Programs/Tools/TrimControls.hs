{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "Quipper/Programs/Tools/TrimControls.hs" #-}
-- | This tool trims excess controls from gates. Specifically, it
-- decomposes a circuit so that:
--
-- * not-gates, Pauli /X/-, /Y/-, and /Z/-gates, and /iX/-gates have
-- at most two controls;
--
-- * phase gates of the form Diag(1, φ) have no controls; and
--
-- * all other gates have at most one control.
module Main where

import Quipper
import Quipper.Libraries.QuipperASCIIParser
import Quipper.Libraries.Decompose
import System.Environment
import System.Exit
import System.IO

-- | Print a usage message to 'stdout'.
usage :: IO ()
usage = do
  name <- getProgName
  putStr (header name)
    where header name =
            name ++ ": trims excess controls from gates. Specifically,\n"
            ++ "decompose a circuit so that not-gates, Pauli gates, and\n"
            ++ "iX-gates have at most two controls, phase gates of the form\n"
            ++ "Diag(1, phi) have no controls; and other gates have at most one\n"
            ++ "control.\n"

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
  let decomposed_circuit = decompose_generic TrimControls circuit
  print_generic ASCII decomposed_circuit ins
