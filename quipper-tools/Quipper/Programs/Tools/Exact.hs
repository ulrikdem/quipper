-- ----------------------------------------------------------------------
-- | This tool decomposes all gates that permit exact Clifford+/T/
-- representations into the following concrete gate base for
-- Clifford+/T/:
--
-- * controlled-not (with one positive or negative control),
--
-- * any single-qubit Clifford gates,
--
-- * /T/ and /T/[sup †].
--
-- Classical controls and classical gates are not subject to the gate
-- base, and are left untouched.
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
            name ++ ": decompose all gates that permit exact Clifford+T\n"
            ++ "representations into the following concrete gate base for\n"
            ++ "Clifford+T: CNOT (with one positive or negative control),\n"
            ++ "any single-qubit Clifford gates, T, and T-inverse.\n"
            ++ "Classical controls and classical gates are not subject to the gate\n"
            ++ "base, and are left untouched.\n"

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
  let decomposed_circuit = decompose_generic Exact circuit
  print_generic ASCII decomposed_circuit ins
