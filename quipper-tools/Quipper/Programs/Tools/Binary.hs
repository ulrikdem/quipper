-- ----------------------------------------------------------------------
-- | This tool decomposes a circuit into binary gates.

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
            name ++ ": decompose a circuit into binary gates.\n"

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
  let decomposed_circuit = decompose_generic Binary circuit
  print_generic ASCII decomposed_circuit ins
