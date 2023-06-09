-- ----------------------------------------------------------------------
-- | This tool reads a circuit from standard input and calculates gate
-- counts.

module Main where

import Quipper
import Quipper.Libraries.QuipperASCIIParser
import System.Environment
import System.Exit
import System.IO

-- | Print a usage message to 'stdout'.
usage :: IO ()
usage = do
  name <- getProgName
  putStr (header name)
    where header name =
            name ++ ": read a circuit from standard input and calculate gate counts.\n"

-- | Main function: read from 'stdin', and write gate counts to
-- 'stdout'.
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
  print_generic GateCount circuit ins
