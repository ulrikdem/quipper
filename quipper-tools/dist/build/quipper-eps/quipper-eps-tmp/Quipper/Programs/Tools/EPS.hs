{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "Quipper/Programs/Tools/EPS.hs" #-}
-- ----------------------------------------------------------------------
-- | This tool reads a circuit from standard input and converts it to
-- the graphical EPS format. The converted circuit is written to
-- standard output.

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
            name ++ ": read a circuit from standard input and convert it to EPS.\n"

-- | Main function: read from 'stdin' and write to 'stdout'.
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
  print_generic EPS circuit ins
