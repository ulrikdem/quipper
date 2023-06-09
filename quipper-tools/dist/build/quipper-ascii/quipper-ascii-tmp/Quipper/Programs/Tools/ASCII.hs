{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "Quipper/Programs/Tools/ASCII.hs" #-}
-- ----------------------------------------------------------------------
-- | This tool is the identity function on circuits: it reads a
-- circuit from standard input, and then outputs the same circuit to
-- standard output. This program serves as an illustration for how to
-- write the simplest kind of standalone circuit processing tool.

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
            name ++ ": the identity function on circuits: read a\n"
            ++ "circuit from standard input, and then outputs the same circuit to\n"
            ++ "standard output. This program serves as an illustration for how to\n"
            ++ "write the simplest kind of standalone circuit processing tool.\n"

-- | Main function: read from 'stdin', then write to 'stdout'. 
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
  print_generic ASCII circuit ins
