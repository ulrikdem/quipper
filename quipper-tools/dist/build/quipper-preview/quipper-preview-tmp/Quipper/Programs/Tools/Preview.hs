{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "Quipper/Programs/Tools/Preview.hs" #-}
-- ----------------------------------------------------------------------
-- | This tool reads a circuit from standard input and displays it in
-- the previewer. For the previewer to work, Acrobat Reader must be
-- installed. 
-- 
-- Note that it is possible to interrupt the circuit generation with
-- Ctrl-C; in this case, the circuit generated up to that point will
-- be displayed. A second Ctrl-C will kill the program.
-- 
-- Interrupting with Ctrl-C may not work in the Windows operating system.

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
            name ++ ": read a circuit from standard input and display it in the\n"
            ++ "previewer. Circuit generation can be interrupted with Ctrl-C;\n"
            ++ "in this case a partial circuit will be displayed. A second Ctrl-C\n"
            ++ "will kill the program. Ctrl-C may not work in the Windows OS.\n"

-- | Main function: read from 'stdin' and call the previewer. 
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

  (ins,circuit) <- parse_from_stdin_with_handler
  print_generic Preview circuit ins
