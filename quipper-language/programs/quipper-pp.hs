-- | The Quipper preprocessor. This is just a wrapper around an awk
-- script.

import Quipper.Internal.Preprocessor

import System.Environment
import System.Exit
import System.IO

-- | Print usage information.
usage :: String -> IO ()
usage myname = do
  putStrLn $ myname ++ ": the Quipper preprocessor"
  putStrLn $ ""
  putStrLn $ "Usage: " ++ myname ++ " <sourcefile> <infile> <outfile>"
  putStrLn $ "       " ++ myname ++ " -f"
  putStrLn $ ""
  putStrLn $ "Arguments and options:"
  putStrLn $ " <sourcefile>  - the name of the original source file"
  putStrLn $ " <infile>      - the name of the file holding the input"
  putStrLn $ " <outfile>     - the name of the file to write the output to"
  putStrLn $ " -f            - act as a filter (for testing)"

main :: IO ()
main = do
  args <- getArgs
  myname <- getProgName
  case args of
   "-h" : _ -> do
     usage myname
     exitSuccess
   "--help" : _ -> do
     usage myname
     exitSuccess
   ["-f"] -> do
     (r, err) <- preprocess_handle "stdin" stdin stdout
     hPutStr stderr err
     exitWith r
   [filename, infile, outfile] -> do
     (r, err) <- preprocess_file filename infile outfile
     hPutStr stderr err
     exitWith r
   _ -> do
     putStrLn $ myname ++ ": illegal command line"
     putStrLn $ "For usage information, try the `--help' option."
     exitFailure
