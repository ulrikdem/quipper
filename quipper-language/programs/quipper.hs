-- | The Quipper executable. This is just a wrapper around GHC
-- with a custom preprocessor.

import System.Environment
import System.Exit
import System.Process

usage :: String -> IO ()
usage myname = do
  putStrLn $ myname ++ ": the Quipper compiler"
  putStrLn $ ""
  putStrLn $ "Usage: " ++ myname ++ " [--help|-h]"
  putStrLn $ "       " ++ myname ++ " [anything-other-than-the-above]"
  putStrLn $ ""
  putStrLn $ "Options:"
  putStrLn $ "   (no parameters)          : bitterly complain"
  putStrLn $ "   --help, -h               : print this message"
  putStrLn $ "   any-other-than-the-above : passed on to ghc"
  putStrLn $ ""
  putStrLn $ "This compiler is a wrapper around ghc."
  putStrLn $ "Its usage is therefore exactly the same as that of ghc."
  putStrLn $ "Try `ghc --help' for more information."

-- | Run GHC with the given command line options.
runGHC :: [String] -> IO ExitCode
runGHC args = do
  rawSystem "ghc" args

-- | Run Quipper with the given command line options.
runQuipper :: [String] -> IO ExitCode
runQuipper args = do
  runGHC ("-F" : "-pgmF" : "quipper-pp" : args)
    
main :: IO ()
main = do
  args <- getArgs
  myname <- getProgName
  case args of
   [] -> do
     putStrLn $ myname ++ ": no input files"
     putStrLn $ "For usage information, try the `--help' option."
     exitWith (ExitFailure 1)
   "-h" : _ -> do
     usage myname
     exitSuccess
   "--help" : _ -> do
     usage myname
     exitSuccess
   _ -> do
     r <- runQuipper args
     exitWith r
