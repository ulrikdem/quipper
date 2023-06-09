-- | The QuipperDoc executable. This is just a wrapper around Haddock
-- with a custom preprocessor.

import System.Environment
import System.Exit
import System.Process

usage :: String -> IO ()
usage myname = do
  putStrLn $ myname ++ ": documentation generator for the Quipper language"
  putStrLn $ ""
  putStrLn $ "Usage: " ++ myname ++ " [--help|-h]"
  putStrLn $ "       " ++ myname ++ " [anything-other-than-the-above]"
  putStrLn $ ""
  putStrLn $ "Options:"
  putStrLn $ "   (no parameters)          : bitterly complain"
  putStrLn $ "   --help, -h               : print this message"
  putStrLn $ "   any-other-than-the-above : passed on to haddock"
  putStrLn $ ""
  putStrLn $ "This program is a wrapper around haddock."
  putStrLn $ "Its usage is therefore exactly the same as that of haddock."
  putStrLn $ "Try `haddock --help' for more information."

-- | Run Haddock with the given command line options.
runHaddock :: [String] -> IO ExitCode
runHaddock args = do
  rawSystem "haddock" args

-- | Run QuipperDoc with the given command line options.
runQuipperDoc :: [String] -> IO ExitCode
runQuipperDoc args = do
  runHaddock ("--optghc=-F" : "--optghc=-pgmF" : "--optghc=quipper-pp" : args)
    
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
     r <- runQuipperDoc args
     exitWith r
