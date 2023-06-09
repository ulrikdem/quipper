-- | The Quipper Interpreter executable. This is just a wrapper around
-- GHC with a custom preprocessor.

import System.Environment
import System.Exit
import System.Process

usage :: String -> IO ()
usage myname = do
  putStrLn $ myname ++ ": interactive interface to the Quipper compiler"
  putStrLn $ ""
  putStrLn $ "Usage: " ++ myname ++ " [--help|-h]"
  putStrLn $ "       " ++ myname ++ " [anything-other-than-the-above]"
  putStrLn $ ""
  putStrLn $ "Options:"
  putStrLn $ "   (no parameters)          : bitterly complain"
  putStrLn $ "   --help, -h               : print this message"
  putStrLn $ "   any-other-than-the-above : passed on to ghci"
  putStrLn $ ""
  putStrLn $ "This compiler is a wrapper around ghci."
  putStrLn $ "Its usage is therefore exactly the same as that of ghci."
  putStrLn $ "Try `ghci --help' for more information."

-- | Run GHCI with the given command line options.
runGHCI :: [String] -> IO ExitCode
runGHCI args = do
  rawSystem "ghci" args

-- | Run QuipperI with the given command line options.
runQuipperI :: [String] -> IO ExitCode
runQuipperI args = do
  runGHCI ("-F" : "-pgmF" : "quipper-pp" : args)
    
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
   _ -> do
     r <- runQuipperI args
     exitWith r
