-- ----------------------------------------------------------------------
-- | This tool decomposes rotation and phase gates into the
-- Clifford+/T/ base, using the approximate synthesis algorithm of
-- <http://arxiv.org/abs/1403.2975>. Other gates are unchanged.

module Main where

import Quipper
import Quipper.Libraries.QuipperASCIIParser
import Quipper.Libraries.Decompose
import Quipper.Libraries.Synthesis

import Quipper.Utils.CommandLine
import Quipper.Utils.RandomSource

import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Random


-- | A data type to hold the command line options.
data Options = Options {
  opt_prec :: Double,         -- ^ Precision in binary digits.
  opt_keepphase :: KeepPhase, -- ^ Respect global phases?
  opt_rseed :: Maybe StdGen   -- ^ An optional random seed.
} deriving Show

-- | The default options.
defaultOptions :: Options
defaultOptions = Options
  { opt_prec = 30 * digits,
    opt_keepphase = False,
    opt_rseed = Nothing
  }
  
-- | The list of command line options, in the format required by 'getOpt'.
options =
  [ Option ['h'] ["help"]    
      (NoArg help)                
      "print usage info and exit",
    Option ['d'] ["digits"]  
      (ReqArg get_digits "<num>") 
      "precision in decimal digits (default: 30)",
    Option ['b'] ["bits"]    
      (ReqArg get_bits "<num>")   
      "precision in binary digits",
    Option ['p'] ["keepphase"]    
      (NoArg keepphase)   
      "respect global phase",
    Option ['r'] ["rseed"]    
      (ReqArg get_rseed "<s>")   
      "set optional random seed (default: random)"
  ]
    where
      get_digits :: String -> Options -> IO Options
      get_digits str o = 
        case parse_double str of
          Just d -> return o { opt_prec = d * digits }
          _ -> optfail ("Invalid value for option -d -- " ++ str ++ "\n")
          
      get_bits :: String -> Options -> IO Options
      get_bits str o = 
        case parse_double str of
          Just b -> return o { opt_prec = b * bits }
          _ -> optfail ("Invalid value for option -b -- " ++ str ++ "\n")

      keepphase :: Options -> IO Options
      keepphase o = return o { opt_keepphase = True }

      get_rseed :: String -> Options -> IO Options      
      get_rseed string o =
        case reads string of
          [(g, "")] -> return o { opt_rseed = Just g }
          _ -> optfail ("Invalid random seed -- " ++ string ++ "\n")

      help :: Options -> IO Options
      help o = do
        usage
        exitSuccess

-- | Process /argv/-style command line options into an 'Options' structure.
dopts :: [String] -> IO Options
dopts argv =
  case getOpt Permute options argv of
    (o, [], []) -> (foldM (flip id) defaultOptions o)
    (_, _, []) -> optfail "Too many non-option arguments\n"
    (_, _, errs) -> optfail (concat errs)

-- | Print a usage message to 'stdout'.
usage :: IO ()
usage = do
  name <- getProgName
  putStr (usageInfo (header name) options)  
    where header name =
            name ++ ": decompose rotation and phase gates into the Clifford+T\n" 
            ++ "base, using the approximate synthesis algorithm of arXiv:1403.2975.\n"
            ++ "Other gates are unchanged.\n\n"
            ++ "Usage: " ++ name ++ " [option...]"

-- | Main function: read from 'stdin', do the decomposition, and write
-- to 'stdout'.
main :: IO ()
main = do
  argv <- getArgs
  options <- dopts argv
  let p = opt_prec options
      keepphase = opt_keepphase options
      
  -- Set random seed.
  g <- case opt_rseed options of
    Nothing -> newStdGen
    Just g -> return g
  
  (ins,circuit) <- parse_from_stdin
  let decomposed_circuit = decompose_generic (Approximate keepphase p (RandomSource g)) circuit
  print_generic ASCII decomposed_circuit ins
