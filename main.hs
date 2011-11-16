{-# LANGUAGE PatternGuards #-}

module Main where

import Tests.Testframe
import System.Environment (getArgs)
import Data.Char (toLower)
import System.Console.GetOpt
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)


data Flag = Alg String | TestVar String

options :: [OptDescr Flag]
options = [Option ['a'] ["algorithm","alg"] (ReqArg (\s -> Alg s) "Algorithm") "Algorithm to test",
           Option ['t','v'] ["testvariant","variant"] (ReqArg (\s -> TestVar s) "Test variant") "Test variant to run"]

parseArgs :: [String] -> IO ()
parseArgs argv = do
    case parse argv of 
        ([], args, [])            -> mapM_ putStrLn args 
        (opts, args, [])
            | [Alg a] <- opts     -> putStrLn ("ALGORITHM: " ++ a) >> mapM_ putStrLn args
            | [TestVar v] <- opts -> putStrLn $ "VARIANT: " ++ v
        (_, _, errs)              -> die errs
    
    where parse argv = getOpt Permute options argv
          header     = "Usage: bla, bla, bla..."
          info       = usageInfo header options
          dump       = hPutStrLn stderr
          die errs   = dump (concat errs ++ info) >> exitWith (ExitFailure 1)
          help       = dump info 

main2 = getArgs >>= parseArgs

main = do
   args <- getArgs
   let argLength = length args 
   if args !! 1 == "e"  
      then runExtreme (read $ args !! 2) 
   else do 
      let alg = map toLower $ args !! 0
          testCase = map toLower $ args !! 1
          byteAligned = read $ args !! 2
      case lookup testCase testOptions of 
         Just filePath -> run alg filePath byteAligned 
         Nothing       -> putStrLn $ testCase ++ "is not a valid test file option. \n\nOptions: shortXXX, longXXX"

algorithms = ["jh","skein","groestl"]

testOptions :: [(String,FilePath)]
testOptions = [("short224", "ShortMsgKAT_224.txt"),("short256","ShortMsgKAT_256.txt"),
               ("short384","ShortMsgKAT_384.txt"),("short512","ShortMsgKAT_512.txt"),
               ("long224","LongMsgKAT_224.txt"),("long256","LongMsgKAT_256.txt"),
               ("long384","LongMsgKAT_384.txt"),("long512","LongMsgKAT_512.txt")]
