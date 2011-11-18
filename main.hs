{-# LANGUAGE PatternGuards #-}

module Main where

import Tests.Testframe
import System.Environment (getArgs)
import Data.Char (toLower)
import System.Console.GetOpt
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)


data Flag = Alg String | TestVar String

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

algorithms = ["jh","groestl"]

testOptions :: [(String,FilePath)]
testOptions = [("short224", "ShortMsgKAT_224.txt"),("short256","ShortMsgKAT_256.txt"),
               ("short384","ShortMsgKAT_384.txt"),("short512","ShortMsgKAT_512.txt"),
               ("long224","LongMsgKAT_224.txt"),("long256","LongMsgKAT_256.txt"),
               ("long384","LongMsgKAT_384.txt"),("long512","LongMsgKAT_512.txt")]
