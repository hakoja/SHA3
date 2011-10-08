
module Tests.Testframe where 
         

import Test.HUnit
import qualified Data.ByteString.Lazy as L
import Text.ParserCombinators.Parsec
import Control.Applicative ((*>),(<*),(<*>),(<$>),liftA3,liftA)
import System.IO
import Data.Word (Word8)
import Control.Monad (void)
import Data.Int (Int64)
import Text.Printf (printf)

import Data.Digest.JH224

-- The hash function to test. 
hashFunc :: Int64 -> L.ByteString -> L.ByteString
hashFunc = jh224
--hashFunc _ = L.pack constVector
--hashFunc = skein
--hashFunc = keccack


constVector = [0x2C,0x99,0xDF,0x88,0x9B,0x01,0x93,0x09,0x05,0x1C,
               0x60,0xFE,0xCC,0x2B,0xD2,0x85,0xA7,0x74,0x94,0x0E,
               0x43,0x17,0x5B,0x76,0xB2,0x62,0x66,0x30]

main :: IO ()
main = do 
   file <- readFile "./Tests/testvectors/jh/KAT_MCT/ShortMsgKAT_224.txt"
   let p_result = parse p_KATFile "ShortMsgKAT_224.txt" file
   case p_result of  
      Left parseError -> putStrLn (show parseError)
      Right testFile -> do 
                           let tests = tf2Test testFile
                           --putStrLn . show $ testFile
                           void . runTestTT $ tf2Test testFile


----------------------------- Create a test suite --------------------------

tf2Test :: KATFile -> Test
tf2Test tf = let fHeader = header tf
             in TestLabel ("File: " ++ (fName fHeader) ++
                           " Algorithm: " ++ (algName fHeader))
                          (tf2TestList tf)

tf2TestList :: KATFile -> Test
tf2TestList = TestList . map kat2TestCase . kats

kat2TestCase :: KAT -> Test
kat2TestCase ti = TestCase $ 
   assertEqual ("Len = " ++ (show dataLen)) expectedDigest (hashFunc dataLen message)
   where dataLen = len ti 
         expectedDigest = digest ti
         message = msg ti



---------------------------- Some types for specifying tests -----------

data KATFile = KATFile {
         header :: Header,
         kats :: [KAT]
      } deriving (Show)

data Header = Header {
         fName :: String,
         algName :: String
      } deriving (Show)

data KAT = KAT {
         len :: Int64,
         msg :: L.ByteString,
         digest :: L.ByteString
      }

instance Show KAT where 
   show (KAT dataLen msg _) = show (dataLen, printAsHex $ L.take 5 msg) 

----------------------------- Parse test vector files --------------------------
  
p_KATFile :: GenParser Char st KATFile
p_KATFile = KATFile <$> 
   (p_header <* newline) <*> (endBy p_KAT (optional newline))

p_header :: GenParser Char st Header
p_header = do
   tfn <- string "# " *> manyTill anyChar (string ".txt\n")
   alg <- string "# Algorithm Name: " *> manyTill anyChar newline
   manyTill anyChar newline
   return $ Header tfn alg

p_KAT :: GenParser Char st KAT
p_KAT = liftA3 KAT len msg digest
   where len = p "Len = " read digit
         msg = p "Msg = " L.pack p_hexNumber
         digest = p "MD = " L.pack p_hexNumber
         p prefix reader format = reader <$> (string prefix *> manyTill format newline)

p_hexNumber :: GenParser Char st Word8
p_hexNumber = read . ("0x" ++) <$> count 2 hexDigit

printAsHex :: L.ByteString -> String
printAsHex = concat . ("0x" :) . map (printf "%02x") . L.unpack