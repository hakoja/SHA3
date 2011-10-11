
module Tests.Testframe where 
         

import Test.HUnit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString as B
import Text.ParserCombinators.Parsec
import Control.Applicative ((*>),(<*),(<*>),(<$>))
import Data.Word (Word8)
import Control.Monad (void)
import Data.Int (Int64)
import Text.Printf (printf)
import System.FilePath ((</>))


import Data.Digest.JH224

-- The hash function to test. 
hashFunc :: Int64 -> L.ByteString -> L.ByteString
hashFunc = jh224
--hashFunc = skein
--hashFunc = keccack

xLength = 500000 * 64 --16777216 * 64

runExtreme :: Bool -> IO ()
--runExtreme False = 
--   void . runTestTT . TestCase $ assertEqual "Extremely long" expectedExtreme (hashFunc xLength extreme224)
runExtreme True = 
   void . runTestTT . TestCase $ assertEqual "Extremely long" (Digest expectedExtreme) (hash extreme224) 

run :: String -> FilePath -> Bool -> IO ()
run alg testFile byteAligned = do 
      file <- readFile $ "./Tests/KAT_MCT" </> alg </> testFile
      let p_result = parse p_KATFile testFile file
      case p_result of  
         Left parseError -> putStrLn (show parseError)
         Right katFile -> void . runTestTT $ makeTests byteAligned katFile


----------------------------- Create a test suite --------------------------

makeTests :: Bool -> KATFile -> Test
makeTests True    = TestList . map makeAlignedTest . dropUnaligned . kats
makeTests False   = TestList . map makeUnalignedTest . kats 

dropUnaligned :: [KAT] -> [KAT]
dropUnaligned = filter (\(KAT len _ _) -> len `mod` 8 == 0)

makeAlignedTest :: KAT -> Test
makeAlignedTest kat = 
   TestCase $ assertEqual ("Len = " ++ (show dataLen)) expectedDigest (hash message)
      where dataLen = len kat
            expectedDigest = Digest $ digest kat
            message = msg kat

makeUnalignedTest :: KAT -> Test
makeUnalignedTest kat = 
   TestCase $ assertEqual ("Len = " ++ (show dataLen)) expectedDigest (hashFunc dataLen message)
      where dataLen = len kat
            expectedDigest = digest kat
            message = msg kat

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
p_KAT = do  len <- p "Len = " read digit
            msg <- p "Msg = " (if len == 0 then \_ -> L.empty else L.pack) p_hexNumber
            digest <- p "MD = " L.pack p_hexNumber
            return $ KAT len msg digest   
        where 
         p prefix reader format = reader <$> (string prefix *> manyTill format newline)

p_hexNumber :: GenParser Char st Word8
p_hexNumber = read . ("0x" ++) <$> count 2 hexDigit

printAsHex :: L.ByteString -> String
printAsHex = concat . ("0x" :) . map (printf "%02x") . L.unpack

readAsHex :: String -> L.ByteString
readAsHex = L.pack . map (read . ("0x"++)) . take2

take2 :: [a] -> [[a]]
take2 (a:b:rest) = [a,b] : take2 rest
take2 _          = []

extreme224 :: L.ByteString
extreme224 = L.take xLength . L.cycle $ C.pack "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"

expectedExtreme = readAsHex "B4ABC2827D3547D19B517C673DE2DF2666AE95A0E73ECB213E5C95D4"