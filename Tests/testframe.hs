
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

import qualified Data.Digest.JH224 as JH224
import qualified Data.Digest.JH256 as JH256
import qualified Data.Digest.JH384 as JH384
import qualified Data.Digest.JH512 as JH512

-- The hash function to test. 
hashFunc :: Int64 -> L.ByteString -> L.ByteString
hashFunc = JH224.jh224
--hashFunc = skein
--hashFunc = keccack

cryptoAPIDigest = JH224.Digest
cryptoAPIHash = JH224.hash

--xLength = 100000 * 64
--xLength = 1000000 * 64
xLength = 16777216 * 64

runExtreme :: Bool -> IO ()
runExtreme False = 
   void . runTestTT . TestCase $ assertEqual "Extremely long" 
   (printAsHex $ expectedExtreme224) (printAsHex $ hashFunc (8 * xLength) extreme)
runExtreme True = 
   void . runTestTT . TestCase $ assertEqual "Extremely long" 
   (cryptoAPIDigest expectedExtreme224) (cryptoAPIHash extreme) 

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
   TestCase $ assertEqual ("Len = " ++ (show dataLen)) expectedDigest (cryptoAPIHash message)
      where dataLen = len kat
            expectedDigest = cryptoAPIDigest $ digest kat
            message = msg kat

makeUnalignedTest :: KAT -> Test
makeUnalignedTest kat = 
   TestCase $ assertEqual ("Len = " ++ (show dataLen)) 
                          (printAsHex expectedDigest) 
                          (printAsHex $ hashFunc dataLen message)
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

extreme :: L.ByteString
extreme = L.take xLength . L.cycle $ C.pack "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"

expectedExtreme224 = readAsHex "B4ABC2827D3547D19B517C673DE2DF2666AE95A0E73ECB213E5C95D4"
expectedExtreme256 = readAsHex "58FFBDE520764DFC03B29598ACD70655BB2C245A3D73FDD6EB9E1BC221AF579B"
expectedExtreme384 = readAsHex "836EC726CA5280BBC490A25389D1F507CECED047E9E3DAF0ED3DAA5D9AEDE2DDA89C8B7995F7855A3354AFBFFF1B4935"
expectedExtreme512 = readAsHex "A3053657024A43187CF8C1C82194D5D944A7408EE3B584801309292DEFF8080F88183B5642318456C7C05998C9A70D0F784E4C42D9EBCBA7F2CA25B3FBDE2CE5"

