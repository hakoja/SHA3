import Test.HUnit
import qualified Data.ByteString.Lazy as L
import Text.ParserCombinators.Parsec
import Control.Applicative ((*>),(<*),(<*>),(<$>),liftA3,liftA)
import System.IO
import Data.Word (Word8)
import Control.Monad (void)

-- The hash function to test. 
hashFunc :: L.ByteString -> L.ByteString
hashFunc _ = L.pack constVector
--hashFunc = skein
--hashFunc = keccack


constVector = [0x2C,0x99,0xDF,0x88,0x9B,0x01,0x93,0x09,0x05,0x1C,
               0x60,0xFE,0xCC,0x2B,0xD2,0x85,0xA7,0x74,0x94,0x0E,
               0x43,0x17,0x5B,0x76,0xB2,0x62,0x66,0x30]

main :: IO ()
main = do 
   file <- readFile "./testvectors/jh/KAT_MCT/ShortMsgKAT_224.txt"
   let p_result = parse p_testFile "ShortMsgKAT_224.txt" file
   case p_result of  
      Left parseError -> putStrLn (show parseError)
      Right testFile -> void . runTestTT $ tf2Test testFile


----------------------------- Create a test suite --------------------------

tf2Test :: TestFile -> Test
tf2Test tf = let fHeader = header tf
             in TestLabel ("File: " ++ (fName fHeader) ++
                           " Algorithm: " ++ (algName fHeader))
                          (tf2TestList tf)

tf2TestList :: TestFile -> Test
tf2TestList = TestList . map ti2TestCase . testInstances

ti2TestCase :: TestInstance -> Test
ti2TestCase ti = TestCase $ 
   assertEqual ("Len = " ++ (show $ len ti)) (digest ti) (hashFunc undefined)


----------------------------- Parse test vector files --------------------------


data TestFile = TestFile {
         header :: Header,
         testInstances :: [TestInstance]
      } deriving (Show)

data Header = Header {
         fName :: String,
         algName :: String
      } deriving (Show)

data TestInstance = TI {
         len :: Int,
         msg :: L.ByteString,
         digest :: L.ByteString
      } deriving (Show)
      
p_testFile :: GenParser Char st TestFile
p_testFile = TestFile <$> 
   (p_header <* newline) <*> (sepEndBy p_testInstance newline)

p_header :: GenParser Char st Header
p_header = do
   tfn <- string "# " *> manyTill anyChar (string ".txt\n")
   alg <- string "# Algorithm Name: " *> manyTill anyChar newline
   manyTill anyChar newline
   return $ Header tfn alg

p_testInstance :: GenParser Char st TestInstance
p_testInstance = liftA3 TI len msg digest
   where len = p "Len = " read digit
         msg = p "Msg = " L.pack p_hexNumber
         digest = p "MD = " L.pack p_hexNumber
         p prefix reader format = reader <$> (string prefix *> manyTill format newline)

p_hexNumber :: GenParser Char st Word8
p_hexNumber = read . ("0x" ++) <$> count 2 hexDigit

      
      
      

      
      
      
      
      
      
      
  
  
  
  