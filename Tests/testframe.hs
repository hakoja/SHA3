import Test.HUnit
import qualified Data.ByteString.Lazy as L
import Text.ParserCombinators.Parsec
import Data.Binary (encode)
import Control.Applicative ((*>),(<*),(<*>),(<$>),liftA3,liftA)
import System.IO

hashFunc :: L.ByteString -> L.ByteString
hashFunc _ = encode "0xF2E180FB5947BE964CD584E22E49624"

tf2Test :: TestFile -> Test
tf2Test tf = let fHeader = header tf
             in TestLabel ("File: " ++ (fName fHeader) ++
                           "Algorithm: " ++ (algName fHeader))
                          (tf2TestList tf)

tf2TestList :: TestFile -> Test
tf2TestList = TestList . map ti2TestCase . testInstances

ti2TestCase :: TestInstance -> Test
ti2TestCase ti = TestCase $ 
   assertEqual (show $ len ti) (digest ti) (hashFunc $ msg ti)



----------------------------- Parse test vector files --------------------------

main :: IO ()
main = do 
   file <- readFile "./testvectors/jh/KAT_MCT/ShortMsgKAT_224.txt"
   let p_result = parse p_testFile "ShortMsgKAT_224.txt" file
   case p_result of  
      Left parseError -> putStrLn "FAIL"
      Right result -> do runTestTT $ tf2TestList result
                         putStrLn "SUCCESS!"
   


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
   where len = f "Len = " read digit
         msg = f "Msg = " encode hexDigit
         digest = f "MD = " encode hexDigit
         f prefix reader format = 
            reader <$> (string prefix *> manyTill format newline)

      
      
      
      
      
      
      
      
      
      
      
  
  
  
  