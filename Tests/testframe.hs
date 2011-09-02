import Test.HUnit
import qualified Data.ByteString.Lazy as L
import Text.ParserCombinators.Parsec
import Data.Binary
import Control.Monad (liftM2, liftM3)
import Control.Applicative ((<$))

test1 :: Test 
test1 = TestCase (assertEqual "input1" (00 ::Integer) (0xF2E180FB5947BE964CD584E22E496242C6A329C577FC4CE8C36D34C3))

tests = TestList [TestLabel "JH ShortMsgKAT_224" test1]


data TestFile = TestFile {
         header :: Header,
         testsInstances :: [TestInstance]
      }

data Header = Header {
         testFileName :: String,
         algName :: String
      } deriving (Show)

data TestInstance = TI {
         len :: Int,
         msg :: L.ByteString,
         digest :: L.ByteString
      } deriving (Show)
      
testFile :: GenParser Char st TestFile
testFile = liftM2 TestFile (parseHeader) (sepBy testInstance eol)

parseHeader :: GenParser Char st Header
parseHeader = do
   tfn <- between (string "# ") newline $ manyTill anyChar (string ".txt")
   alg <- between (string "# Algorithm Name: ") newline $ many (noneOf "\n")
   skipMany1 (manyTill anyChar newline)
   return $ Header tfn alg

testInstance :: GenParser Char st TestInstance
testInstance = liftM3 TI len msg digest
   where len = undefined --string "Len = " *> many digit
         msg = undefined --string "Msg = " *> manyTill 
         digest = undefined
      
eol = count 2 newline
      
      
      
      
      
      
      
      
      
      
      
  
  
  
  