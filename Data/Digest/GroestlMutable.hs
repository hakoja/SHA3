{-# LANGUAGE BangPatterns, Rank2Types #-}

module Data.Digest.GroestlMutable (
           groestl224M,
           groestl224,
           printWAsHex,
           printAsHex
       ) where

import Data.Word (Word64, Word8)
import Data.Int (Int64) 	
import Data.Bits (xor, shiftR, setBit)
import Data.List (foldl')
import qualified Data.Binary as B
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad
import Control.Monad.ST
import Prelude hiding (truncate)
import Text.Printf (printf)

import Data.Digest.GroestlTables


-------------------------------------- Data types used in the implementation ----------------------

data DigestLength = G224 | G256 | G384 | G512

---------------------------------- A port of the optimized 64-bit C version -----------------------

groestl224M :: Int64 -> L.ByteString -> L.ByteString
groestl224M dataBitLen 
    | dataBitLen < 0 = error "The data length can not be less than 0"
    | otherwise = truncate G224 . outputTransformation . foldl' f512M h0_224 . parseMessage dataBitLen 512


groestl224 :: Int64 -> L.ByteString -> L.ByteString
groestl224 dataBitLen 
    | dataBitLen < 0 = error "The data length can not be less than 0"
    | otherwise = truncate G224 . outputTransformation . compress dataBitLen
    where {-# INLINE compress #-}
          compress d xs = runST (foldM f512 h0_224 $ parseMessage d 512 xs)

--{-# INLINE f512M #-}
f512M :: V.Vector Word64 -> V.Vector Word64 -> V.Vector Word64
f512M h m = V.zipWith3 xor3 h (runP inP) (runQ m)
    where xor3 x1 x2 x3 = x1 `xor` x2 `xor` x3
          inP = V.zipWith xor h m

f512 :: V.Vector Word64 -> V.Vector Word64 -> ST s (V.Vector Word64)
f512 h m = do
    outP <- permPM =<< V.unsafeThaw inP
    outQ <- permQM =<< V.unsafeThaw m
    liftM2 (V.zipWith3 xor3 h) (V.unsafeFreeze outP) (V.unsafeFreeze outQ)    
    where xor3 x1 x2 x3 = x1 `xor` x2 `xor` x3
          inP = V.zipWith xor h m

--bar :: Int64 -> L.ByteString -> L.ByteString
--bar dataBitLen xs = truncate G224 . outputTransformation $ runST $ foldM f512 h0_224 $ parseMessage dataBitLen 512 xs

--{-# INLINE runP #-}
runP :: V.Vector Word64 -> V.Vector Word64
runP x = runST $ V.unsafeThaw x >>= permPM >>= V.unsafeFreeze

--{-# INLINE runQ #-}
runQ :: V.Vector Word64 -> V.Vector Word64
runQ x = runST $ V.unsafeThaw x >>= permQM >>= V.unsafeFreeze

--{-# INLINE permPM #-}
permPM :: MV.STVector s Word64 -> ST s (MV.STVector s Word64)
permPM x = V.foldM' rnd512PM x (V.enumFromStepN 0 0x0100000000000000 10)

{-# INLINE permQM #-}
permQM :: MV.STVector s Word64 -> ST s (MV.STVector s Word64)
permQM x = V.foldM' rnd512QM  x (V.enumFromN 0 10)

{-# INLINE rnd512PM #-}
rnd512PM :: MV.STVector s Word64 -> Word64 -> ST s (MV.STVector s Word64)
rnd512PM x rndNr = do 
    update x 0 rndNr 0x0000000000000000
    update x 1 rndNr 0x1000000000000000
    update x 2 rndNr 0x2000000000000000
    update x 3 rndNr 0x3000000000000000
    update x 4 rndNr 0x4000000000000000
    update x 5 rndNr 0x5000000000000000
    update x 6 rndNr 0x6000000000000000
    update x 7 rndNr 0x7000000000000000
    
    y <- MV.unsafeNew 8
    
    columnM 0 x y 0 1 2 3 4 5 6 7
    columnM 1 x y 1 2 3 4 5 6 7 0
    columnM 2 x y 2 3 4 5 6 7 0 1
    columnM 3 x y 3 4 5 6 7 0 1 2
    columnM 4 x y 4 5 6 7 0 1 2 3
    columnM 5 x y 5 6 7 0 1 2 3 4
    columnM 6 x y 6 7 0 1 2 3 4 5
    columnM 7 x y 7 0 1 2 3 4 5 6
        
    return y

-- !!! Inlining this function leads to 4 times the run-time. 
-- Why?! It's practically the same as rnd512PM, so why does this perform sp badly?
--{-# INLINE rnd512QM #-}  
rnd512QM :: MV.STVector s Word64 -> Word64 -> ST s (MV.STVector s Word64)
rnd512QM x rndNr = do 
    update x 0 rndNr 0xffffffffffffffff
    update x 1 rndNr 0xffffffffffffffef
    update x 2 rndNr 0xffffffffffffffdf
    update x 3 rndNr 0xffffffffffffffcf
    update x 4 rndNr 0xffffffffffffffbf
    update x 5 rndNr 0xffffffffffffffaf
    update x 6 rndNr 0xffffffffffffff9f
    update x 7 rndNr 0xffffffffffffff8f
    
    y <- MV.unsafeNew 8
    
    columnM 0 x y 1 3 5 7 0 2 4 6
    columnM 1 x y 2 4 6 0 1 3 5 7
    columnM 2 x y 3 5 7 1 2 4 6 0
    columnM 3 x y 4 6 0 2 3 5 7 1
    columnM 4 x y 5 7 1 3 4 6 0 2
    columnM 5 x y 6 0 2 4 5 7 1 3
    columnM 6 x y 7 1 3 5 6 0 2 4
    columnM 7 x y 0 2 4 6 7 1 3 5
        
    return y

{-# INLINE update #-}
update :: MV.STVector s Word64 -> Int -> Word64 -> Word64 -> ST s () 
update x i rndNr c = do
    xi <- MV.unsafeRead x i 
    MV.unsafeWrite x i (xi `xor` c `xor` rndNr)

----{-# INLINE columnM #-}
columnM :: Int 
        -> MV.STVector s Word64
        -> MV.STVector s Word64
        -> Int -> Int -> Int -> Int
        -> Int -> Int -> Int -> Int 
        -> ST s ()
columnM i x y c0 c1 c2 c3 c4 c5 c6 c7 = do
    x0 <- tableLookup x 0 c0
    x1 <- tableLookup x 1 c1
    x2 <- tableLookup x 2 c2
    x3 <- tableLookup x 3 c3 
    x4 <- tableLookup x 4 c4
    x5 <- tableLookup x 5 c5 
    x6 <- tableLookup x 6 c6
    x7 <- tableLookup x 7 c7      
    
    MV.unsafeWrite y i (x0 `xor` x1 `xor` x2 `xor` x3 `xor` x4 `xor` x5 `xor` x6 `xor` x7) 

{-# INLINE tableLookup #-}
tableLookup :: MV.STVector s Word64 -> Int -> Int -> ST s Word64
tableLookup x !i !c = do
    t <- MV.unsafeRead x c
    let v = V.unsafeIndex tables (i * 256 + fromIntegral (extractByte i t))
    return v
    where extractByte :: Int -> Word64 -> Word8   
          {-# INLINE extractByte #-}
          extractByte n w = fromIntegral $ shiftR w (8 * (7 - n))

outputTransformation :: V.Vector Word64 -> V.Vector Word64
outputTransformation !x = V.zipWith xor (permP' x) x
    where permP' y = V.create (do
              v <- MV.unsafeNew 8 
              V.unsafeCopy v y
              permPM v)

---------------------------- Parsing, padding and truncating ------------------------------

parseMessage :: Int64 -> Int64 -> L.ByteString -> [V.Vector Word64]
parseMessage dataLen blockLen xs
   | L.null suf   = pad dataLen blockLen pre
   | otherwise    = parseBlock pre : parseMessage dataLen blockLen suf
   where (!pre,suf) = L.splitAt 64 xs


parseBlock :: L.ByteString  -> V.Vector Word64
parseBlock = V.unfoldr (\bs -> if L.null bs then Nothing else Just (G.runGet G.getWord64be bs, L.drop 8 bs))

-- This function is a mess. Needs to be cleaned up!
pad :: Int64 -> Int64 -> L.ByteString -> [V.Vector Word64]
pad dataLen blockLen xs
    | dataLen == 0 || L.null xs = [V.fromList [0x8000000000000000, 0,0,0,0,0,0, lengthPad + 1]] 
    | partialBlockLen == 0 = [parseBlock xs, V.fromList [0x8000000000000000, 0,0,0,0,0,0, lengthPad + 1]]  
    | L.length onePadded <= (blockByteLen - 8) = [V.unsafeUpdate (parseBlock fullBlock) (V.singleton (7, lengthPad + 1))]
    | otherwise = [parseBlock fullBlock, V.fromList [0,0,0,0,0,0,0, lengthPad + 2]]
    where 
          onePadded = appendOne xs partialBlockLen
          fullBlock = L.append onePadded (L.take (blockByteLen - L.length onePadded) zeroes) 
          zeroes = L.repeat 0x00
          blockByteLen = blockLen `div` 8
          partialBlockLen = dataLen `rem` blockLen
          lengthPad = fromIntegral $ dataLen `div` blockLen

appendOne :: L.ByteString -> Int64 -> L.ByteString
appendOne xs len
    | len == 0 || L.null xs = L.singleton 0x80
    | byteOffset == 0 =  L.snoc xs 0x80
    | otherwise = L.snoc (L.init xs) (setBit (L.last xs) (7 - byteOffset))
    where byteOffset = fromIntegral $ len `mod` 8


truncate :: DigestLength -> V.Vector Word64 -> L.ByteString
truncate G224 = L.drop 4 . L.concat . map B.encode . V.toList . V.unsafeSlice 4 4 
truncate G256 = L.concat . map B.encode . V.toList . V.unsafeSlice 4 4 
truncate G384 = L.concat . map B.encode . V.toList . V.unsafeSlice 2 6 
truncate G512 = L.concat . map B.encode . V.toList 


------------------------------------------- Initial vector -------------------------------------

h0_224 :: V.Vector Word64
h0_224 = V.fromList [0, 0, 0, 0, 0, 0, 0, 0xe0]

------------------------------------ Some convenience functions and tests -----------------------

printWAsHex :: Word64 -> String
printWAsHex = printf "0x%016x"

printAsHex :: L.ByteString -> String
printAsHex = concat . ("0x" :) . map (printf "%02x") . L.unpack

testQ :: V.Vector Word64
testQ = V.fromList [0x6162638000000000, 0, 0, 0, 0, 0, 0, 1]

testP :: V.Vector Word64
testP = V.fromList [0x6162638000000000, 0, 0, 0, 0, 0, 0, 0xe1]

testQM, testPM :: IO ()
testQM = print . map printWAsHex . V.toList . runQ $ testQ       
testPM = print . map printWAsHex . V.toList . runP $ testP


