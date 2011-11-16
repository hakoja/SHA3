{-# LANGUAGE BangPatterns #-}

module Data.Digest.GroestlMutable where

import Data.Word (Word64, Word8)
import Data.Int (Int64) 	
import Data.Bits
import Data.List (foldl')
import qualified Data.Binary as B
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Exception.Base (assert)
import Text.Printf
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Prelude hiding (truncate)

import Data.Digest.GroestlTables
import Data.Digest.Groestl

testQ :: V.Vector Word64
testQ = V.fromList [0x6162638000000000, 0, 0, 0, 0, 0, 0, 1]

testP :: V.Vector Word64
testP = V.fromList [0x6162638000000000, 0, 0, 0, 0, 0, 0, 0xe1]

testQM = putStrLn . show . map printWAsHex . V.toList . runQ $ testQ       
testPM = putStrLn . show . map printWAsHex . V.toList . runP $ testP


-----------------------------------------------------------------------------------

groestl224M :: Int64 -> L.ByteString -> L.ByteString
groestl224M dataBitLen 
    | dataBitLen < 0 = error "The data length can not be less than 0"
    | otherwise = truncate G224 . outputTransformation . foldl' f512M h0_224 . parseMessage dataBitLen 512


f512M :: V.Vector Word64 -> V.Vector Word64 -> V.Vector Word64
f512M !h !m = V.zipWith3 xor3 h (runP inP) (runQ m)
    where xor3 x1 x2 x3 = x1 `xor` x2 `xor` x3
          inP = V.zipWith xor h m

runQ :: V.Vector Word64 -> V.Vector Word64
runQ x = V.create $ do
     w <- MV.unsafeNew 8
     V.unsafeCopy w x
     permQM w

runP :: V.Vector Word64 -> V.Vector Word64
runP x = runST $ V.unsafeThaw x >>= permPM >>= V.unsafeFreeze

permPM :: MV.STVector s Word64 -> ST s (MV.STVector s Word64)
permPM x = V.foldM' rnd512PM x (V.enumFromStepN 0 0x0100000000000000 10)

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

permQM :: MV.STVector s Word64 -> ST s (MV.STVector s Word64)
permQM x = V.foldM' rnd512QM  x (V.enumFromN 0 10)

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
    
    y <- MV.new 8
    
    columnM 0 x y 1 3 5 7 0 2 4 6
    columnM 1 x y 2 4 6 0 1 3 5 7
    columnM 2 x y 3 5 7 1 2 4 6 0
    columnM 3 x y 4 6 0 2 3 5 7 1
    columnM 4 x y 5 7 1 3 4 6 0 2
    columnM 5 x y 6 0 2 4 5 7 1 3
    columnM 6 x y 7 1 3 5 6 0 2 4
    columnM 7 x y 0 2 4 6 7 1 3 5
        
    return y

update :: MV.STVector s Word64 -> Int -> Word64 -> Word64 -> ST s () 
update x i rndNr c = do
    xi <- MV.unsafeRead x i 
    MV.unsafeWrite x i (xi `xor` c `xor` rndNr)


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

tableLookup :: MV.STVector s Word64 -> Int -> Int -> ST s Word64
tableLookup x i c = do
    t <- MV.unsafeRead x c
    return . V.unsafeIndex tables $ i * 256 + (fromIntegral $ extractByte i t)
    where extractByte :: Int -> Word64 -> Word8   
          extractByte n w = fromIntegral $ shiftR w (8 * (7 - n))
