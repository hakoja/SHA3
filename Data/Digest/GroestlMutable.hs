{-# LANGUAGE BangPatterns, Rank2Types, NoMonomorphismRestriction #-}

module Data.Digest.GroestlMutable (
                  
           fM,
           outputTransform,
           parseMessage,
           pad,         
           truncate,           
           DigestLength(..),
           
           GroestlCtx(..),
           groestlInit,
           groestlUpdate,
           groestlFinalize,
           
           printWAsHex,
           printAsHex
       ) where

import Data.Word (Word8, Word64)
import Data.Int (Int64) 	
import Data.Bits (xor, shiftR, setBit)
import qualified Data.Binary as B
import qualified Data.Binary.Get as G
import qualified Data.Serialize as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad  (liftM, foldM, void, (>=>))
import Control.Monad.ST (ST, runST)
import Control.Arrow ((***))
import Prelude hiding (truncate)
import Text.Printf (printf)

import Data.Digest.GroestlTables

-------------------------------------- Data types used in the implementation ----------------------

data DigestLength = G224 | G256 | G384 | G512
    deriving (Eq, Ord)

type BlockLength = Int64

---------------------------------- A port of the optimized 64-bit C version -----------------------

{-# INLINE fM #-}
fM :: BlockLength -> V.Vector Word64 -> V.Vector Word64 -> ST s (V.Vector Word64)
fM b h m = do
    outP <- V.unsafeFreeze =<< permPM b =<< V.unsafeThaw inP
    outQ <- V.unsafeFreeze =<< permQM b =<< V.unsafeThaw m
    return $ V.zipWith3 xor3 h outQ outP    
    where xor3 x1 x2 x3 = x1 `xor` x2 `xor` x3
          inP = V.zipWith xor h m

{-# INLINE permPM #-}
permPM :: BlockLength -> MV.STVector s Word64 -> ST s (MV.STVector s Word64)
permPM 512  x = V.foldM' rnd512PM  x (V.enumFromStepN 0 0x0100000000000000 10)
permPM 1024 x = V.foldM' rnd1024PM x (V.enumFromStepN 0 0x0100000000000000 14)

-- !!! Inlining this function leads to 4 times the run-time. 
-- See also: rnd512QM
--{-# INLINE permQM #-}
permQM :: BlockLength -> MV.STVector s Word64-> ST s (MV.STVector s Word64)
permQM 512  x = V.foldM' rnd512QM  x (V.enumFromN 0 10)
permQM 1024 x = V.foldM' rnd1024QM x (V.enumFromN 0 14)

{-# INLINE rnd512PM #-}
rnd512PM :: MV.STVector s Word64 -> Word64 -> ST s (MV.STVector s Word64)
rnd512PM x rndNr = do 
    addRndConstant x 0 rndNr 0x0000000000000000
    addRndConstant x 1 rndNr 0x1000000000000000
    addRndConstant x 2 rndNr 0x2000000000000000
    addRndConstant x 3 rndNr 0x3000000000000000
    addRndConstant x 4 rndNr 0x4000000000000000
    addRndConstant x 5 rndNr 0x5000000000000000
    addRndConstant x 6 rndNr 0x6000000000000000
    addRndConstant x 7 rndNr 0x7000000000000000
    
    y <- MV.unsafeNew 8
    
    extractColumn 0 x y 0 1 2 3 4 5 6 7
    extractColumn 1 x y 1 2 3 4 5 6 7 0
    extractColumn 2 x y 2 3 4 5 6 7 0 1
    extractColumn 3 x y 3 4 5 6 7 0 1 2
    extractColumn 4 x y 4 5 6 7 0 1 2 3
    extractColumn 5 x y 5 6 7 0 1 2 3 4
    extractColumn 6 x y 6 7 0 1 2 3 4 5
    extractColumn 7 x y 7 0 1 2 3 4 5 6
        
    return y

-- !!! Inlining this function leads to 4 times the run-time. 
-- Why?! It's practically the same as rnd512PM, so why does this perform sp badly?
--{-# INLINE rnd512QM #-}  
rnd512QM :: MV.STVector s Word64 -> Word64 -> ST s (MV.STVector s Word64)
rnd512QM x rndNr = do 
    addRndConstant x 0 rndNr 0xffffffffffffffff
    addRndConstant x 1 rndNr 0xffffffffffffffef
    addRndConstant x 2 rndNr 0xffffffffffffffdf
    addRndConstant x 3 rndNr 0xffffffffffffffcf
    addRndConstant x 4 rndNr 0xffffffffffffffbf
    addRndConstant x 5 rndNr 0xffffffffffffffaf
    addRndConstant x 6 rndNr 0xffffffffffffff9f
    addRndConstant x 7 rndNr 0xffffffffffffff8f
    
    y <- MV.unsafeNew 8
    
    extractColumn 0 x y 1 3 5 7 0 2 4 6
    extractColumn 1 x y 2 4 6 0 1 3 5 7
    extractColumn 2 x y 3 5 7 1 2 4 6 0
    extractColumn 3 x y 4 6 0 2 3 5 7 1
    extractColumn 4 x y 5 7 1 3 4 6 0 2
    extractColumn 5 x y 6 0 2 4 5 7 1 3
    extractColumn 6 x y 7 1 3 5 6 0 2 4
    extractColumn 7 x y 0 2 4 6 7 1 3 5
        
    return y

rnd1024PM :: MV.STVector s Word64 -> Word64 -> ST s (MV.STVector s Word64)
rnd1024PM x rndNr = do 
    addRndConstant x 0  rndNr 0x0000000000000000
    addRndConstant x 1  rndNr 0x1000000000000000
    addRndConstant x 2  rndNr 0x2000000000000000
    addRndConstant x 3  rndNr 0x3000000000000000
    addRndConstant x 4  rndNr 0x4000000000000000
    addRndConstant x 5  rndNr 0x5000000000000000
    addRndConstant x 6  rndNr 0x6000000000000000
    addRndConstant x 7  rndNr 0x7000000000000000
    addRndConstant x 8  rndNr 0x8000000000000000
    addRndConstant x 9  rndNr 0x9000000000000000
    addRndConstant x 10 rndNr 0xa000000000000000
    addRndConstant x 11 rndNr 0xb000000000000000
    addRndConstant x 12 rndNr 0xc000000000000000
    addRndConstant x 13 rndNr 0xd000000000000000
    addRndConstant x 14 rndNr 0xe000000000000000
    addRndConstant x 15 rndNr 0xf000000000000000
    
    y <- MV.unsafeNew 16
  
    extractColumn 15 x y 15  0  1  2  3  4  5 10     
    extractColumn 14 x y 14 15  0  1  2  3  4  9    
    extractColumn 13 x y 13 14 15  0  1  2  3  8    
    extractColumn 12 x y 12 13 14 15  0  1  2  7    
    extractColumn 11 x y 11 12 13 14 15  0  1  6    
    extractColumn 10 x y 10 11 12 13 14 15  0  5    
    extractColumn  9 x y  9 10 11 12 13 14 15  4    
    extractColumn  8 x y  8  9 10 11 12 13 14  3    
    extractColumn  7 x y  7  8  9 10 11 12 13  2    
    extractColumn  6 x y  6  7  8  9 10 11 12  1    
    extractColumn  5 x y  5  6  7  8  9 10 11  0    
    extractColumn  4 x y  4  5  6  7  8  9 10 15    
    extractColumn  3 x y  3  4  5  6  7  8  9 14    
    extractColumn  2 x y  2  3  4  5  6  7  8 13    
    extractColumn  1 x y  1  2  3  4  5  6  7 12    
    extractColumn  0 x y  0  1  2  3  4  5  6 11    

    return y

rnd1024QM :: MV.STVector s Word64 -> Word64 -> ST s (MV.STVector s Word64)
rnd1024QM x rndNr = do 
    addRndConstant x 0  rndNr 0xffffffffffffffff
    addRndConstant x 1  rndNr 0xffffffffffffffef
    addRndConstant x 2  rndNr 0xffffffffffffffdf
    addRndConstant x 3  rndNr 0xffffffffffffffcf
    addRndConstant x 4  rndNr 0xffffffffffffffbf
    addRndConstant x 5  rndNr 0xffffffffffffffaf
    addRndConstant x 6  rndNr 0xffffffffffffff9f
    addRndConstant x 7  rndNr 0xffffffffffffff8f
    addRndConstant x 8  rndNr 0xffffffffffffff7f
    addRndConstant x 9  rndNr 0xffffffffffffff6f
    addRndConstant x 10 rndNr 0xffffffffffffff5f
    addRndConstant x 11 rndNr 0xffffffffffffff4f
    addRndConstant x 12 rndNr 0xffffffffffffff3f
    addRndConstant x 13 rndNr 0xffffffffffffff2f
    addRndConstant x 14 rndNr 0xffffffffffffff1f
    addRndConstant x 15 rndNr 0xffffffffffffff0f
    
    y <- MV.unsafeNew 16
    
    extractColumn 15 x y  0  2  4 10 15  1  3  5
    extractColumn 14 x y 15  1  3  9 14  0  2  4
    extractColumn 13 x y 14  0  2  8 13 15  1  3
    extractColumn 12 x y 13 15  1  7 12 14  0  2
    extractColumn 11 x y 12 14  0  6 11 13 15  1
    extractColumn 10 x y 11 13 15  5 10 12 14  0
    extractColumn  9 x y 10 12 14  4  9 11 13 15
    extractColumn  8 x y  9 11 13  3  8 10 12 14
    extractColumn  7 x y  8 10 12  2  7  9 11 13
    extractColumn  6 x y  7  9 11  1  6  8 10 12
    extractColumn  5 x y  6  8 10  0  5  7  9 11
    extractColumn  4 x y  5  7  9 15  4  6  8 10
    extractColumn  3 x y  4  6  8 14  3  5  7  9
    extractColumn  2 x y  3  5  7 13  2  4  6  8
    extractColumn  1 x y  2  4  6 12  1  3  5  7
    extractColumn  0 x y  1  3  5 11  0  2  4  6

    return y

{-# INLINE addRndConstant #-}
addRndConstant :: MV.STVector s Word64 -> Int -> Word64 -> Word64 -> ST s () 
addRndConstant x i rndNr c = do
    xi <- MV.unsafeRead x i 
    MV.unsafeWrite x i (xi `xor` c `xor` rndNr)

--{-# INLINE extractColumn #-}
extractColumn :: Int 
        -> MV.STVector s Word64
        -> MV.STVector s Word64
        -> Int -> Int -> Int -> Int
        -> Int -> Int -> Int -> Int 
        -> ST s ()
extractColumn i x y c0 c1 c2 c3 c4 c5 c6 c7 = do
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
tableLookup x i c = do
    w <- MV.unsafeRead x c
    return . V.unsafeIndex tables $ i * 256 + fromIntegral (w # i)
    where -- Extract byte from Word64
          (#) :: Word64 -> Int -> Word8    
          w # n = fromIntegral $ shiftR w (8 * (7 - n))

outputTransform :: BlockLength -> V.Vector Word64 -> V.Vector Word64
outputTransform blockLen x = V.zipWith xor (permP' x) x
    where permP' y = V.create (V.thaw y >>= permPM blockLen)

---------------------------- Parsing, padding and truncating ------------------------------

parseMessage :: Int64 -> Int64 -> L.ByteString -> [V.Vector Word64]
parseMessage dataLen blockLen xs
   | L.null suf   = pad dataLen blockLen pre
   | otherwise    = parseBlock pre : parseMessage dataLen blockLen suf
   where (!pre,suf) = L.splitAt byteBlockLen xs
         byteBlockLen = blockLen `div` 8


{-# INLINE parseBlock #-}
parseBlock :: L.ByteString  -> V.Vector Word64
parseBlock = V.unfoldr (\bs -> if L.null bs then Nothing else Just (G.runGet G.getWord64be bs, L.drop 8 bs))

-- This function is a mess. Needs to be cleaned up!
pad :: Int64 -> BlockLength -> L.ByteString -> [V.Vector Word64]
pad dataLen blockLen xs   
    | dataLen == 0 || L.null xs               = [v1] 
    | dataLen `rem` blockLen == 0             = [parseBlock xs, v1]
    | dataLen `rem` blockLen <= blockLen - 65 = [v2]
    | otherwise                               = [v3, v4]
    where 
          v1 = V.modify (padOne byte bit >=> padBlockNumber blocks) zeroBlock
          v2 = V.modify (padOne byte bit >=> padBlockNumber blocks) fullBlock
          v3 = V.modify (void . padOne byte bit) fullBlock
          v4 = V.modify (padBlockNumber (blocks + 1)) zeroBlock
          byte = (fromIntegral (dataLen `div` 64)) `rem` vectorLen
          bit = fromIntegral (63 - dataLen `rem` 64)
          blockByteLen = blockLen `div` 8
          fullBlock = parseBlock . L.take blockByteLen . L.append xs $ L.repeat 0x00
          blocks = fromIntegral $ dataLen `div` blockLen + 1        
          zeroBlock = V.replicate vectorLen 0x00
          vectorLen = fromIntegral $ blockLen `div` 64 

padBlockNumber :: Word64 -> MV.STVector s Word64 -> ST s ()
padBlockNumber blocks v = MV.write v (MV.length v - 1) blocks

padOne :: Int -> Int -> MV.STVector s Word64 -> ST s (MV.STVector s Word64)
padOne i bit v = (flip setBit bit) `liftM` (MV.read v i) >>= MV.write v i >> return v

truncate :: DigestLength -> V.Vector Word64 -> L.ByteString
truncate G224 = L.drop 4 . L.concat . map B.encode . V.toList . V.unsafeSlice 4 4 
truncate G256 = L.concat . map B.encode . V.toList . V.unsafeSlice 4 4 
truncate G384 = L.concat . map B.encode . V.toList . V.unsafeSlice 10 6 
truncate G512 = L.concat . map B.encode . V.toList . V.unsafeSlice 8 8

--------------------------------- Iterative hashing --------------------

data GroestlCtx = Ctx {
            dataParsed :: !Int64,
            digestLength :: DigestLength,
            blockLength :: BlockLength,
            hashState :: V.Vector Word64
        }

groestlInit :: DigestLength -> BlockLength -> V.Vector Word64 -> GroestlCtx
groestlInit dLen bLen h0 = Ctx {dataParsed = 0, 
                                digestLength = dLen, 
                                blockLength = bLen, 
                                hashState = h0}

groestlUpdate :: GroestlCtx -> BS.ByteString -> GroestlCtx
groestlUpdate ctx bs
   | BS.null bs = ctx
   | otherwise  = result
   where 
       (!newState, result) = foldUpdate . BS.splitAt blockByteLen $ bs
       foldUpdate = hashBlock *** groestlUpdate newCtx
       hashBlock bs = runST $ fM blockLen (hashState ctx) $ parseBlock' bs
       newCtx = Ctx (dataParsed ctx + blockLen) (digestLength ctx) blockLen newState
       blockLen = blockLength ctx
       blockByteLen = fromIntegral $ blockLen `div` 8


{-# INLINE parseBlock' #-}
parseBlock' :: BS.ByteString  -> V.Vector Word64
parseBlock' = V.unfoldr p
    where p bs = case S.runGet S.getWord64be bs of
                      Left _  -> Nothing
                      Right w -> Just (w, BS.drop 8 bs)       

groestlFinalize :: GroestlCtx -> BS.ByteString -> L.ByteString
groestlFinalize ctx bs = runST $ liftM (truncate digestLen . outputTransform blockLen) $ padLast bs
    where padLast = foldM (fM blockLen) prevState . pad dataLen blockLen . L.pack . BS.unpack
          dataLen = dataParsed ctx + fromIntegral (BS.length bs * 8)
          prevState = hashState ctx
          digestLen = digestLength ctx
          blockLen = blockLength ctx    


------------------------------------ Some convenience functions -----------------------

printWAsHex :: Word64 -> String
printWAsHex = printf "0x%016x"

printAsHex :: L.ByteString -> String
printAsHex = concat . ("0x" :) . map (printf "%02x") . L.unpack


