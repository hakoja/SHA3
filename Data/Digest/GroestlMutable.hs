{-# LANGUAGE BangPatterns, Rank2Types #-}

module Data.Digest.GroestlMutable (
           
           f512Par,
           
           f512M,
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

import Data.Word (Word64)
import Data.Int (Int64) 	
import Data.Bits (xor, shiftR, setBit)
import Data.List (foldl')
import qualified Data.Binary as B
import qualified Data.Binary.Get as G
import qualified Data.Serialize as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad (liftM, foldM)
import Control.Monad.ST (ST, runST)
import Control.Arrow ((***))
import Prelude hiding (truncate)
import Text.Printf (printf)
import Control.Parallel.Strategies (runEval, rpar, rseq)


import Data.Digest.GroestlTables


-------------------------------------- Data types used in the implementation ----------------------

data DigestLength = G224 | G256 | G384 | G512
    deriving (Eq, Ord)

---------------------------------- A port of the optimized 64-bit C version -----------------------


-- This part is only used for experimenting with 
-- parallel execution of the P and Q functions.
-- f512M is the stable (serial) version. 
f512Par :: V.Vector Word64 -> V.Vector Word64 -> V.Vector Word64
f512Par h m = runEval $ do
             outP <- rpar (runP inP)
             outQ <- rseq (runQ m)
             return (V.zipWith3 xor3 h outP outQ)
    where xor3 x1 x2 x3 = x1 `xor` x2 `xor` x3
          inP = V.zipWith xor h m

{-# INLINE runP #-}
runP :: V.Vector Word64 -> V.Vector Word64
runP x = runST $ V.unsafeThaw x >>= permPM >>= V.unsafeFreeze

{-# INLINE runQ #-}
runQ :: V.Vector Word64 -> V.Vector Word64
runQ x = runST $ V.unsafeThaw x >>= permQM >>= V.unsafeFreeze


{-# INLINE f512M #-}
f512M :: V.Vector Word64 -> V.Vector Word64 -> ST s (V.Vector Word64)
f512M h m = do
    outP <- V.unsafeFreeze =<< permPM =<< V.unsafeThaw inP
    outQ <- V.unsafeFreeze =<< permQM =<< V.unsafeThaw m
    return $ V.zipWith3 xor3 h outQ outP    
    where xor3 x1 x2 x3 = x1 `xor` x2 `xor` x3
          inP = V.zipWith xor h m

{-# INLINE permPM #-}
permPM :: MV.STVector s Word64 -> ST s (MV.STVector s Word64)
permPM x = V.foldM' rnd512PM x (V.enumFromStepN 0 0x0100000000000000 10)

-- !!! Inlining this function leads to 4 times the run-time. 
-- See also: rnd512QM
--{-# INLINE permQM #-}
permQM :: MV.STVector s Word64 -> ST s (MV.STVector s Word64)
permQM x = V.foldM' rnd512QM  x (V.enumFromN 0 10)

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
    t <- MV.unsafeRead x c
    return $ V.unsafeIndex tables (i * 256 + extractByte i t)
    where extractByte :: Int -> Word64 -> Int   
          {-# INLINE extractByte #-}
          extractByte n w = fromIntegral $ shiftR w (8 * (7 - n)) `mod` 256

outputTransform :: V.Vector Word64 -> V.Vector Word64
outputTransform !x = V.zipWith xor (permP' x) x
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


{-# INLINE parseBlock #-}
parseBlock :: L.ByteString  -> V.Vector Word64
parseBlock = V.unfoldr (\bs -> if L.null bs then Nothing else Just (G.runGet G.getWord64be bs, L.drop 8 bs))

-- This function is a mess. Needs to be cleaned up!
pad :: Int64 -> Int64 -> L.ByteString -> [V.Vector Word64]
pad dataLen blockLen xs
    | dataLen == 0 || L.null xs = [V.fromList [0x8000000000000000, 0,0,0,0,0,0, lengthPad + 1]] 
    | partialBlockLen == 0 = [parseBlock xs, V.fromList [0x8000000000000000, 0,0,0,0,0,0, lengthPad + 1]]  
    | L.length onePadded <= (blockByteLen - 8) = [V.update (parseBlock fullBlock) (V.singleton (7, lengthPad + 1))]
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
    | byteOffset == 0       =  L.snoc xs 0x80
    | otherwise             = L.snoc (L.init xs) (setBit (L.last xs) (7 - byteOffset))
    where byteOffset = fromIntegral $ len `mod` 8


truncate :: DigestLength -> V.Vector Word64 -> L.ByteString
truncate G224 = L.drop 4 . L.concat . map B.encode . V.toList . V.unsafeSlice 4 4 
truncate G256 = L.concat . map B.encode . V.toList . V.unsafeSlice 4 4 
truncate G384 = L.concat . map B.encode . V.toList . V.unsafeSlice 2 6 
truncate G512 = L.concat . map B.encode . V.toList 

--------------------------------- Iterative hashing --------------------

data GroestlCtx = Ctx {
            dataParsed :: !Int64,
            digestLength :: DigestLength,
            hashState :: V.Vector Word64
        }

groestlInit :: DigestLength -> V.Vector Word64 -> GroestlCtx
groestlInit dLen h0 = Ctx {dataParsed = 0, digestLength = dLen, hashState = h0}

groestlUpdate :: GroestlCtx -> BS.ByteString -> GroestlCtx
groestlUpdate ctx bs
   | BS.null bs = ctx
   | otherwise  = result
   where 
       (!newState, result) = foldUpdate . BS.splitAt 64 $ bs
       foldUpdate          = hashBlock *** groestlUpdate newCtx
       hashBlock bs        = runST $ f512M (hashState ctx) $ parseBlock' bs
       newCtx              = Ctx (dataParsed ctx + 512) (digestLength ctx) newState

{-# INLINE parseBlock' #-}
parseBlock' :: BS.ByteString  -> V.Vector Word64
parseBlock' = V.unfoldr p
    where p bs = case S.runGet S.getWord64be bs of
                      Left _  -> Nothing
                      Right w -> Just (w, BS.drop 8 bs)       

groestlFinalize :: GroestlCtx -> BS.ByteString -> L.ByteString
groestlFinalize ctx bs = runST $ liftM (truncate dLen . outputTransform) . foldM f512M prevState $ pad n 512 bs'
    where bs'       = L.pack $ BS.unpack bs
          n         = dataParsed ctx + fromIntegral (BS.length bs * 8)
          prevState = hashState ctx
          dLen      = digestLength ctx


------------------------------------ Some convenience functions -----------------------

printWAsHex :: Word64 -> String
printWAsHex = printf "0x%016x"

printAsHex :: L.ByteString -> String
printAsHex = concat . ("0x" :) . map (printf "%02x") . L.unpack


