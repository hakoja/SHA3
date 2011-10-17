{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Data.BigWord.Word128 (
			Word128(..),
			w128toInteger
		) where

import Data.Bits
import Data.Word (Word64)	
import qualified Data.Binary as B
import qualified Data.Serialize as S
import Control.Monad (liftM2)
		


-- an unsigned 128 bit word. 
-- given x = W a b; then a represents the 64 MSB, and b the 64 LSB
data Word128 = W !Word64 !Word64
	deriving (Show, Eq, Ord) 

w128toInteger :: Word128 -> Integer
w128toInteger (W h l) = shiftL (toInteger h) 64 + (toInteger l)

---------------------- Num instance -------------------------

instance Num Word128 where
   (+)      = word128Plus
   (*)      = word128Times
   abs x    = x
   signum 0 = 0
   signum _ = 1
   fromInteger x = W (fromInteger $ shiftR x 64) (fromInteger x)
   
word128Plus :: Word128 -> Word128 -> Word128
(W xh xl) `word128Plus` (W yh yl) =
   let xl' = fromIntegral xl :: Integer
       yl' = fromIntegral yl :: Integer
       xh' = shiftL (fromIntegral xh :: Integer) 64
       yh' = shiftL (fromIntegral yh :: Integer) 64
       sum = (xl' + xh') + (yl' + yh')
   in W (fromIntegral $ shiftR sum 64) (fromIntegral sum)

word128Times :: Word128 -> Word128 -> Word128
(W xh xl) `word128Times` (W yh yl) = 
   let xl' = fromIntegral xl :: Integer
       yl' = fromIntegral yl :: Integer
       xh' = shiftL (fromIntegral xh :: Integer) 64
       yh' = shiftL (fromIntegral yh :: Integer) 64
       product = (xl' + xh') * (yl' + yh')
   in W (fromIntegral $ shiftR product 64) (fromIntegral product)

-------------------------- Bits instance --------------------

instance Bits Word128 where
   (W xh xl) .&. (W yh yl)     = W (xh .&. yh) (xl .&. yl)
   (W xh xl) .|. (W yh yl)      = W (xh .|. yh) (xl .|. yl)
   (W xh xl) `xor` (W yh yl)    = W (xh `xor` yh) (xl `xor` yl)
   complement (W xh xl)      = W (complement xh) (complement xl)
   shift                   = word128Shift
   rotate                  = word128Rotate
   bitSize _               = 128
   isSigned _              = False
	   
word128Shift :: Word128 -> Int -> Word128
{-# INLINE word128Shift #-}
word128Shift (W xh xl) n 
   | n >= 0       = W ((shiftL xh n) .|. (shiftR xl (64 - n))) (shiftL xl n)
   | otherwise    = W (shiftR xh (-n)) ((shiftR xl (-n)) .|. (shiftL xh (64 + n)))

word128Rotate :: Word128 -> Int -> Word128
word128Rotate x n
   | n >= 0    = (shiftL x n) .|. (shiftR x (128 - n))
   | otherwise = (shiftR x (-n)) .|. (shiftL x (128 + n))

---------------------- Binary instance ---------------------

instance B.Binary Word128 where
   put (W h l) = B.put h >> B.put l
   get         = liftM2 W B.get B.get
   
--------------------- Serialize instance -------------------

instance S.Serialize Word128 where
   put (W h l) = S.put h >> S.put l
   get         = liftM2 W S.get S.get