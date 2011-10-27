{-# LANGUAGE BangPatterns, TypeFamilies, MultiParamTypeClasses #-}
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
import Control.Monad (liftM, liftM2)
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as V        


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
(W xh xl) `word128Plus` (W yh yl) = W hi lo
    where lo = xl + yl
          hi = xh + yh + if lo < xh then 1 else 0

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
   (W xh xl) .&. (W yh yl)   = W (xh .&. yh) (xl .&. yl)
   (W xh xl) .|. (W yh yl)   = W (xh .|. yh) (xl .|. yl)
   (W xh xl) `xor` (W yh yl) = W (xh `xor` yh) (xl `xor` yl)
   complement (W xh xl)      = W (complement xh) (complement xl)
   shift                     = word128Shift
   rotate                    = word128Rotate
   bitSize _                 = 128
   isSigned _                = False
       
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


------------------------ UnboxedVector instance ------------

newtype instance V.MVector s Word128 = MV_Word128 (V.MVector s (Word64, Word64))
newtype instance V.Vector Word128 = V_Word128 (V.Vector (Word64,Word64))


instance GM.MVector V.MVector Word128 where
    basicLength (MV_Word128 v) = GM.basicLength v
    basicUnsafeSlice i n (MV_Word128 v) = MV_Word128 $ GM.basicUnsafeSlice i n v
    basicOverlaps (MV_Word128 v1) (MV_Word128 v2) = GM.basicOverlaps v1 v2
    basicUnsafeNew n = MV_Word128 `liftM` GM.basicUnsafeNew n
    basicUnsafeRead (MV_Word128 v) i = uncurry W `liftM` GM.basicUnsafeRead v i
    basicUnsafeReplicate n (W x y) = MV_Word128 `liftM` GM.basicUnsafeReplicate n (x,y)
    basicUnsafeWrite (MV_Word128 v) i (W x y) = GM.basicUnsafeWrite v i (x,y)
    basicClear (MV_Word128 v) = GM.basicClear v
    basicSet (MV_Word128 v) (W x y) = GM.basicSet v (x,y)
    basicUnsafeCopy (MV_Word128 v1) (MV_Word128 v2) = GM.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_Word128 v1) (MV_Word128 v2) = GM.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_Word128 v) n = MV_Word128 `liftM` GM.basicUnsafeGrow v n   

instance G.Vector V.Vector Word128 where
    basicUnsafeFreeze (MV_Word128 v) = V_Word128 `liftM` G.basicUnsafeFreeze v
    basicUnsafeThaw (V_Word128 v) = MV_Word128 `liftM` G.basicUnsafeThaw v
    basicLength (V_Word128 v) = G.basicLength v
    basicUnsafeSlice i n (V_Word128 v) = V_Word128 $ G.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_Word128 v) i = uncurry W `liftM` G.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_Word128 mv) (V_Word128 v) = G.basicUnsafeCopy mv v
    elemseq _ (W x y) z = G.elemseq (undefined :: V.Vector Word64) x 
                        $ G.elemseq (undefined :: V.Vector Word64) y z
    
    
instance V.Unbox Word128

