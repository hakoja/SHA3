module Data.Digest.JH 
   (
      jh
   ) where 

import qualified Data.ByteString.Lazy as L
import Data.Array.ST
import Data.Array.Unboxed
import Data.Word (Word8, Word64)
import Data.Bits

class Hashable a where 
   hash :: a -> L.ByteString

sbox :: UArray (Int,Int) Word8
sbox = listArray ((1,1),(2,16)) $
   [9,0,4,11,13,12,3,15,1,10,2,6,7,5,8,14] ++ [3,12,6,13,5,7,1,9,15,2,0,4,11,10,14,8]

roundConstants0 :: UArray Int Word8   
roundConstants0 = listArray (1,64) 
   [0x6,0xa,0x0,0x9,0xe,0x6,0x6,0x7,0xf,0x3,0xb,0xc,0xc,0x9,0x0,0x8,0xb,0x2,
    0xf,0xb,0x1,0x3,0x6,0x6,0xe,0xa,0x9,0x5,0x7,0xd,0x3,0xe,0x3,0xa,0xd,0xe,
    0xc,0x1,0x7,0x5,0x1,0x2,0x7,0x7,0x5,0x0,0x9,0x9,0xd,0xa,0x2,0xf,0x5,0x9,
    0x0,0xb,0x0,0x6,0x6,0x7,0x3,0x2,0x2,0xa]

linearTransform :: (Word8, Word8) -> (Word8, Word8)
linearTransform (a,b) = 
   let d0 = (b .&. 1) `xor` (a .&. 2)
       d1 = (b .&. 2) `xor` (a .&. 3)
       d2 = (b .&. 3) `xor` (a .&. 4) `xor` (a .&. 1)
       d3 = (b .&. 4) `xor` (a .&. 1)
       c0 = (a .&. 1) `xor` d1
       c1 = (a .&. 2) `xor` d2
       c2 = (a .&. 3) `xor` d3 `xor` d0
       c3 = (a .&. 4) `xor` d0
   in (foldl1 (.|.) [c0,c1,c2,c3], foldl1 (.|.) [d0,d1,d2,d3])

data HashState = HashState { 
         hashBitLen :: Int,
         dataBitLen :: Word64,
         dataSizeInBuffer :: Word64,
         hashArray :: STUArray Word8 Int Word8
      }


jh :: Hashable a => a -> L.ByteString
jh = undefined



