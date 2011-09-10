module Data.Digest.JH 
   (
      jh
   ) where 

import qualified Data.ByteString.Lazy as L
import Data.Array.ST
import Data.Array.Unboxed
import Data.Word (Word8, Word64)
import Data.Bits
import Data.Binary


class Hashable a where 
   hash :: a -> L.ByteString

type Hash = STUArray Word8 Int Word8

roundConstants0 :: UArray Int Word8   
roundConstants0 = listArray (0,63) 
   [0x6,0xa,0x0,0x9,0xe,0x6,0x6,0x7,0xf,0x3,0xb,0xc,0xc,0x9,0x0,0x8,0xb,0x2,
    0xf,0xb,0x1,0x3,0x6,0x6,0xe,0xa,0x9,0x5,0x7,0xd,0x3,0xe,0x3,0xa,0xd,0xe,
    0xc,0x1,0x7,0x5,0x1,0x2,0x7,0x7,0x5,0x0,0x9,0x9,0xd,0xa,0x2,0xf,0x5,0x9,
    0x0,0xb,0x0,0x6,0x6,0x7,0x3,0x2,0x2,0xa]

dim = 4


------------------------------------ The S-box step ---------------------------------

elems sbox


sbox :: UArray (Int,Int) Word8
sbox = listArray ((0,0),(1,15)) $
   [9,0,4,11,13,12,3,15,1,10,2,6,7,5,8,14] ++ [3,12,6,13,5,7,1,9,15,2,0,4,11,10,14,8]


--------------------------- The linear transformation step L -----------------------


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



-------------------------------- The permutation step P8 ----------------------------

p_8 :: L.ByteString -> L.ByteString 
p_8 = L.pack . phi_8 . p'_8 . pi_8 . L.unpack

--in every other byte, permute the high and low nibbles
pi_8 :: [Word8] -> [Word8]
pi_8 = zipWith ($) (cycle [id,switchNibbles])

--all the low nibbles (4 lsb) becomes the |xs| / 2 first bytes
--all the high nibbles (4 msb) becomes the |xs| / 2 last bytes  
p'_8 :: [Word8] -> [Word8]
p'_8 xs = evenNibblesPacked xs ++ oddNibblesPacked xs
   where evenNibblesPacked = take (dim `div` 2) . consumePairs (.|.) . evenNibbles
         oddNibblesPacked = take 4 . consumePairs (.|.) . oddNibbles
         evenNibbles = zipWith ($) (cycle [(.&.) 0x0f, flip shiftL 4 . (.&.) 0x0f])
         oddNibbles = zipWith ($) (cycle [flip shiftR 4 . (.&.) 0xf0, (.&.) 0xf0])

--permute the high and low nibbles in the last |xs| / 2 bytes
phi_8 :: [Word8] -> [Word8]
phi_8 xs = let (lbytes, hbytes) = splitAt (dim `div` 2) xs  
           in lbytes ++ map switchNibbles hbytes 

--permutes the low order nibbel (4 lsb) with the high order nibble (4 msb)
switchNibbles :: Word8 -> Word8
switchNibbles a = let bl = shiftR (a .&. 0xf0) 4
                      bh = shiftL (a .&. 0x0f) 4
                  in bh .|. bl

-----------------------------------------------------------------------------------------

data HashState = HashState { 
         hashBitLen :: Int,
         dataBitLen :: Word64,
         dataSizeInBuffer :: Word64,
         hashArray :: Hash
      }


jh :: Hashable a => a -> L.ByteString
jh = undefined

-- sort of like map, but halves the list in size by consuming two elements. 
consumePairs :: (a -> a -> b) -> [a] -> [b]
consumePairs f (x : x' : xs) = f x x' : consumePairs f xs
consumePairs _ [] = []
consumePairs _ _ = error "list must contain even number of elements"


