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

sbox :: UArray (Int,Int) Word8
sbox = listArray ((0,0),(1,15)) $
   [9,0,4,11,13,12,3,15,1,10,2,6,7,5,8,14] ++ [3,12,6,13,5,7,1,9,15,2,0,4,11,10,14,8]

roundConstants0 :: UArray Int Word8   
roundConstants0 = listArray (0,63) 
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

--permute bit 2 with bit 3, and bit 6 with bit 7, leave all other
pi_8 :: [Word8] -> [Word8]
pi_8 = map f
   where f a = let b26 = shiftL (a .&. 0x44) 1 
                   b37 = shiftR (a .&. 0x88) 1
               in (a .&. 0x33) .|. b26 .|. b37 

--all the even numbered bits becomes the |xs| / 2 first bits
--all the odd numbered bits becomes the |xs| / 2 last bits  
p'_8 :: [Word8] -> [Word8]
p'_8 = packWords

--in every other byte, permute bit 0 with bit 1, bit 2 with bit 3 etc...
phi_8 :: [Word8] -> [Word8]
phi_8 = zipWith ($) (cycle [id,f])
   where f a = let b1 = shiftL (a .&. 0x55) 1
                   b2 = shiftR (a .&. 0xaa) 1
               in b1 .|. b2

p_8 :: L.ByteString -> L.ByteString 
p_8 = L.pack . phi_8 . p'_8 . pi_8 . L.unpack

data HashState = HashState { 
         hashBitLen :: Int,
         dataBitLen :: Word64,
         dataSizeInBuffer :: Word64,
         hashArray :: Hash
      }


jh :: Hashable a => a -> L.ByteString
jh = undefined

---------------------------------Solution provided by user Boris on StackOverflow -------------

-- the main attraction
packString :: L.ByteString -> L.ByteString
packString = L.pack . packWords . L.unpack

-- main attraction equivalent, in [Word8]
packWords :: [Word8] -> [Word8]
packWords ws = evenPacked ++ unevenPacked
    where evenPacked = consumePairs packNibbles evenBits
          unevenPacked = consumePairs packNibbles unevenBits 
          evenBits = map packEven ws
          unevenBits = map packUneven ws

-- combines 2 low nibbles into a (low nibble, high nibble) word
-- assumes that only the low nibble (first 4 bits) of both arguments can be non-zero. 
packNibbles :: Word8 -> Word8 -> Word8
packNibbles w1 w2 = w1 .|. (shiftL w2 4)

packEven w = packBits w [0, 2, 4, 6]
packUneven w = packBits w [1, 3, 5, 7]

-- packBits 254 [0, 2, 4, 6] = 14 
-- packBits 254 [1, 3, 5, 7] = 15
packBits :: Word8 -> [Int] -> Word8
packBits w = foldr1 (.|.) . map (packBit w)

-- packBit 255 0 = 1
-- packBit 255 1 = 1
-- packBit 255 2 = 2
-- packBit 255 3 = 2
-- packBit 255 4 = 4
-- packBit 255 5 = 4
-- packBit 255 6 = 8
-- packBit 255 7 = 8
packBit :: Word8 -> Int -> Word8
packBit w i = shiftR (w .&. bit i) ((i `div` 2) + (i `mod` 2))

-- sort of like map, but halves the list in size by consuming two elements. 
-- Is there a clearer way to write this with built-in function?
consumePairs :: (a -> a -> b) -> [a] -> [b]
consumePairs f (x : x' : xs) = f x x' : consumePairs f xs
consumePairs _ [] = []
consumePairs _ _ = error "list must contain even number of elements"


