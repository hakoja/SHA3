import Data.LargeWord (LargeKey(..), Word128, loHalf, hiHalf)
import Data.Bits
import Data.Array.Unboxed
import Data.Array.ST
import Data.Word (Word8)
import Data.Int (Int64)

type Block512 = (Word128, Word128, Word128, Word128)
type Block1024 = (Block512, Block512)

data HashState = HashState {
      hashBitLen :: Int,
      dataSizeInBuffer :: Int64,
      state :: STUArray Word128 Int Word128,
      buffer :: UArray Int Word8,
      roundNr :: Int
   } 

sbox :: Block512 -> Word128 -> Block512
sbox (a0,a1,a2,a3) c = 
   let b3 = negate a3                           --1
       b0   = a0 `xor` (c .&. (complement a2))      --2
       t    = c   `xor` (b0 .&. a1)             --3
       b0'  = b0  `xor` (a2 .&. b3)             --4
       b3'  = b3  `xor` (complement a1 .&. a2)  --5
       b1   = a1  `xor` (b0' .&. a2)            --6
       b2   = a2  `xor` (b0' .&. complement b3')--7
       b0'' = b0' `xor` (b1 .|. b3')            --8
       b3'' = b3' `xor` (b1 .&. b2)             --9
       b1'  = b1  `xor` (t .&. b0'')            --10
       b2'  = b2  `xor`t                        --11
   in (b0'',b1',b2',b3'')


linearTransform :: Block1024 -> Block1024
linearTransform ((a0,a1,a2,a3), (a4,a5,a6,a7)) =
   let b4 = a4 `xor` a1
       b5 = a5 `xor` a2
       b6 = a6 `xor` a3 `xor` a0
       b7 = a7 `xor` a0
       b0 = a0 `xor` b5
       b1 = a1 `xor` b6
       b2 = a2 `xor` b7 `xor` b4
       b3 = a3 `xor` b4
   in ((b0,b1,b2,b3),(b4,b5,b6,b7))

swap :: Int -> Word128 -> Word128
swap 0 = swap1
swap 1 = swap2
swap 2 = swap4
swap 3 = swap8
swap 4 = swap16
swap 5 = swap32
swap 6 = swap64
swap _ = error "Not a number in:  r `mod` 7"

swap1,swap2,swap4,swap8,swap16,swap32,swap64 :: Word128 -> Word128

swap1 x = shiftL (x .&. 0x55555555555555555555555555555555) 1 
          .|. 
          shiftR (x .&. 0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa) 1 

swap2 x = shiftL (x .&. 0x33333333333333333333333333333333) 2 
          .|. 
          shiftR (x .&. 0xcccccccccccccccccccccccccccccccc) 2           

swap4 x = shiftL (x .&. 0x0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f) 4 
          .|. 
          shiftR (x .&. 0xf0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0) 4           

swap8 x = shiftL (x .&. 0x00ff00ff00ff00ff00ff00ff00ff00ff) 8 
          .|. 
          shiftR (x .&. 0xff00ff00ff00ff00ff00ff00ff00ff00) 8            

swap16 x = shiftL (x .&. 0x0000ffff0000ffff0000ffff0000ffff) 16 
           .|. 
           shiftR (x .&. 0xffff0000ffff0000ffff0000ffff0000) 16           

swap32 x = shiftL (x .&. 0x00000000ffffffff00000000ffffffff) 32 
           .|. 
           shiftR (x .&. 0xffffffff00000000ffffffff00000000) 32 

swap64 x = LargeKey (hiHalf x) (loHalf x)           

roundFunction :: Block1024 -> Int -> Block1024
roundFunction ((a0,a1,a2,a3),(a4,a5,a6,a7)) roundNr = 
   let r = roundNr `mod` 7
       evens = sbox (a0, a2, a4, a6) 0xff
       odds  = sbox (a1, a3, a5, a7) 0xff
       ((b0,b2,b4,b6),(u1,u3,u5,u7)) = linearTransform (evens,odds)
       (b1,b3,b5,b7) = (swap r u1, swap r u3, swap r u5, swap r u7)
   in ((b0,b1,b2,b3),(b4,b5,b6,b7))
                       