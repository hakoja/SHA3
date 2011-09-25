{-# LANGUAGE TypeSynonymInstances, BangPatterns #-}

module Data.Digest.JH (
         jh,
      ) where

import Data.Bits
import Data.Word (Word8, Word64)
import Data.Int (Int64)
import Data.List (foldl')
import Data.Array 
import Data.Binary
import qualified Data.ByteString.Lazy as L 

import Text.Printf (printf)


type Block512 = (Word128, Word128, Word128, Word128)
type Block1024 = (Block512, Block512)



jh = undefined

-------------------- Conversion between Bytestrings Word128 -------------

bsToWord128 :: L.ByteString -> Word128
bsToWord128 xs = let (wh,rest) = L.splitAt 8 xs
					  in (decode (L.take 8 rest), decode wh) 




--------------------- testing ------------------

print1024 :: Block1024 -> [String]
print1024 (u,v) = print512 u ++ print512 v

print512 :: Block512 -> [String]
print512 xs = let (a,b,c,d) = tupleMap (printf "0x%032x" . w128toInteger) xs
				  in [a,b,c,d]

print128 :: Word128 -> String
print128 = printf "0x%032x" . w128toInteger

evenWords :: Block1024 -> Block512
evenWords ((a0,a1,a2,a3),(a4,a5,a6,a7)) = (a0,a2,a4,a6)

m0 = ((0,0xaa80000000000000),0,0,0)
m1 = (0,0,0,0x8)

testRun = print1024 $ f8 (f8 jh224_H0 m0) m1 

testRoundFunction = print1024 . roundFunction ((0,0,0,0),(0,0,0,0))

testE8 = print1024 $ e8 ((0,0,0,0),(0,0,0,0))

testF8 = print1024 . f8 jh224_H0

s = ((0x8cd388eeb2c2911bc6cf0e9d1154ec00, 0x0acd42fab302532a993388977f447367,
     	0xb89b84b3f57b549f7c6bdf91a6098003, 0x433b6f6a1b341d64e6f9b8f1e5fb7727),
     (0xba4e51200cf7eb274e42d2e6bfaeae60, 0xb2fae4c12dedf3e269b41778469f506a,
      0x43492014ff4532921b2b279f5548a521, 0xb91ea5ff24c22cf3babc4b3fffe1594c))

finalize (_,(x1,x2,x3,x4)) 224 = 
	printf "0x%056x\n" $ shiftL ((w128toInteger (shiftR x3 32))) 128 + (w128toInteger x4) 


------------------------------------------------


data Parity = Even | Odd
   deriving (Eq, Ord, Read, Show, Ix)

sbox :: Block512 -> Word128 -> Block512
sbox (a0,a1,a2,a3) c = 
   let b3   = complement a3                        --1
       b0   = a0 	`xor` (c .&. (complement a2))    --2
       t    = c   `xor` (b0 .&. a1)             	--3
       b0'  = b0  `xor` (a2 .&. b3)             	--4
       b3'  = b3  `xor` ((complement a1) .&. a2)  	--5
       b1   = a1  `xor` (b0' .&. a2)            	--6
       b2   = a2  `xor` (b0' .&. (complement b3'))	--7
       b0'' = b0' `xor` (b1 .|. b3')            	--8
       b3'' = b3' `xor` (b1 .&. b2)             	--9
       b1'  = b1  `xor` (t .&. b0'')           	 	--10
       b2'  = b2  `xor`t                        	--11
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

swap64 (lo, hi) = (hi, lo)

roundFunction :: Block1024 -> Int -> Block1024
roundFunction ((a0,a1,a2,a3),(a4,a5,a6,a7)) roundNr = 
   let r = roundNr `mod` 7
       evens = sbox (a0, a2, a4, a6) (constants ! (roundNr, Even))
       odds  = sbox (a1, a3, a5, a7) (constants ! (roundNr, Odd))
       ((b0,b2,b4,b6),oddsTransformed) = linearTransform (evens,odds)
       (b1,b3,b5,b7) = tupleMap (swap r) oddsTransformed
   in ((b0,b1,b2,b3),(b4,b5,b6,b7))
 
e8 :: Block1024 -> Block1024
e8 hs = foldl' roundFunction hs [0..41] 

f8 :: Block1024 -> Block512 -> Block1024
f8 (lo, hi) m = let al =  tupleZip xor lo m
                    (!bl, !bh) = e8 (al, hi)
                in (bl, tupleZip xor bh m)

---------------------- Utility functions -----------------

tupleZip :: (a -> b -> c) -> (a, a, a, a) -> (b, b, b, b) -> (c, c, c, c)
tupleZip f (a1,a2,a3,a4) (b1,b2,b3,b4) = (f a1 b1, f a2 b2, f a3 b3, f a4 b4) 

tupleMap :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
tupleMap f (a0,a1,a2,a3) = (f a0, f a1, f a2, f a3)

-------------- Constants -------------------

-- Initial hash values
jh224_H0 =
{- 
	((0xe734d619d6ac7caeac989af962ddfe2d,0x941466c9c63860b8161230bc051083a4,
	  0xdc1a9b1d1ba39ece6f7080259f89d966,0xc106fa027f8594f9106e367b5f32e811),
		
	 (0x9980736e7fa1f697b340c8d85c1b4f1b,0x689a53c9dee831a4d3a3eaada593dfdc,
	  0xf06ce59c95ac74d5e4a186ec8aa9b422,0x6eea64ddf0dc1196bf2babb5ea0d9615))
-}	
	((0x2dfedd62f99a98acae7cacd619d634e7,0xa4831005bc301216b86038c6c9661494,
	  0x66d9899f2580706fce9ea31b1d9b1adc,0x11e8325f7b366e10f994857f02fa06c1),
	  
	 (0x1b4f1b5cd8c840b397f6a17f6e738099,0xdcdf93a5adeaa3d3a431e8dec9539a68,
	  0x22b4a98aec86a1e4d574ac959ce56cf0,0x15960deab5ab2bbf9611dcf0dd64ea6e))



-- Round constants
constants :: Array (Int,Parity) Word128
constants = array ((0, Even), (41, Odd)) $ zip [(i,p) | i <- [0..41], p <- [Even,Odd]]
     [0x72d5dea2df15f8677b84150ab7231557, 0x81abd6904d5a87f64e9f4fc5c3d12b40,
		0xea983ae05c45fa9c03c5d29966b2999a, 0x660296b4f2bb538ab556141a88dba231,
		0x03a35a5c9a190edb403fb20a87c14410, 0x1c051980849e951d6f33ebad5ee7cddc,
		0x10ba139202bf6b41dc786515f7bb27d0, 0x0a2c813937aa78503f1abfd2410091d3,
		0x422d5a0df6cc7e90dd629f9c92c097ce, 0x185ca70bc72b44acd1df65d663c6fc23,
		0x976e6c039ee0b81a2105457e446ceca8, 0xeef103bb5d8e61fafd9697b294838197,
		0x4a8e8537db03302f2a678d2dfb9f6a95, 0x8afe7381f8b8696c8ac77246c07f4214,
		0xc5f4158fbdc75ec475446fa78f11bb80, 0x52de75b7aee488bc82b8001e98a6a3f4,
		0x8ef48f33a9a36315aa5f5624d5b7f989, 0xb6f1ed207c5ae0fd36cae95a06422c36,
		0xce2935434efe983d533af974739a4ba7, 0xd0f51f596f4e81860e9dad81afd85a9f,
		0xa7050667ee34626a8b0b28be6eb91727, 0x47740726c680103fe0a07e6fc67e487b,
		0x0d550aa54af8a4c091e3e79f978ef19e, 0x8676728150608dd47e9e5a41f3e5b062,
		0xfc9f1fec4054207ae3e41a00cef4c984, 0x4fd794f59dfa95d8552e7e1124c354a5,
		0x5bdf7228bdfe6e2878f57fe20fa5c4b2, 0x05897cefee49d32e447e9385eb28597f,
		0x705f6937b324314a5e8628f11dd6e465, 0xc71b770451b920e774fe43e823d4878a,
		0x7d29e8a3927694f2ddcb7a099b30d9c1, 0x1d1b30fb5bdc1be0da24494ff29c82bf,
		0xa4e7ba31b470bfff0d324405def8bc48, 0x3baefc3253bbd339459fc3c1e0298ba0,
		0xe5c905fdf7ae090f947034124290f134, 0xa271b701e344ed95e93b8e364f2f984a,
		0x88401d63a06cf61547c1444b8752afff, 0x7ebb4af1e20ac6304670b6c5cc6e8ce6,
		0xa4d5a456bd4fca00da9d844bc83e18ae, 0x7357ce453064d1ade8a6ce68145c2567,
		0xa3da8cf2cb0ee11633e906589a94999a, 0x1f60b220c26f847bd1ceac7fa0d18518,
		0x32595ba18ddd19d3509a1cc0aaa5b446, 0x9f3d6367e4046bbaf6ca19ab0b56ee7e,
		0x1fb179eaa9282174e9bdf7353b3651ee, 0x1d57ac5a7550d3763a46c2fea37d7001,
		0xf735c1af98a4d84278edec209e6b6779, 0x41836315ea3adba8fac33b4d32832c83,
		0xa7403b1f1c2747f35940f034b72d769a, 0xe73e4e6cd2214ffdb8fd8d39dc5759ef,
		0x8d9b0c492b49ebda5ba2d74968f3700d, 0x7d3baed07a8d5584f5a5e9f0e4f88e65,
		0xa0b8a2f436103b530ca8079e753eec5a, 0x9168949256e8884f5bb05c55f8babc4c,
		0xe3bb3b99f387947b75daf4d6726b1c5d, 0x64aeac28dc34b36d6c34a550b828db71,
		0xf861e2f2108d512ae3db643359dd75fc, 0x1cacbcf143ce3fa267bbd13c02e843b0,
		0x330a5bca8829a1757f34194db416535c, 0x923b94c30e794d1e797475d7b6eeaf3f,
		0xeaa8d4f7be1a39215cf47e094c232751, 0x26a32453ba323cd244a3174a6da6d5ad,
		0xb51d3ea6aff2c90883593d98916b3c56, 0x4cf87ca17286604d46e23ecc086ec7f6,
		0x2f9833b3b1bc765e2bd666a5efc4e62a, 0x06f4b6e8bec1d43674ee8215bcef2163,
		0xfdc14e0df453c969a77d5ac406585826, 0x7ec1141606e0fa167e90af3d28639d3f,
		0xd2c9f2e3009bd20c5faace30b7d40c30, 0x742a5116f2e032980deb30d8e3cef89a,
		0x4bc59e7bb5f17992ff51e66e048668d3, 0x9b234d57e6966731cce6a6f3170a7505,
		0xb17681d913326cce3c175284f805a262, 0xf42bcbb378471547ff46548223936a48,
		0x38df58074e5e6565f2fc7c89fc86508e, 0x31702e44d00bca86f04009a23078474e,
		0x65a0ee39d1f73883f75ee937e42c3abd, 0x2197b2260113f86fa344edd1ef9fdee7,
		0x8ba0df15762592d93c85f7f612dc42be, 0xd8a7ec7cab27b07e538d7ddaaa3ea8de,
		0xaa25ce93bd0269d85af643fd1a7308f9, 0xc05fefda174a19a5974d66334cfd216a,
		0x35b49831db411570ea1e0fbbedcd549b, 0x9ad063a151974072f6759dbf91476fe2]
 
 -------------------------------- Word128 ---------------------------



type Word128 = (Word64, Word64)

w128toInteger :: Word128 -> Integer
w128toInteger (l,h) = shiftL (toInteger h) 64 + (toInteger l)

---------------------- Num instance -------------------------

instance Num Word128 where
   (+)      = word128Plus
   (*)      = word128Times
   abs x    = x
   signum 0 = 0
   signum _ = 1
   fromInteger x = (fromInteger x, fromInteger $ shiftR x 64)
   
word128Plus :: Word128 -> Word128 -> Word128
(xl,xh) `word128Plus` (yl,yh) =
   let xl' = fromIntegral xl :: Integer
       yl' = fromIntegral yl :: Integer
       xh' = shiftL (fromIntegral xh :: Integer) 64
       yh' = shiftL (fromIntegral yh :: Integer) 64
       sum = (xl' + xh') + (yl' + yh')
   in (fromIntegral sum, fromIntegral $ shiftR sum 64)

word128Times :: Word128 -> Word128 -> Word128
(xl,xh) `word128Times` (yl,yh) = 
   let xl' = fromIntegral xl :: Integer
       yl' = fromIntegral yl :: Integer
       xh' = shiftL (fromIntegral xh :: Integer) 64
       yh' = shiftL (fromIntegral yh :: Integer) 64
       product = (xl' + xh') * (yl' + yh')
   in (fromIntegral product, fromIntegral $ shiftR product 64)

word128Abs :: Word128 -> Word128
word128Abs = id

-------------------------- Bits instance --------------------

instance Bits Word128 where
   (xl,xh) .&. (yl,yh)     = (xl .&. yl, xh .&. yh)
   (xl,xh) .|. (yl,yh)     = (xl .|. yl, xh .|. yh)
   (xl,xh) `xor` (yl,yh)   = (xl `xor` yl, xh `xor` yh)
   complement (xl,xh)      = (complement xl, complement xh)
   shift                   = word128Shift
   rotate                  = word128Rotate
   bitSize _               = 128
   isSigned _              = False
	   
word128Shift :: Word128 -> Int -> Word128
word128Shift (xl, xh) n 
   | n >= 0       = (shiftL xl n, (shiftL xh n) .|. (shiftR xl (64 - n)))
   | otherwise    = ((shiftR xl (-n)) .|. (shiftL xh (64 + n)), shiftR xh (-n))

word128Rotate :: Word128 -> Int -> Word128
word128Rotate x n
   | n >= 0    = (shiftL x n) .|. (shiftR x (128 - n))
   | otherwise = (shiftR x (-n)) .|. (shiftL x (128 + n))

