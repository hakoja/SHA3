{-# LANGUAGE TypeSynonymInstances, BangPatterns #-}

module Data.Digest.JH (
         jh,
      ) where

import Data.Bits
import Data.Word (Word8, Word64)
import Data.Int (Int64)
import Data.List (foldl')
import Data.Array 

import Text.Printf (printf)


type Block512 = (Word128, Word128, Word128, Word128)
type Block1024 = (Block512, Block512)



jh = undefined


--------------------- testing ------------------

print1024 :: Block1024 -> [String]
print1024 (u,v) = print512 u ++ print512 v

print512 :: Block512 -> [String]
print512 xs = let (a,b,c,d) = tupleMap (printf "0x%032x" . toIntegerW128) xs
				  in [a,b,c,d]

evenWords :: Block1024 -> Block512
evenWords ((a0,a1,a2,a3),(a4,a5,a6,a7)) = (a0,a2,a4,a6)

m0 = (0x40,0,0,0)
m1 = (0,0,0,2^127)

testRun = drop 6 . print1024 $ f8 (f8 jh224_H0 m0) m1 

testRoundFunction = print1024 . roundFunction ((0,0,0,0),(0,0,0,0))

testE8 = print1024 $ e8 ((0,0,0,0),(0,0,0,0))

testF8 = print1024 $ f8 ((0,0,0,0),(0,0,0,0)) (0xaf, 0, 0, 0)

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

-------------- Round constants -------------------

jh224_H0 = 
	((0xe734d619d6ac7caeac989af962ddfe2d,0x941466c9c63860b8161230bc51083a4,
	  0xdc1a9b1d1ba39ece6f7080259f89d966,0xc16fa27f8594f9106e367b5f32e811),
	  
	 (0x9980736e7fa1f697b340c8d85c1b4f1b,0x689a53c9dee831a4d3a3eaada593dfdc,
	  0xf06ce59c95ac74d5e4a186ec8aa9b422,0x6eea64ddf0dc1196bf2babb5ead9615)) :: Block1024


constants :: Array (Int,Parity) Word128
constants = array ((0, Even), (41, Odd)) $ zip [(i,p) | i <- [0..41], p <- [Even,Odd]]
     [0x571523b70a15847b67f815dfa2ded572, 0x402bd1c3c54f9f4ef6875a4d90d6ab81,
		0x9a99b26699d2c5039cfa455ce03a98ea, 0x31a2db881a1456b58a53bbf2b4960266,
		0x1044c1870ab23f40db0e199a5c5aa303, 0xdccde75eadeb336f1d959e848019051c,
		0xd027bbf7156578dc416bbf029213ba10, 0xd3910041d2bf1a3f5078aa3739812c0a,
		0xce97c0929c9f62dd907eccf60d5a2d42, 0x23fcc663d665dfd1ac442bc70ba75c18,
		0xa8ec6c447e4505211ab8e09e036c6e97, 0x97818394b29796fdfa618e5dbb03f1ee,
		0x956a9ffb2d8d672a2f3003db37858e4a, 0x14427fc04672c78a6c69b8f88173fe8a,
		0x80bb118fa76f4475c45ec7bd8f15f4c5, 0xf4a3a6981e00b882bc88e4aeb775de52,
		0x89f9b7d524565faa1563a3a9338ff48e, 0x362c42065ae9ca36fde05a7c20edf1b6,
		0xa74b9a7374f93a533d98fe4e433529ce, 0x9f5ad8af81ad9d0e86814e6f591ff5d0,
		0x2717b96ebe280b8b6a6234ee670605a7, 0x7b487ec66f7ea0e03f1080c626077447,
		0x9ef18e979fe7e391c0a4f84aa50a550d, 0x62b0e5f3415a9e7ed48d605081727686,
		0x84c9f4ce001ae4e37a205440ec1f9ffc, 0xa554c324117e2e55d895fa9df594d74f,
		0xb2c4a50fe27ff578286efebd2872df5b, 0x7f5928eb85937e442ed349eeef7c8905,
		0x65e4d61df128865e4a3124b337695f70, 0x8a87d423e843fe74e720b95104771bc7,
		0xc1d9309b097acbddf2947692a3e8297d, 0xbf829cf24f4924dae01bdc5bfb301b1d,
		0x48bcf8de0544320dffbf70b431bae7a4, 0xa08b29e0c1c39f4539d3bb5332fcae3b,
		0x34f19042123470940f09aef7fd05c9e5, 0x4a982f4f368e3be995ed44e301b771a2,
		0xffaf52874b44c14715f66ca0631d4088, 0xe68c6eccc5b6704630c60ae2f14abb7e,
		0xae183ec84b849dda00ca4fbd56a4d5a4, 0x67255c1468cea6e8add1643045ce5773,
		0x9a99949a5806e93316e10ecbf28cdaa3, 0x1885d1a07facced17b846fc220b2601f,
		0x46b4a5aac01c9a50d319dd8da15b5932, 0x7eee560bab19caf6ba6b04e467633d9f,
		0xee51363b35f7bde9742128a9ea79b11f, 0x01707da3fec2463a76d350755aac571d,
		0x79676b9e20eced7842d8a498afc135f7, 0x832c83324d3bc3faa8db3aea15638341,
		0x9a762db734f04059f347271c1f3b40a7, 0xef5957dc398dfdb8fd4f21d26c4e3ee7,
		0x0d70f36849d7a25bdaeb492b490c9b8d, 0x658ef8e4f0e9a5f584558d7ad0ae3b7d,
		0x5aec3e759e07a80c533b1036f4a2b8a0, 0x4cbcbaf8555cb05b4f88e85692946891,
		0x5d1c6b72d6f4da757b9487f3993bbbe3, 0x71db28b850a5346c6db334dc28acae64,
		0xfc75dd593364dbe32a518d10f2e261f8, 0xb043e8023cd1bb67a23fce43f1bcac1c,
		0x5c5316b44d19347f75a12988ca5b0a33, 0x3fafeeb6d77574791e4d790ec3943b92,
		0x5127234c097ef45c21391abef7d4a8ea, 0xadd5a66d4a17a344d23c32ba5324a326,
		0x563c6b91983d598308c9f2afa63e1db5, 0xf6c76e08cc3ee2464d608672a17cf84c,
		0x2ae6c4efa566d62b5e76bcb1b333982f, 0x6321efbc1582ee7436d4c1bee8b6f406,
		0x26585806c45a7da769c953f40d4ec1fd, 0x3f9d63283daf907e16fae0061614c17e,
		0x300cd4b730ceaa5f0cd29b00e3f2c9d2, 0x9af8cee3d830eb0d9832e0f216512a74,
		0xd36886046ee651ff9279f1b57b9ec54b, 0x05750a17f3a6e6cc316796e6574d239b,
		0x62a205f88452173cce6c3213d98176b1, 0x486a9323825446ff47154778b3cb2bf4,
		0x8e5086fc897cfcf265655e4e0758df38, 0x4e477830a20940f086ca0bd0442e7031,
		0xbd3a2ce437e95ef78338f7d139eea065, 0xe7de9fefd1ed44a36ff8130126b29721,
		0xbe42dc12f6f7853cd992257615dfa08b, 0xdea83eaada7d8d537eb027ab7ceca7d8,
		0xf908731afd43f65ad86902bd93ce25aa, 0x6a21fd4c33664d97a5194a17daef5fc0,
		0x9b54cdedbb0f1eea701541db3198b435, 0xe26f4791bf9d75f672409751a163d09a]
 
 -------------------------------- Word128 ---------------------------


type Word128 = (Word64, Word64)

toIntegerW128 :: Word128 -> Integer
toIntegerW128 (l,h) = shiftL (toInteger h) 64 + (toInteger l)

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
word128Abs x = x

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

