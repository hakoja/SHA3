{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS -fno-warn-orphans #-}

module Data.Digest.Groestl256 (
            groestl256,
            
            Groestl256Digest (..),
            GroestlCtx,
            Hash(..),
            hash,
            hash',
            
            printAsHex
      ) where


import Data.Int (Int64)
import Data.Word (Word64)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.Vector.Unboxed (Vector, fromList)
import Control.Monad (foldM, liftM)
import Control.Monad.ST (runST)
import Crypto.Classes 
import Data.Tagged
import Data.Serialize
import Prelude hiding (truncate)

import Data.Digest.GroestlMutable


groestl256 :: Int64 -> L.ByteString -> L.ByteString
groestl256 dataBitLen
    | dataBitLen < 0 = error "The data length can not be less than 0"
    | otherwise = truncate G256 . outputTransform . compress . parse
    where parse = parseMessage dataBitLen 512
          compress xs = runST (foldM f512M h0_256 xs)


---------------------- Crypto-api instance -------------

instance Hash GroestlCtx Groestl256Digest where 
    outputLength = Tagged 256
    blockLength  = Tagged 512
    initialCtx   = groestlInit G256 h0_256
    updateCtx    = groestlUpdate
    finalize ctx = Digest . groestlFinalize ctx

data Groestl256Digest = Digest L.ByteString
    deriving (Eq,Ord)

instance Show Groestl256Digest where
    show (Digest h) = printAsHex h

instance Serialize Groestl256Digest where
    put (Digest bs) = put bs
    get = liftM Digest get

------------------------------------------- Initial vector -------------------------------------

h0_256 :: Vector Word64
h0_256 = fromList [0, 0, 0, 0, 0, 0, 0, 0x0100]
