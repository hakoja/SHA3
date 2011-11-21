{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS -fno-warn-orphans #-}

module Data.Digest.Groestl224 (
            groestl224,
            groestl224Par,
            
            Groestl224Digest (..),
            GroestlCtx,
            Hash(..),
            hash,
            hash',
            
            printAsHex
      ) where


import Data.Int (Int64)
import Data.Word (Word64)
import Data.List (foldl')
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.Vector.Unboxed (Vector, fromList)
import Control.Monad (foldM, liftM)
import Control.Monad.ST (runST)
import Crypto.Classes 
import Data.Tagged
import Data.Serialize
import Prelude hiding (truncate)

import Data.Digest.GroestlMutable

groestl224 :: Int64 -> L.ByteString -> L.ByteString
groestl224 dataBitLen
    | dataBitLen < 0 = error "The data length can not be less than 0"
    | otherwise = truncate G224 . outputTransform . compress . parse
    where parse = parseMessage dataBitLen 512
          compress xs = runST (foldM f512M h0_224 xs)


-- Experimental version using the parallel version of f512
groestl224Par :: Int64 -> L.ByteString -> L.ByteString
groestl224Par dataBitLen
    | dataBitLen < 0 = error "The data length can not be less than 0"
    | otherwise = truncate G224 . outputTransform . foldl' f512Par h0_224 . parseMessage dataBitLen 512

---------------------- Crypto-api instance -------------

instance Hash GroestlCtx Groestl224Digest where 
    outputLength = Tagged 224
    blockLength  = Tagged 512
    initialCtx   = groestlInit G224 h0_224
    updateCtx    = groestlUpdate
    finalize ctx = Digest . groestlFinalize ctx

data Groestl224Digest = Digest L.ByteString
    deriving (Eq,Ord)

instance Show Groestl224Digest where
    show (Digest h) = printAsHex h

instance Serialize Groestl224Digest where
    put (Digest bs) = put bs
    get = liftM Digest get

------------------------------------------- Initial vector -------------------------------------

h0_224 :: Vector Word64
h0_224 = fromList [0, 0, 0, 0, 0, 0, 0, 0xe0]
