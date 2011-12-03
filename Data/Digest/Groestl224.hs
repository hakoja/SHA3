{-# LANGUAGE BangPatterns, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

module Data.Digest.Groestl224 (
            groestl224,
            
            Groestl224Digest (..),
            GroestlCtx,
            Hash(..),
            hash,
            hash'
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

groestl224 :: Int64 -> L.ByteString -> L.ByteString
groestl224 dataBitLen
    | dataBitLen < 0 = error "The data length can not be less than 0"
    | otherwise = truncate G224 . outputTransform 512 . compress . parse
    where parse = parseMessage dataBitLen 512
          compress xs = runST $ foldM (fM 512) h0_224 xs

---------------------- Crypto-api instance -------------

instance Hash GroestlCtx Groestl224Digest where 
    outputLength = Tagged 224
    blockLength  = Tagged 512
    initialCtx   = groestlInit G224 512 h0_224
    updateCtx    = groestlUpdate
    finalize ctx = Digest . groestlFinalize ctx

newtype Groestl224Digest = Digest L.ByteString
    deriving (Eq, Ord)

instance Serialize Groestl224Digest where
    put (Digest bs) = put bs
    get = get

instance Show Groestl224Digest where
    show (Digest bs) = printAsHex bs
------------------------------------------- Initial vector -------------------------------------

h0_224 :: Vector Word64
h0_224 = fromList [0, 0, 0, 0, 0, 0, 0, 0xe0]
