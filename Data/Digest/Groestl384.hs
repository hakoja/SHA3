{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS -fno-warn-orphans #-}

module Data.Digest.Groestl384 (
            groestl384,
            
            Groestl384Digest (..),
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

groestl384 :: Int64 -> L.ByteString -> L.ByteString
groestl384 dataBitLen
    | dataBitLen < 0 = error "The data length can not be less than 0"
    | otherwise = truncate G384 . outputTransform 1024 . compress . parse
    where parse = parseMessage dataBitLen 1024
          compress xs = runST $ foldM (fM 1024) h0_384 xs

---------------------- Crypto-api instance -------------

instance Hash GroestlCtx Groestl384Digest where 
    outputLength = Tagged 384
    blockLength  = Tagged 1024
    initialCtx   = groestlInit G384 1024 h0_384
    updateCtx    = groestlUpdate
    finalize ctx = Digest . groestlFinalize ctx

data Groestl384Digest = Digest L.ByteString
    deriving (Eq,Ord)

instance Show Groestl384Digest where
    show (Digest h) = printAsHex h

instance Serialize Groestl384Digest where
    put (Digest bs) = put bs
    get = liftM Digest get

------------------------------------------- Initial vector -------------------------------------

h0_384 :: Vector Word64
h0_384 = fromList [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0x0180]
