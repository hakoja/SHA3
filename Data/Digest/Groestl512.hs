{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS -fno-warn-orphans #-}

module Data.Digest.Groestl512 (
            groestl512,
            
            Groestl512Digest (..),
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

groestl512 :: Int64 -> L.ByteString -> L.ByteString
groestl512 dataBitLen
    | dataBitLen < 0 = error "The data length can not be less than 0"
    | otherwise = truncate G512 . outputTransform 1024 . compress . parse
    where parse = parseMessage dataBitLen 1024
          compress xs = runST $ foldM (fM 1024) h0_512 xs

---------------------- Crypto-api instance -------------

instance Hash GroestlCtx Groestl512Digest where 
    outputLength = Tagged 512
    blockLength  = Tagged 1024
    initialCtx   = groestlInit G512 1024 h0_512
    updateCtx    = groestlUpdate
    finalize ctx = Digest . groestlFinalize ctx

data Groestl512Digest = Digest L.ByteString
    deriving (Eq,Ord)

instance Show Groestl512Digest where
    show (Digest h) = printAsHex h

instance Serialize Groestl512Digest where
    put (Digest bs) = put bs
    get = liftM Digest get

------------------------------------------- Initial vector -------------------------------------

h0_512 :: Vector Word64
h0_512 = fromList [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0x0200]
