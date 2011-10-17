{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Digest.JH224 (
         jh224,
         JH224Digest(..),
         JHContext (..),
         
         Hash(..),
         hash,
         hash'

      )  where


import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.List (foldl')
import Control.Monad (liftM)
import Crypto.Classes 
import Data.Tagged
import Data.Serialize
import Prelude hiding (truncate)


import Data.Digest.JHInternal

jh224 :: Int64 -> L.ByteString -> L.ByteString
jh224 dataBitLen = truncate JH224 . foldl' f8 jh224_h0 . parseMessage dataBitLen

---------------------- Crypto-api instance -------------

instance Hash JHContext JH224Digest where 
   outputLength = Tagged 224
   blockLength  = Tagged 512
   initialCtx   = jhInit JH224 jh224_h0
   updateCtx    = jhUpdate
   finalize ctx = Digest . jhFinalize ctx

data JH224Digest = Digest L.ByteString
	deriving (Eq,Ord)

instance Show JH224Digest where
   show (Digest h) = printAsHex h

instance Serialize JH224Digest where
   put (Digest bs) = put bs
   get = liftM Digest get

--------------------- Initial hash value -----------------
jh224_h0 :: Block1024
jh224_h0 = (B 0x2dfedd62f99a98acae7cacd619d634e7 0xa4831005bc301216b86038c6c9661494
              0x66d9899f2580706fce9ea31b1d9b1adc 0x11e8325f7b366e10f994857f02fa06c1, 
            B 0x1b4f1b5cd8c840b397f6a17f6e738099 0xdcdf93a5adeaa3d3a431e8dec9539a68
              0x22b4a98aec86a1e4d574ac959ce56cf0 0x15960deab5ab2bbf9611dcf0dd64ea6e)
