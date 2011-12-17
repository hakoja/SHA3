{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS -fno-warn-orphans #-}

module Data.Digest.JH256 (
         jh256,
         JH256Digest(..),
         JHContext (..),
         
         Hash(..),
         hash,
         hash',
         
         printAsHex,
         printAsHex'

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

jh256 :: Int64 -> L.ByteString -> L.ByteString
jh256 dataBitLen
    | dataBitLen < 0 = error "The data length can not be less than 0"
    | otherwise      = truncate JH256 . foldl' f8 jh256_h0 . parseMessage dataBitLen

---------------------- Crypto-api instance -------------

instance Hash JHContext JH256Digest where 
   outputLength = Tagged 256
   blockLength  = Tagged 512
   initialCtx   = jhInit JH256 jh256_h0
   updateCtx    = jhUpdate
   finalize ctx = Digest . jhFinalize ctx

data JH256Digest = Digest L.ByteString
    deriving (Eq,Ord)

instance Show JH256Digest where
   show (Digest h) = printAsHex h

instance Serialize JH256Digest where
   put (Digest bs) = put bs
   get = liftM Digest get

--------------------- Initial hash value -----------------
jh256_h0 :: Block1024
jh256_h0 = B1024 (B512 0xeb98a3412c20d3eb92cdbe7b9cb245c1 0x1c93519160d4c7fa260082d67e508a03
                       0xa4239e267726b945e0fb1a48d41a9477 0xcdb5ab26026b177a56f024420fff2fa8)
                 (B512 0x71a396897f2e4d751d144908f77de262 0x277695f776248f9487d5b6574780296c 
                       0x5c5e272dac8e0d6c518450c657057a0f 0x7be4d367702412ea89e3ab13d31cd769)
