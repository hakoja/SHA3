{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Digest.JH512 (
         jh512,
         JH512Digest(..),
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

jh512 :: Int64 -> L.ByteString -> L.ByteString
jh512 dataBitLen 
    | dataBitLen < 0 = error "The data length can not be less than 0"
    | otherwise      = truncate JH512 . foldl' f8 jh512_h0 . parseMessage dataBitLen

---------------------- Crypto-api instance -------------

instance Hash JHContext JH512Digest where 
   outputLength = Tagged 512
   blockLength  = Tagged 512
   initialCtx   = jhInit JH512 jh512_h0
   updateCtx    = jhUpdate
   finalize ctx = Digest . jhFinalize ctx

data JH512Digest = Digest L.ByteString
    deriving (Eq,Ord)

instance Show JH512Digest where
   show (Digest h) = printAsHex h

instance Serialize JH512Digest where
   put (Digest bs) = put bs
   get = liftM Digest get

--------------------- Initial hash value -----------------
jh512_h0 :: Block1024
jh512_h0 = (B 0x6fd14b963e00aa17636a2e057a15d543 0x8a225e8d0c97ef0be9341259f2b3c361
              0x891da0c1536f801e2aa9056bea2b6d80 0x588eccdb2075baa6a90f3a76baf83bf7,
            B 0x0169e60541e34a6946b58a8e2e6fe65a 0x1047a7d0c1843c243b6e71b12d5ac199 
              0xcf57f6ec9db1f856a706887c5716b156 0xe3c2fcdfe68517fb545a4678cc8cdd4b)
