{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS -fno-warn-orphans #-}

module Data.Digest.JH384 (
         jh384,
         JH384Digest(..),
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

jh384 :: Int64 -> L.ByteString -> L.ByteString
jh384 dataBitLen
    | dataBitLen < 0 = error "The data length can not be less than 0"
    | otherwise      = truncate JH384 . foldl' f8 jh384_h0 . parseMessage dataBitLen

---------------------- Crypto-api instance -------------

instance Hash JHContext JH384Digest where 
   outputLength = Tagged 384
   blockLength  = Tagged 512
   initialCtx   = jhInit JH384 jh384_h0
   updateCtx    = jhUpdate
   finalize ctx = Digest . jhFinalize ctx

data JH384Digest = Digest L.ByteString
    deriving (Eq,Ord)

instance Show JH384Digest where
   show (Digest h) = printAsHex h

instance Serialize JH384Digest where
   put (Digest bs) = put bs
   get = liftM Digest get

--------------------- Initial hash value -----------------
jh384_h0 :: Block1024
jh384_h0 = B1024 (B 0x481e3bc6d813398a6d3b5e894ade879b 0x63faea68d480ad2e332ccb21480f8267
                    0x98aec84d9082b928d455ea3041114249 0x36f555b2924847ecc7250a93baf43ce1)
                 (B 0x569b7f8a27db454c9efcbd496397af0e 0x589fc27d26aa80cd80c08b8c9deb2eda
                    0x8a7981e8f8d5373af43967adddd17a71 0xa9b4d3bda475d394976c3fba9842737f)
