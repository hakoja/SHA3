{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Digest.JH224 (
         jh224,
         JH224Digest,
         
         Hash(..),
         hash,
         hash',
         JHContext (..)
      ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.List (foldl')
import Prelude hiding (truncate)
import Control.Monad (liftM)
import Data.Serialize
import Crypto.Classes 
import Data.Tagged
import Control.Arrow

import Data.Digest.JHInternal

jh224 :: Int64 -> L.ByteString -> L.ByteString
jh224 dataLen = truncate JH224 . foldl' f8 jh224_h0 . parseMessage dataLen

-- Initial hash value
jh224_h0 :: Block1024
jh224_h0 = (B 0x2dfedd62f99a98acae7cacd619d634e7 0xa4831005bc301216b86038c6c9661494
              0x66d9899f2580706fce9ea31b1d9b1adc 0x11e8325f7b366e10f994857f02fa06c1, 
            B 0x1b4f1b5cd8c840b397f6a17f6e738099 0xdcdf93a5adeaa3d3a431e8dec9539a68
              0x22b4a98aec86a1e4d574ac959ce56cf0 0x15960deab5ab2bbf9611dcf0dd64ea6e)

---------------------- Crypto-api instance -------------

data JHContext = Ctx {
            dataParsed :: !Int64,
            hashState :: !Block1024
         } deriving (Show)

data JH224Digest = Digest L.ByteString
   deriving (Eq,Ord)

instance Show JH224Digest where
   show (Digest h) = printAsHex h

instance Serialize JH224Digest where
   put (Digest bs) = put bs
   get = liftM Digest get
   
instance Hash JHContext JH224Digest where 
   outputLength = Tagged 224
   blockLength  = Tagged 512
   initialCtx   = jh224Init
   updateCtx    = jh224Update
   finalize     = jh224Finalize

jh224Init :: JHContext
jh224Init = Ctx {dataParsed = 0, hashState = jh224_h0}

jh224Update :: JHContext -> B.ByteString -> JHContext
jh224Update ctx bs
   | B.null  bs = ctx
   | otherwise  = result
   where 
   (!newState, result) = foldUpdate . B.splitAt 64 $ bs
   foldUpdate = hashBlock *** jh224Update newCtx
   hashBlock = f8 (hashState ctx) . parseBlock
   newCtx = Ctx (dataParsed ctx + 512) newState      

jh224Finalize :: JHContext -> B.ByteString -> JH224Digest
jh224Finalize ctx bs = 
   Digest . truncate JH224 . foldl' f8 prevState $ pad n bs
      where !n = dataParsed ctx + (fromIntegral $ B.length bs * 8)
            !prevState = hashState ctx 