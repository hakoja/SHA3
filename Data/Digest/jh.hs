module Data.Digest.JH 
   (
      jh
   ) where 

import qualified Data.ByteString.Lazy as L

class Hashable a where 
   hash :: a -> L.ByteString

jh :: Hashable a => a -> L.ByteString
jh = undefined