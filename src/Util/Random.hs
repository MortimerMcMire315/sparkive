module Util.Random (getRandomToken, getRandomByteString) where

import System.Random   ( randomIO )

import Control.Monad   ( replicateM )
import Data.Word       ( Word8 )
import Data.ByteString ( ByteString, pack )

getRandomToken :: IO ByteString
getRandomToken = getRandomByteString 32

getRandomByteString :: Int -> IO ByteString
getRandomByteString n = pack <$> replicateM n (randomIO :: IO Word8)
