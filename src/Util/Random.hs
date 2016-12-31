module Util.Random (getRandomToken, getRandomTokenN) where

import System.Random   ( randomIO )

import Control.Monad   ( replicateM )
import Data.Word       ( Word8 )
import Data.ByteString ( ByteString, pack )

getRandomToken :: IO ByteString
getRandomToken = getRandomTokenN 32

getRandomTokenN :: Int -> IO ByteString
getRandomTokenN n = pack <$> replicateM n (randomIO :: IO Word8)
