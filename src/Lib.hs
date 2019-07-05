module Lib
    ( getLocalPort
    , getRemotePort
    , getRemoteAddr
    ) where

import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString
import qualified Data.ByteString as B
import           System.Environment

getLocalPort :: IO String
getLocalPort = do
  [a,b,c] <- getArgs
  return a

getRemotePort :: IO String
getRemotePort = do
  [a,b,c] <- getArgs
  return c

getRemoteAddr :: IO String
getRemoteAddr = do
  [a,b,c] <- getArgs
  return b
