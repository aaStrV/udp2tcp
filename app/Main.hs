module Main where

import           Data.Bits
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString
--import           Network.BSD
import           Data.List
import           System.Environment
import qualified Data.ByteString as B
import qualified Control.Exception as E
import           Lib

main :: IO ()
main = do
  [uport, tip, tport] <- getArgs
  putStrLn $ uport ++ " -> " ++ tip ++ ":" ++ tport
  serve uport
  putStrLn "Bye-bye"

serve :: String -> IO ()
serve port = withSocketsDo $
  do
    -- TCP socket
    rport <- getRemotePort
    raddr <- getRemoteAddr
    remoteaddrinfo <- getAddrInfo
                      (Just (defaultHints { addrSocketType = Stream }))
                      (Just raddr)
                      (Just rport)
    let remoteaddr = head remoteaddrinfo
    
    -- UDP socket
    addrinfosl <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just port)
    let serveraddrl = head addrinfosl
    sockudp <- socket (addrFamily serveraddrl) Datagram defaultProtocol
    bind sockudp (addrAddress serveraddrl)

    let raddr = addrAddress remoteaddr
    procMessages sockudp raddr

procMessages :: Socket -> SockAddr -> IO ()
procMessages localsock remoteaddr = do
  (bmsg, localaddr) <- recvFrom localsock 1024
  E.bracket (open remoteaddr) close (talk bmsg localaddr)
    `E.catch` \e -> putStrLn ("Caught exception - " ++ show (e :: E.SomeException))
  procMessages localsock remoteaddr
    where
      open remoteaddr = do
        remotesock <- socket AF_INET Stream defaultProtocol
        connect remotesock remoteaddr
        return remotesock
      talk breq localaddr remotesock = do
        send remotesock breq
        bresp <- recv remotesock 1024
        sendTo localsock bresp localaddr
        return ()
