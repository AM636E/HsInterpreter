module Server
  ( startServer,
    HandlerFunc,
  )
where

import Control.Concurrent (forkIO, newMVar, withMVar)
import Data.ByteString (ByteString)
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Socket
    ( defaultHints,
      getAddrInfo,
      setSocketOption,
      accept,
      bind,
      listen,
      socket,
      defaultProtocol,
      AddrInfo(addrFlags, addrFamily, addrAddress),
      AddrInfoFlag(AI_PASSIVE),
      SocketOption(ReuseAddr),
      SockAddr,
      Socket,
      SocketType(Stream) )
import Network.Socket.ByteString (recv, sendAllTo)

type HandlerFunc = String -> Either String String

startServer :: String -> HandlerFunc -> IO ()
startServer port handlerFunc = do
  addrInfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)

  let addrInfo = head addrInfos

  sock <- socket (addrFamily addrInfo) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addrInfo)
  listen sock 5

  lock <- newMVar ()
  processRequests lock sock
  where
    processRequests lock mastersock = do
      conn <- accept mastersock
      putStrLn $ "Connected client " ++ (show $ snd conn)
      threadId <- forkIO (processMessages lock conn) -- run connection in a new thread
      putStrLn $ show threadId
      processRequests lock mastersock

    processMessages lock (conSock, addr) = do
      byteStr <- receiveMessage conSock
      putStrLn $ "Received message: " ++ (convertToString byteStr)
      withMVar
        lock
        ( \a -> case handlerFunc $ convertToString byteStr of
            Right error -> sendUtf8String ("Error " ++ error) (conSock, addr)
            Left result -> sendUtf8String result (conSock, addr)
        )
    -- TODO: handle message parts
    receiveMessage sock =
      recv sock 1024

sendUtf8String :: String -> (Socket, SockAddr) -> IO ()
sendUtf8String [] _ = return ()
sendUtf8String str (socket, addr) = sendAllTo socket (convertToUtf8 str) addr

convertToUtf8 :: String -> ByteString
convertToUtf8 = encodeUtf8 . pack

convertToString :: ByteString -> String
convertToString = unpack . decodeUtf8