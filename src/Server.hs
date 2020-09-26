module Server
  ( startServer,
  )
where

import Control.Concurrent ( withMVar, forkIO, newMVar )
import Data.ByteString (ByteString)
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Socket
    ( setSocketOption,
      accept,
      bind,
      listen,
      socket,
      SocketOption(ReuseAddr),
      Family(AF_INET),
      SockAddr(SockAddrInet),
      Socket,
      SocketType(Stream) )
import Network.Socket.ByteString (recv, sendAllTo)

type HandlerFunc = (Socket, SockAddr) -> String -> IO ()

startServer :: HandlerFunc -> IO ()
startServer handlerFunc = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 5001 0)
  listen sock 2
  lock <- newMVar ()
  processRequests lock sock
  where
    handleMessage lock addr message =
      withMVar lock (\a -> handlerFunc addr message >> return a)

    processRequests lock mastersock = do
      conn <- accept mastersock
      putStrLn $ "Connected client " ++ (show $ snd conn)
      threadId <- forkIO (processMessages lock conn) -- run connection in a new thread
      putStrLn $ show threadId
      processRequests lock mastersock

    processMessages lock (conSock, addr) = do
      byteStr <- receiveMessage conSock
      putStrLn $ "Received message: " ++ (convertToString byteStr)
      handlerFunc (conSock, addr) (convertToString byteStr)

    receiveMessage sock =
      recv sock 1024

sendUtf8String :: String -> (Socket, SockAddr) -> IO ()
sendUtf8String [] _ = return ()
sendUtf8String str (socket, addr) = sendAllTo socket (convertToUtf8 str) addr

convertToUtf8 :: String -> ByteString
convertToUtf8 = encodeUtf8 . pack

convertToString :: ByteString -> String
convertToString = unpack . decodeUtf8