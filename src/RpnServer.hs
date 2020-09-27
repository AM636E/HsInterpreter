module RpnServer
  ( startRpnServer,
  )
where

import Rpn (rpn)
import Server (HandlerFunc, startServer)

rpnHandler :: HandlerFunc
rpnHandler str = case rpn str of
  Left result -> Left $ show result
  Right error -> Right error

startRpnServer :: IO ()
startRpnServer = startServer rpnHandler >>= (\_ -> putStrLn "Server started.")
