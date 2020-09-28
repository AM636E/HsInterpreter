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

startRpnServer :: String -> IO ()
startRpnServer port = startServer port rpnHandler >>= (\_ -> putStrLn $ "RPN Server listening on " ++ port)
