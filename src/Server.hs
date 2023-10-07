{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Server (server) where

import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty qualified as Scotty

server :: Scotty.ScottyM () -> IO ()
server scottyApp = Scotty.scotty 3000 $ do
  Scotty.middleware logStdout
  scottyApp
