{-# LANGUAGE ImportQualifiedPost #-}

module Server (server) where

import Web.Scotty qualified as Scotty

server :: Scotty.ScottyM () -> IO ()
server scottyApp = Scotty.scotty 3000 scottyApp
