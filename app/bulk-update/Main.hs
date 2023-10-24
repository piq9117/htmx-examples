module Main where

import BulkUpdate (bulkUpdate, usersDatabase)
import Server (server)

main :: IO ()
main = do
  usersRef <- usersDatabase
  server (bulkUpdate usersRef)
