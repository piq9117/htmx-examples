module Main where

import ClickToEdit (clickToEdit)
import Server (server)

main :: IO ()
main = server clickToEdit
