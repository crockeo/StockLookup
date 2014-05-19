module Main where

import Hack2.Handler.SnapServer
import Network.Miku

main :: IO ()
main = run . miku $ get "/" $ text "rawr"