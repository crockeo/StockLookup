{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hack2.Handler.SnapServer
import Data.ByteString.Char8 (unpack, pack)
import Network.Miku.Type
import Network.Miku

import Templater

-- Serving the index page
serveIndexIO :: IO MikuMonad
serveIndexIO =
  (bsPage [("pageName", "index")] "templates/index.html") >>= return . get "/" . html

main :: IO ()
main = do
  -- Recieving the routes
  serveIndex <- serveIndexIO

  index <- bsPage [("pageName", "index")] "templates/index.html"

  run . miku $ do
    public (Just ".") ["/css/", "/js/"]

    serveIndex