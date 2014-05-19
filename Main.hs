{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hack2.Handler.SnapServer

import Network.Miku.Engine
import Network.Miku.Utils
import Network.Miku.Type
import Network.Miku

import Control.Monad

import Air.Env hiding ((.))

import Templater

-- Serving the index page
serveIndex :: MikuMonad
serveIndex =
  get "/" $ html =<< io (bsPage [("pageName", "index")] "templates/index.html")

-- Serving the 404 page
serve404 :: MikuMonad
serve404 =
  get "*" $ html =<< io (bsPage [("pageName", "404")] "templates/404.html")

main :: IO ()
main =
  run . miku $ do
    -- Setting up the static files
    public (Just ".") ["/css/", "/js/"]

    -- Setting up the routes
    serveIndex
    serve404