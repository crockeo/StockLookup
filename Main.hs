{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hack2.Handler.SnapServer
import Hack2.Contrib.Response
import Hack2.Contrib.Request

import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad

import Network.Miku.Engine
import Network.Miku.Utils
import Network.Miku.Type
import Network.Miku

import Data.Char

import Templater
import Data

-- Serving the index page
serveIndex :: MikuMonad
serveIndex =
  get "/" $ html =<< liftIO (simpleBsPage "index")

-- Serving the information page
serveInformation :: MikuMonad
serveInformation =
  get "/information" $ html =<< liftIO (simpleBsPage "information")

-- Serving the stock page
serveStock :: MikuMonad
serveStock =
  get "/stock" $ do
    env <- ask

    case lookup "scode" $ params env of
      Nothing  -> serveNoStock
      Just (c) ->
        if c == ""
          then serveNoStock
          else serveStock $ map (toUpper) $ unbs c
  where serveNoStock   = html =<< liftIO (bsPage [("pageName", "stock")              ] "templates/nostock.html")
        serveStock   c = html =<< liftIO (do
          html <- codeToHTML c

          (bsPage [("pageName", "stock"), ("scode", c), ("renderedCSV", html)] "templates/stock.html"  ))

-- Serving the 404 page
serve404 :: MikuMonad
serve404 =
  get "*" $ html =<< liftIO (simpleBsPage "404")

main :: IO ()
main =
  run . miku $ do
    -- Setting up the static files
    public (Just ".") ["/css/", "/js/"]

    -- Setting up the routes
    serveIndex
    serveInformation
    serveStock
    serve404