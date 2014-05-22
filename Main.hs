{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Middleware.Static
import Control.Monad.Trans
import Data.Text.Lazy
import Data.Monoid
import Web.Scotty

import Templater
import Data
import CSV

-- Static files
staticFiles :: ScottyM ()
staticFiles =
  middleware $ staticPolicy (noDots >-> addBase "static")

-- Serving the index page
serveIndex :: ScottyM ()
serveIndex =
  get "/" $
    html =<< liftIO (simpleTextPage "index")

-- Serving the information page
serveInformation :: ScottyM ()
serveInformation =
  get "/information" $
    html =<< liftIO (simpleTextPage "information")

-- Serving a stock page
serveStock :: ScottyM ()
serveStock =
  get "/stock" $ do
    tScode <- param "scode" `rescue` (\x -> redirect "/stock?scode=")

    let scode = unpack tScode in
      html =<< liftIO (do
        (b, c) <- openCode scode

        if c == 404
          then textPage [("pageName", "stock"), ("scode", scode)]                                   "templates/nostock.html"
          else textPage [("pageName", "stock"), ("scode", scode), ("renderedCSV", constructHTML b)] "templates/stock.html")

-- Serving a 404 page
serve404 :: ScottyM ()
serve404 =
  notFound $
    html =<< liftIO (simpleTextPage "404")

main :: IO ()
main = scotty 80 $ do
  -- Loading the routes
  staticFiles
  serveIndex
  serveInformation
  serveStock
  serve404