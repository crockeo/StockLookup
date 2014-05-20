{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Middleware.Static
import Control.Monad.Trans
import Data.Text.Lazy
import Data.Monoid
import Web.Scotty

import Templater
import Data

-- Static files
staticFiles :: ScottyM ()
staticFiles = do
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
    tScode <- param "scode"

    let scode = unpack tScode in
      if scode == ""
        then html =<< liftIO (simpleTextPage "nostock")
        else html =<< liftIO (do
          rendered <- codeToHTML scode

          textPage [("pageName", "stock"), ("scode", scode), ("renderedCSV", rendered)] "templates/stock.html")

-- Serving a 404 page
serve404 :: ScottyM ()
serve404 =
  notFound $ do
    html =<< liftIO (simpleTextPage "404")

main :: IO ()
main = scotty 80 $ do
  -- Loading the routes
  staticFiles
  serveIndex
  serveInformation
  serveStock
  serve404