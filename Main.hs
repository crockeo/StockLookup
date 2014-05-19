{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hack2.Handler.SnapServer

import Network.Miku.Engine
import Network.Miku.Utils
import Network.Miku

import Control.Monad

import Air.Env hiding ((.))

import Templater

main :: IO ()
main =
  run . miku $ do
    public (Just ".") ["/*.hs", "/css/", "/js/"]

    -- The index page
    get "/" $ html =<< io (bsPage [("pageName", "index")] "templates/index.html")