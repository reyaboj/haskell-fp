{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import qualified Data.Text.Lazy as T
import Data.Monoid (mconcat)

main :: IO ()
main = scotty 3000 $ do
    get "/:belo" $ do
        belo <- param "belo"
        html . T.pack . mconcat $ [
            "<h1>", belo, "</h1>"
            ]