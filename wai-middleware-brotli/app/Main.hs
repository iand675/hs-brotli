{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Types.Status
import Network.Wai
import qualified Network.Wai.Middleware.Brotli as B
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static

main :: IO ()
main = do
  let settings =
        B.defaultSettings
        {B.brotliFilesBehavior = B.BrotliPreCompressed (B.BrotliCacheFolder "."), B.brotliMinimumSize = 5}
      app = staticApp $ defaultFileServerSettings "."
  runEnv 3000 (B.brotli' settings app)
