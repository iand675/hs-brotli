{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Brotli

main :: IO ()
main = do
  putStrLn "Booting up"
  runEnv 3000 $ brotliEncodeMiddleware app

app :: Application
app req respond = do
  putStrLn "Received a request, inform the men"
  print req
  (respond $ responseLBS status200 [] "Hello World 000000000000000000000000000000")
