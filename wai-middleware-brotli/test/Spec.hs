{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Types
import Codec.Compression.Brotli (decompress, compress)
import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as Builder
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Brotli
import Network.Wai.Internal

main :: IO ()
main = do
  bod <- L.readFile "/usr/share/dict/words" -- "Hello World 000000000000000000000000000000"
  -- app :: Application
  let app req respond = respond $ responseLBS status200 [] bod
  var <- newEmptyMVar
  (brotli app)
    (defaultRequest {requestHeaders = [("Accept-Encoding", "br")]})
    (\resp -> do
       putMVar var resp
       return ResponseReceived)
  resp <- takeMVar var
  case resp of
    ResponseBuilder stats hs b -> do
      print hs
      print (decompress (Builder.toLazyByteString b) == bod)
    _ -> error "Not what we expected"

  var <- newEmptyMVar
  (brotli app)
    (defaultRequest {requestHeaders = [("Accept-Encoding", "gzip")]})
    (\resp -> do
       putMVar var resp
       return ResponseReceived)
  resp <- takeMVar var
  case resp of
    ResponseBuilder stats hs b -> print (Builder.toLazyByteString b == bod)
    _ -> error "Not what we expected"

  var <- newEmptyMVar
  let reqWithBody =
        defaultRequest
        { requestMethod = "POST"
        , requestHeaders = [("Content-Encoding", "br")]
        , requestBody =
            return $ L.toStrict $ compress ("Hello World" :: L.ByteString)
        }
  (brotli $ \req resp -> do
     reqBod <- requestBody req
     resp $ responseLBS status200 [] (L.fromStrict reqBod))
    reqWithBody
    (\resp -> do
       putMVar var resp
       return ResponseReceived)
  resp <- takeMVar var
  case resp of
    ResponseBuilder stats hs b ->
      print (Builder.toLazyByteString b == "Hello World")
    _ -> error "Not what we expected"

-- curl -H "Accept-Encoding: br" http://leviathan.local:9450 | bro --decompress
-- Should return Hello World etc.
