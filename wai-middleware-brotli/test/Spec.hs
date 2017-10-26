{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Types
import Codec.Compression.Brotli (decompress, compress)
import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as Builder
import Data.IORef
import Data.Monoid
import Network.Wai
import Network.Wai.Middleware.Brotli
import Network.Wai.Internal
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn ""

  bod <- L.readFile "/usr/share/dict/words" -- "Hello World 000000000000000000000000000000"
  -- app :: Application
  putStr "Encoding text: "
  let app req respond = respond $ responseLBS status200 [("Content-Type", "text/whatever")] bod
  var <- newEmptyMVar
  (brotli app)
    (defaultRequest {requestHeaders = [("Accept-Encoding", "br")]})
    (\resp -> do
       putMVar var resp
       return ResponseReceived)
  resp <- takeMVar var
  case resp of
    ResponseBuilder stats hs b -> do
      print (decompress (Builder.toLazyByteString b) == bod)
    _ -> error "Not what we expected"
  putStrLn ""

  putStr "Not encoding if brotli not in Accept-Encoding header: "
  var <- newEmptyMVar
  (brotli app)
    (defaultRequest {requestHeaders = [("Accept-Encoding", "gzip"), ("Content-Type", "text/whatever")]})
    (\resp -> do
       putMVar var resp
       return ResponseReceived)
  resp <- takeMVar var
  case resp of
    ResponseBuilder stats hs b -> do
      print (Builder.toLazyByteString b == bod)
    _ -> error "Not what we expected"
  putStrLn ""

  putStr "Decode request body if it's encoded: "
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
  putStrLn ""

  putStr "Streaming response body encoding: "
  var <- newEmptyMVar
  (brotli $ \req respond -> do
      respond $ responseStream status200 [("Content-Type", "text/whatever")] $ \send flush -> do
        send "Hello world\n"
        flush
        send "Goodbye world"
    )
    (defaultRequest {requestHeaders = [("Accept-Encoding", "br")]})
    (\resp -> do
       putMVar var resp
       return ResponseReceived)
  resp <- takeMVar var
  r <- newIORef mempty
  case resp of
    ResponseStream stats hs f -> do
      f (\b -> modifyIORef r (<> b)) (return ())
      b <- readIORef r
      print (decompress (Builder.toLazyByteString b) == "Hello world\nGoodbye world")
    _ -> error "Not what we expected"
  putStrLn ""

  putStr "Don't encode non-textual content: "
  let app req respond = respond $ responseLBS status200 [("Content-Type", "application/binary")] bod
  var <- newEmptyMVar
  (brotli app)
    (defaultRequest {requestHeaders = [("Accept-Encoding", "br")]})
    (\resp -> do
       putMVar var resp
       return ResponseReceived)
  resp <- takeMVar var
  case resp of
    ResponseBuilder stats hs b -> do
      print (Builder.toLazyByteString b == bod)
    _ -> error "Not what we expected"
  putStrLn ""

  putStr "Don't encode if content length is too low: "
  let app req respond = respond $ responseLBS status200 [("Content-Type", "application/json"), ("Content-Length", "2")] "{}"
  var <- newEmptyMVar
  (brotli app)
    (defaultRequest {requestHeaders = [("Accept-Encoding", "br")]})
    (\resp -> do
       putMVar var resp
       return ResponseReceived)
  resp <- takeMVar var
  case resp of
    ResponseBuilder stats hs b -> do
      print (Builder.toLazyByteString b == "{}")
    _ -> error "Not what we expected"
  putStrLn ""

  putStr "Do encode if content length is long enough: "
  let app req respond = respond $ responseLBS status200 [("Content-Type", "text/whatever"), ("Content-Length", B.pack $ show $ L.length bod)] bod
  var <- newEmptyMVar
  (brotli app)
    (defaultRequest {requestHeaders = [("Accept-Encoding", "br")]})
    (\resp -> do
       putMVar var resp
       return ResponseReceived)
  resp <- takeMVar var
  case resp of
    ResponseBuilder stats hs b -> do
      print (decompress (Builder.toLazyByteString b) == bod)
    _ -> error "Not what we expected"
  putStrLn ""

  putStrLn "Compress fallback response for responseRaw if appropriate: Pending"
  putStrLn "Don't compress files if set to ignore: Pending"
  onTheFlyFileCompressTest
  precompressedTest
  putStrLn "Create cached file versions in given folder if appropriate: Pending"

  putStrLn "Support flushing compressed data: Pending"
  putStrLn "Return appropriate error statuses for malformed compressed input: Pending"

onTheFlyFileCompressTest :: IO ()
onTheFlyFileCompressTest = do
  let settings = defaultSettings { brotliFilesBehavior = BrotliCompress, brotliMinimumSize = 5 }
  putStr "Perform on-the-fly compression of files if set to compress or requesting appropriate content range size: "
  let app req respond =
        respond $
        responseFile
          status200
          [ ("Content-Type", "text/whatever")
          ]
          "test/sample.txt"
          Nothing
  var <- newEmptyMVar
  (brotli' settings app)
    (defaultRequest {requestHeaders = [("Accept-Encoding", "br")]})
    (\resp -> do
       putMVar var resp
       return ResponseReceived)
  resp <- takeMVar var
  r <- newIORef mempty
  case resp of
    ResponseStream stats hs f -> do
      f (\b -> modifyIORef r (<> b)) (return ())
      b <- readIORef r
      print (decompress (Builder.toLazyByteString b) == "Hello world")
    _ -> error "Not what we expected"
  putStrLn ""

precompressedTest :: IO ()
precompressedTest = do
  let settings = defaultSettings { brotliFilesBehavior = BrotliPreCompressed BrotliIgnore }
  putStr "Serve precomputed compressed files if appropriate & they exist: "
  let app req respond =
        respond $
        responseFile
          status200
          [ ("Content-Type", "text/whatever")
          ]
          "test/words"
          Nothing
  var <- newEmptyMVar
  (brotli' settings app)
    (defaultRequest {requestHeaders = [("Accept-Encoding", "br")]})
    (\resp -> do
       putMVar var resp
       return ResponseReceived)
  resp <- takeMVar var
  case resp of
    ResponseFile stats hs fp Nothing -> do
      print (fp == "test/words.br")
    _ -> error "Not what we expected"
  putStrLn ""
