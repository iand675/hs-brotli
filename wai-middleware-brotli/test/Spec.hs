{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Types
import Codec.Compression.Brotli (decompress, compress)
import Control.Monad.Trans
import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as Builder
import Data.IORef
import Data.Monoid
import Network.Wai
import Network.Wai.Middleware.Brotli
import Network.Wai.Internal
import Network.Wai.Test
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import System.IO

main :: IO ()
main = do
  putStrLn ""

  bod <- L.readFile "/usr/share/dict/words" -- "Hello World 000000000000000000000000000000"
  let app req respond = respond $ responseLBS status200 [("Content-Type", "text/whatever")] bod

  specs <- testSpecs $ parallel $ do

    it "Encodes text" $ do
      -- app :: Application
      putStr "Encoding text: "
      var <- newEmptyMVar
      (brotli defaultSettings app)
        (defaultRequest {requestHeaders = [("Accept-Encoding", "br")]})
        (\resp -> do
          putMVar var resp
          return ResponseReceived)
      resp <- takeMVar var
      case resp of
        ResponseBuilder stats hs b -> do
          assert (decompress (Builder.toLazyByteString b) == bod)
        _ -> error "Not what we expected"

    it "Does not encode if brotli not in Accept-Encoding header" $ do
      var <- newEmptyMVar
      (brotli defaultSettings app)
        (defaultRequest {requestHeaders = [("Accept-Encoding", "gzip"), ("Content-Type", "text/whatever")]})
        (\resp -> do
          putMVar var resp
          return ResponseReceived)
      resp <- takeMVar var
      case resp of
        ResponseBuilder stats hs b -> do
          assert (Builder.toLazyByteString b == bod)
        _ -> error "Not what we expected"

    it "Decode request body if it's encoded" $ do
      var <- newEmptyMVar
      let reqWithBody =
            defaultRequest
            { requestMethod = "POST"
            , requestHeaders = [("Content-Encoding", "br")]
            , requestBody =
                return $ L.toStrict $ compress ("Hello World" :: L.ByteString)
            }
      (brotli defaultSettings $ \req resp -> do
        reqBod <- requestBody req
        resp $ responseLBS status200 [] (L.fromStrict reqBod))
        reqWithBody
        (\resp -> do
          putMVar var resp
          return ResponseReceived)
      resp <- takeMVar var
      case resp of
        ResponseBuilder stats hs b ->
          assert (Builder.toLazyByteString b == "Hello World")
        _ -> error "Not what we expected"

    it "Streaming response body encoding" $ do
      var <- newEmptyMVar
      (brotli defaultSettings $ \req respond -> do
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
          assert (decompress (Builder.toLazyByteString b) == "Hello world\nGoodbye world")
        _ -> error "Not what we expected"

    it "Don't encode non-textual content" $ do
      let app req respond = respond $ responseLBS status200 [("Content-Type", "application/binary")] bod
      var <- newEmptyMVar
      (brotli defaultSettings app)
        (defaultRequest {requestHeaders = [("Accept-Encoding", "br")]})
        (\resp -> do
          putMVar var resp
          return ResponseReceived)
      resp <- takeMVar var
      case resp of
        ResponseBuilder stats hs b -> do
          assert (Builder.toLazyByteString b == bod)
        _ -> error "Not what we expected"

    it "Don't encode if content length is too low" $ do
      let app req respond = respond $ responseLBS status200 [("Content-Type", "application/json"), ("Content-Length", "2")] "{}"
      var <- newEmptyMVar
      (brotli defaultSettings app)
        (defaultRequest {requestHeaders = [("Accept-Encoding", "br")]})
        (\resp -> do
          putMVar var resp
          return ResponseReceived)
      resp <- takeMVar var
      case resp of
        ResponseBuilder stats hs b -> do
          assert (Builder.toLazyByteString b == "{}")
        _ -> error "Not what we expected"

    it "Do encode if content length is long enough" $ do
      let app req respond = respond $ responseLBS status200 [("Content-Type", "text/whatever"), ("Content-Length", B.pack $ show $ L.length bod)] bod
      var <- newEmptyMVar
      (brotli defaultSettings app)
        (defaultRequest {requestHeaders = [("Accept-Encoding", "br")]})
        (\resp -> do
          putMVar var resp
          return ResponseReceived)
      resp <- takeMVar var
      case resp of
        ResponseBuilder stats hs b -> do
          assert (decompress (Builder.toLazyByteString b) == bod)
        _ -> error "Not what we expected"

    it "Performs on-the-fly compression of files if set to compress or requesting appropriate content range size" onTheFlyFileCompressTest
    it "Serves precomputed compressed files if appropriate & they exist" precompressedTest

  defaultMain $ testGroup "Tests" specs


  {-

  putStrLn ""

  putStrLn ""

  putStr ": "
  putStrLn ""

  putStrLn ""

  putStrLn "Compress fallback response for responseRaw if appropriate: Pending"
  putStrLn "Don't compress files if set to ignore: Pending"
  onTheFlyFileCompressTest
  precompressedTest
  putStrLn "Create cached file versions in given folder if appropriate: Pending"

  putStrLn "Support flushing compressed data: Pending"
  putStrLn "Return appropriate error statuses for malformed compressed input: Pending"
-}

onTheFlyFileCompressTest :: IO ()
onTheFlyFileCompressTest = do
  let settings = defaultSettings { brotliFilesBehavior = BrotliCompress, brotliMinimumSize = 5 }
  let app req respond =
        respond $
        responseFile
          status200
          [ ("Content-Type", "text/whatever")
          ]
          "test/sample.txt"
          Nothing
  runSession session $ brotli settings app
  where
    session = do
     resp <- request (defaultRequest {requestHeaders = [("Accept-Encoding", "br")]})
     liftIO $ assert (decompress (simpleBody resp) @=? "Hello world\n")

precompressedTest :: IO ()
precompressedTest = do
  let settings = defaultSettings { brotliFilesBehavior = BrotliPreCompressed BrotliIgnore }
  let app req respond =
        respond $
        responseFile
          status200
          [ ("Content-Type", "text/whatever")
          ]
          "test/words"
          Nothing
  var <- newEmptyMVar
  (brotli settings app)
    (defaultRequest {requestHeaders = [("Accept-Encoding", "br")]})
    (\resp -> do
       putMVar var resp
       return ResponseReceived)
  resp <- takeMVar var
  case resp of
    ResponseFile stats hs fp Nothing -> do
      assert (fp == "test/words.br")
    _ -> error "Not what we expected"
