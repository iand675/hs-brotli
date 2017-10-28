{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
---------------------------------------------------------
-- |
-- Module        : Network.Wai.Middleware.Brotli
-- Copyright     : Ian Duncan
-- License       : BSD3
--
-- Maintainer    : Ian Duncan <ian@iankduncan.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Automatic brotli compression of responses.
--
-- If you are using this middleware with wai-extra's @gzip@ middleware,
-- it is important that @brotli@ wraps your application before gzip does
-- or your responses will be compressed by both, which is not beneficial.
--
-- Correct:
--
-- > gzip def . brotli defaultSettings
--
-- Incorrect:
--
-- > brotli defaultSettings . gzip def
---------------------------------------------------------
module Network.Wai.Middleware.Brotli
  ( brotli
  , defaultSettings
  , BrotliSettings(..)
  , defaultShouldCompress
  , BrotliFiles(..)
  ) where

import Codec.Compression.Brotli
import Control.Exception
import Control.Monad
import Data.Binary.Builder (toLazyByteString, fromLazyByteString, fromByteString, Builder)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Data.Maybe
import Data.Monoid
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Internal
import System.Directory
import System.FilePath
import System.IO
import System.Posix

data BrotliFiles
  = BrotliIgnore -- ^ Do not compress file responses
  | BrotliCompress
    -- ^ Compress files on the fly. Note that this may counteract zero-copy
    -- response optimizations on some platforms.
  | BrotliCacheFolder FilePath
    -- ^ Compress files, caching them in the specified directory. Note that
    -- changes to the original files will not invalidate existing cached files,
    -- so it is important to clear the cache directory appropriately if the
    -- original file has changed
  | BrotliPreCompressed BrotliFiles
    -- ^ Look for the original file, only with the ".br" extension appended to it. Will fall back
    -- to the provided file setting if the file doesn't exist.
  deriving (Read, Eq, Show)

data BrotliSettings = BrotliSettings
  { brotliCompressionSettings :: Request -> Response -> CompressionSettings
  , brotliMinimumSize :: Int
  , brotliFilesBehavior :: BrotliFiles
  , brotliShouldCompress :: BrotliSettings -> Request -> Response -> Bool
  , brotliMimePrefixes :: [B.ByteString]
  , brotliMimeSuffixes :: [B.ByteString]
  }

-- | It is recommended that you combine your custom logic around
-- deciding to compress with this function, such as:
--
-- > \settings req resp -> defaultShouldCompress settings req resp && customPredicate
defaultShouldCompress :: BrotliSettings -> Request -> Response -> Bool
defaultShouldCompress settings req resp =
  let (mclb, hs) = pluckHeader hContentLength (responseHeaders resp)
      (mctb, hs') = pluckHeader hContentType hs
  in case mctb of
       Nothing -> False
       Just ctb ->
         case mclb of
           Nothing -> checkMime settings ctb
           Just clb ->
             (fromMaybe True $ do
                (x, rest) <- B.readInt clb
                return $
                  if x >= brotliMinimumSize settings
                    then True
                    else False) &&
             checkMime settings ctb

checkMime :: BrotliSettings -> B.ByteString -> Bool
checkMime settings ctb =
  any (\b -> b `B.isPrefixOf` ctb) (brotliMimePrefixes settings) ||
  any (\b -> b `B.isSuffixOf` ctb) (brotliMimeSuffixes settings)

-- A sane set of starting defaults. Tuned to use faster / better compression
-- than GZip defaults for non-file responses. Compresses most common text-based formats,
-- skips compression on responses with known Content-Lengths that already fit
-- within one TCP packet.
--
-- Note that this configuration does *not* compress file responses. Customizing
-- @brotliFilesBehavior@ will use brotli's maximum compression quality by default, which is
-- quite slow (albeit achieves very good compression ratios). It is highly recommended that
-- you have an appropriate caching strategy to avoid compression of files on each request.
defaultSettings :: BrotliSettings
defaultSettings =
  BrotliSettings
    (\_ resp ->
       case resp of
         ResponseFile _ _ _ _ ->
           defaultCompressionSettings {compressionQuality = 11}
         _ -> defaultCompressionSettings {compressionQuality = 4})
    860
    BrotliIgnore
    defaultShouldCompress
    [ "text/"
    , "application/json"
    , "application/javascript"
    , "application/x-javascript"
    , "application/ecmascript"
    , "application/xml"
    , "application/x-font-ttf"
    , "image/x-icon"
    , "image/vnd.microsoft.icon"
    , "application/vnd.ms-fontobject"
    , "application/x-font-opentype"
    , "application/x-font-truetype"
    , "font/eot"
    , "font/otf"
    , "font/ttf"
    , "font/opentype"
    ]
    ["+json", "+xml"]


pluckHeader :: HeaderName -> [Header] -> (Maybe B.ByteString, [Header])
pluckHeader hName = foldr go (Nothing, [])
  where
    go h@(hKey, hVal) (mh, hs) = if hKey == hName
      then (Just hVal, hs)
      else (mh, h:hs)

splitCommas :: B.ByteString -> [B.ByteString]
splitCommas = B.split ',' . B.filter (/= ' ')

-- SDCH + brotli may be better together than separate, but gzip and brotli together aren't useful
canUseReqEncoding :: [B.ByteString] -> (Bool, [B.ByteString])
canUseReqEncoding = foldr go (False, [])
  where
    go :: B.ByteString -> (Bool, [B.ByteString]) -> (Bool, [B.ByteString])
    go bs (seenBr, encs) = if bs == "br"
      then (True, encs)
      else if seenBr && bs == "gzip"
        then (seenBr, encs)
        else (seenBr, bs:encs)

decodeRequestBody :: Request -> IO Request
decodeRequestBody req = do
  let (encoding, hs) = pluckHeader "Content-Encoding" $ requestHeaders req
  case encoding of
    Nothing -> return req
    Just enc ->
      let encs = splitCommas enc
          getBs = requestBody req
      in case encs of
           ("br":encs) -> do
             bodyDecompressor <- decomp getBs
             return $
               req
               { requestHeaders =
                   ("Content-Encoding", B.intercalate ", " encs) : hs
               , requestBody = bodyDecompressor
               }
           _ -> return req
  where
    decomp :: IO B.ByteString -> IO (IO B.ByteString)
    decomp popper = do
      dc <- decompressor
      ref <- newIORef dc
      return $ step ref
      where
        step ref = do
          c <- readIORef ref
          case c of
            Done -> return ""
            Consume f -> do
              bs <- popper
              r <- f bs
              writeIORef ref r
              step ref
            Produce bs act -> do
              r <- act
              writeIORef ref r
              return bs
            Error -> error "Brotli stream decoding error in request body"

-- | Use brotli to decompress request bodies & compress response bodies.
--
-- Analyzes the Accept-Encoding and Content-Type headers to determine if
-- brotli is supported.
brotli :: BrotliSettings -> Middleware
brotli settings app req sendResponse = do
  decodedReq <-
    decodeRequestBody $
    (reqWithoutBrotliAcceptEnc
     { requestHeaders =
         (hAcceptEncoding, B.intercalate ", " remainingReqEncs) :
         requestHeaders reqWithoutBrotliAcceptEnc
     })
  app decodedReq $ \res ->
    if isBrotliResp
      then wrapResponse settings req res sendResponse
      else sendResponse res
  where
    (mEncs, reqWithoutBrotliAcceptEnc) =
      case pluckHeader hAcceptEncoding $ requestHeaders req of
        (ms, hs) -> (ms, req {requestHeaders = hs})
    (isBrotliResp, remainingReqEncs) =
      case mEncs of
        Nothing -> (False, [])
        Just ebs -> canUseReqEncoding $ splitCommas ebs

fixHeaders :: [Header] -> [Header]
fixHeaders = filter (\(k, _) -> k /= hContentLength)

wrapResponse :: BrotliSettings -> Request -> Response -> (Response -> IO a) -> IO a
wrapResponse settings req res sendResponse =
  let shouldCompress = (brotliShouldCompress settings) settings req
  in if shouldCompress res
       then case res of
              ResponseFile status hs fp bp ->
                compressedFileResponse
                  settings
                  addBrotliToContentEnc
                  req
                  status
                  hs
                  fp
                  bp
                  sendResponse

              ResponseBuilder status hs b -> do
                let compressedResp =
                      compressWith
                        (brotliCompressionSettings settings req res)
                        (toLazyByteString b)
                sendResponse $
                  responseLBS status (addBrotliToContentEnc hs) compressedResp

              ResponseStream status hs f ->
                sendResponse $
                ResponseStream status (addBrotliToContentEnc hs) $ \send flush -> do
                  c <- compressor (brotliCompressionSettings settings req res)
                  ref <- newIORef c
                  f (compressSend send flush ref) (compressFlush send flush ref)
                  c' <- readIORef ref
                  case c' of
                    Consume consumer ->
                      consumer (Chunk B.empty) >>= finishStreaming send
                    Produce _ _ -> error "Shouldn't be producing right now"
                    Error -> error "Shouldn't be in an error state right now"
                    Done -> return ()

              ResponseRaw act fallback -> sendResponse $ responseRaw act fallback

       else sendResponse res
  where
    addBrotliToContentEnc hs =
      let (respEncs, hsWithoutBrotliEnc) = pluckHeader hContentEncoding hs
      in fixHeaders $
         ((hContentEncoding, maybe "br" (<> ", br") respEncs) :
          hsWithoutBrotliEnc)
    finishStreaming send c =
      case c of
        Consume _ -> error "Shouldn't be consuming right now"
        Produce b next ->
          send (fromByteString b) >> next >>= finishStreaming send
        Error -> error "Encountered error while finishing stream"
        Done -> return ()

compressSend :: (Builder -> IO ()) -> IO () -> IORef (BrotliStream Chunk) -> Builder -> IO ()
compressSend innerSend innerFlush st bs = do
  let steps = L.toChunks $ toLazyByteString bs

  action <- readIORef st
  action' <- foldM step action steps
  writeIORef st action'

  where
    step c bs = case c of
      Produce b n -> do
        innerSend $ fromByteString b
        r <- n
        step r bs
      Consume consumer -> consumer $ Chunk bs
      Error -> error "Streaming send of response body failed in brotli compression phase"
      Done -> error "Should not be done compressing while still sending data. Did you send an empty bytestring?"

performFlush :: (Builder -> IO ()) -> IO () -> BrotliStream Chunk -> IO (BrotliStream Chunk)
performFlush innerSend innerFlush c = case c of
  Consume consumer -> consumer Flush >>= performFlush'
  _ -> error "Shouldn't be flushing value when not in consumption state"
  where
    performFlush' c = case c of
      Produce b n -> do
        innerSend $ fromByteString b
        r <- n
        performFlush' r
      _ -> innerFlush >> pure c

compressFlush :: (Builder -> IO ()) -> IO () -> IORef (BrotliStream Chunk) -> IO ()
compressFlush innerSend innerFlush ref = do
  action <- readIORef ref
  r <- performFlush innerSend innerFlush action
  writeIORef ref r

compressedFileResponse ::
     BrotliSettings
  -> (ResponseHeaders -> ResponseHeaders)
  -> Request
  -> Status
  -> ResponseHeaders
  -> FilePath
  -> Maybe FilePart
  -> (Response -> IO a)
  -> IO a
compressedFileResponse settings addBrotliHeaders req status hs p mfp sendResponse =
  case mfp of
    Nothing ->
      case brotliFilesBehavior settings of
        BrotliIgnore -> sendResponse $ responseFile status hs p Nothing
        BrotliCompress -> do
          size <- fileSize <$> getFileStatus p
          if fromIntegral size >= brotliMinimumSize settings
            then do
              let (status', hs', f) =
                    responseToStream $ responseFile status hs p Nothing
              f $ \body ->
                wrapResponse
                  settings
                  req
                  (responseStream status' hs' body)
                  sendResponse
            else sendResponse $ responseFile status hs p Nothing
        BrotliCacheFolder cachePath
      -- TODO check file size here maybe
         -> do
          createDirectoryIfMissing True cachePath
          let normalized = normalizeOriginalFile p <.> "br"
              adjustedFile = (cachePath </> normalizeOriginalFile p <.> "br")
          exists <- doesFileExist adjustedFile
          when (not exists) $ do
            (tmpPath, h) <- openBinaryTempFile cachePath ("XXXXX" ++ normalized)
            (flip onException) (removeFile tmpPath) $ do
              original <- L.readFile p
              L.hPut h $ compressWith (brotliCompressionSettings settings req (responseFile status hs p mfp)) original 
              hClose h
              rename tmpPath adjustedFile
          sendResponse $
            responseFile status (addBrotliHeaders hs) adjustedFile Nothing
        BrotliPreCompressed fallback -> do
          let modifiedPath = p <.> "br"
          exists <- doesFileExist modifiedPath
          if exists
            then sendResponse $
                 responseFile status (addBrotliHeaders hs) modifiedPath Nothing
            else compressedFileResponse
                   (settings {brotliFilesBehavior = fallback})
                   addBrotliHeaders
                   req
                   status
                   hs
                   p
                   mfp
                   sendResponse
    _ -> do
      let (status', hs', f) = responseToStream $ responseFile status hs p mfp
      f $ \body -> do
        wrapResponse settings req (responseStream status' hs' body) sendResponse

normalizeOriginalFile :: FilePath -> FilePath
normalizeOriginalFile file = map safe file
  where
    safe c
        | 'A' <= c && c <= 'Z' = c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
    safe '-' = '-'
    safe '_' = '_'
    safe '.' = '.'
    safe _ = '_'

