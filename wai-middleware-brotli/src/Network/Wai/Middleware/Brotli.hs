{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Brotli
  ( brotli
  , brotli'
  , defaultSettings
  , BrotliSettings(..)
  , defaultShouldCompress
  , BrotliFiles(..)
  ) where

import Codec.Compression.Brotli
import Control.Monad
import Data.Binary.Builder (toLazyByteString, fromLazyByteString, fromByteString, Builder)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Data.Maybe
import Data.Monoid
import qualified Data.Trie as Trie
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Internal

data BrotliFiles
  = BrotliIgnore
  | BrotliCompress
  | BrotliCacheFolder FilePath
  | BrotliPreCompressed BrotliFiles
  deriving (Read, Eq, Show)

data BrotliSettings = BrotliSettings
  { brotliCompressionSettings :: !CompressionSettings
  , brotliMinimumSize :: Int
  , brotliFilesBehavior :: BrotliFiles
  , brotliShouldCompress :: BrotliSettings -> Request -> Response -> Bool
  , brotliMimePrefixes :: [B.ByteString]
  , brotliMimeSuffixes :: [B.ByteString]
  }

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

defaultSettings :: BrotliSettings
defaultSettings = BrotliSettings defaultCompressionSettings 860 BrotliIgnore defaultShouldCompress
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
  [ "+json"
  , "+xml"
  ]


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

brotli :: Middleware
brotli = brotli' defaultSettings

brotli' :: BrotliSettings -> Middleware
brotli' settings app req sendResponse = do
  let shouldCompress = (brotliShouldCompress settings) settings req
  decodedReq <-
    decodeRequestBody $
    (reqWithoutBrotliAcceptEnc
     { requestHeaders =
         (hAcceptEncoding, B.intercalate ", " remainingReqEncs) :
         requestHeaders reqWithoutBrotliAcceptEnc
     })
  -- TODO support caching files
  app decodedReq $ \res ->
    case res of
      ResponseFile status hs fp bp -> case brotliFilesBehavior settings of
        BrotliIgnore -> sendResponse res
      (ResponseBuilder status hs b) ->
        if isBrotliResp && shouldCompress res
          then do
            let compressedResp =
                  compressWith
                    (toLazyByteString b)
                    (brotliCompressionSettings settings)
            sendResponse $
              responseLBS
                status
                (addBrotliToContentEnc $ fixHeaders hs)
                compressedResp
          else sendResponse res
      (ResponseStream status hs f) ->
        if isBrotliResp && shouldCompress res
        then sendResponse $
          ResponseStream status (addBrotliToContentEnc $ fixHeaders hs) $ \send flush -> do
            c <- compressor (brotliCompressionSettings settings)
            ref <- newIORef (c, False)
            f (compressSend send flush ref) (compressFlush ref)
            (c', shouldFlush) <- readIORef ref
            when shouldFlush flush
            case c' of
              Consume consumer -> consumer B.empty >>= finishStreaming send
              Produce _ _ -> error "Shouldn't be producing right now"
              Error -> error "Shouldn't be in an error state right now"
              Done -> return ()
        else sendResponse res
      ResponseRaw {} -> sendResponse res
  where
    (mEncs, reqWithoutBrotliAcceptEnc) =
      case pluckHeader hAcceptEncoding $ requestHeaders req of
        (ms, hs) -> (ms, req {requestHeaders = hs})
    addBrotliToContentEnc hs =
      let (respEncs, hsWithoutBrotliEnc) = pluckHeader hContentEncoding hs
      in ((hContentEncoding, maybe "br" (<> ", br") respEncs) :
          hsWithoutBrotliEnc)
    (isBrotliResp, remainingReqEncs) =
      case mEncs of
        Nothing -> (False, [])
        Just ebs -> canUseReqEncoding $ splitCommas ebs
    finishStreaming send c =
      case c of
        Consume _ -> error "Shouldn't be consuming right now"
        Produce b next ->
          send (fromByteString b) >> next >>= finishStreaming send
        Error -> error "Encountered error while finishing stream"
        Done -> return ()
    prefixTree = Trie.fromList $ zip (brotliMimePrefixes settings) (repeat ())

-- TODO add to middleware
fixHeaders :: [Header] -> [Header]
fixHeaders = filter (\(k, _) -> k /= hContentLength)

compressSend :: (Builder -> IO ()) -> IO () -> IORef (Compressor, Bool) -> Builder -> IO ()
compressSend innerSend innerFlush st bs = do
  let steps = L.toChunks $ toLazyByteString bs
  mapM_ (step st) steps
  where
    step st bs = do
      -- TODO this needs to produce until a consume is hit
      (c, shouldFlush) <- readIORef st
      case c of
        Produce b n -> do
          innerSend $ fromByteString b
          when shouldFlush $ innerFlush
          r <- n
          writeIORef st (r, False)
        Consume consumer -> do
          r <- consumer bs
          writeIORef st (r, shouldFlush)
        Error -> error "Streaming send of response body failed in brotli compression phase"
        Done -> error "Should not be done compressing while still sending data. Did you send an empty bytestring?"


compressFlush :: IORef (Compressor, Bool) -> IO ()
compressFlush ref = modifyIORef ref $ \(c, b) -> (c, True)
