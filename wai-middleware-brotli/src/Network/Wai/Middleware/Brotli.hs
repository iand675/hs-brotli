{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Brotli
  (brotli
  ) where
import Data.Binary.Builder (toLazyByteString, fromLazyByteString)
import qualified Data.ByteString.Char8 as B
import Codec.Compression.Brotli
import Data.IORef
import Data.Monoid
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Internal

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
             return $ req { requestHeaders = ("Content-Encoding", B.intercalate ", " encs) : hs
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
brotli app req sendResponse = do
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
      ResponseFile {} -> sendResponse res
      (ResponseBuilder status hs b) ->
        if isBrotliResp
          then do
            let compressedResp = compress $ toLazyByteString b
            sendResponse $
              responseLBS status (addBrotliToContentEnc hs) compressedResp
          else sendResponse res
      (ResponseStream status hs f) ->
        sendResponse $
        ResponseStream status (addBrotliToContentEnc hs) $ \send flush ->
          f (send . fromLazyByteString . compress . toLazyByteString) flush
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
