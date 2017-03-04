{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Brotli where
import Data.Binary.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as B
import Codec.Compression.Brotli
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

canUseBrotli :: [B.ByteString] -> Bool
canUseBrotli = any (== "br")

brotliEncodeMiddleware :: Middleware
brotliEncodeMiddleware app req sendResponse = app req $ \res ->
  case res of
    -- TODO support caching files
    ResponseFile{} -> sendResponse res
    (ResponseBuilder status hs b) -> if isBrotliResp
      then do
        putStrLn "Tryna encode"
        let compressedResp = compress $ toLazyByteString b
        print compressedResp
        sendResponse $ responseLBS status ((hContentEncoding, "br") : hs) compressedResp
      else sendResponse res
    ResponseStream{} -> sendResponse res
    ResponseRaw{} -> sendResponse res
  where
    (mEncs, reqWithoutEnc) = case pluckHeader hAcceptEncoding $ requestHeaders req of
      (mEncs, hs) -> (mEncs, req { requestHeaders = hs })
    isBrotliResp = case mEncs of
      Nothing -> False
      Just ebs -> canUseBrotli $ splitCommas ebs
