{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as CL
import Codec.Compression.Brotli
import Codec.Compression.Brotli.Internal
import Test.QuickCheck
import Test.QuickCheck.Instances

main :: IO ()
main = do
  putStrLn ""
  {-
  quickCheck $ \bs ->
    let cbs = (compress (bs :: B.ByteString)) :: B.ByteString
        dbs = (decompress cbs) :: B.ByteString
    in dbs == bs
  -}
  quickCheck $ \bs ->
    let cbs = (compress (bs :: L.ByteString)) :: L.ByteString
        dbs = (decompress cbs) :: L.ByteString
    in dbs == bs

  sampleRoundTrip ";"
  sampleRoundTrip "\139\NUL\128\SOH\NUL\ETX"
  sampleRoundTrip ""
  sampleRoundTrip "What you need is an eclipse. However being a tidally locked planet you're not going to have a moon, at least your people would have been idiots for settling on a tidally locked planet with a moon as it would be unstable as discussed in this question: "
  L.readFile "/usr/share/dict/words" >>= sampleRoundTrip
  longReallyCompressable
  {-
  needsOutputL
  let comped = compress f
  print (L.length f, L.length comped, map B.length $ L.toChunks comped)
  -}
  -- print (compress str :: ByteString)
  -- print (compress lstr :: L.ByteString)
  (Consume c) <- compressor defaultCompressionSettings
  putStrLn "Got consumer"
  let bs = B.replicate (2 ^ 8) 0
  (Consume c') <- c bs
  putStrLn "Still consuming"
  (Produce r f) <- c' ""
  putStrLn "Done, so should start producing"
  Done <- f
  putStrLn "Yup, hit the end"
  print (CL.fromStrict bs == decompress (CL.fromStrict r))

sampleRoundTrip :: L.ByteString -> IO ()
sampleRoundTrip l = do
  let rt = decompress $ compress l
  print (L.toStrict l == L.toStrict rt)

sample :: IO ()
sample = do
  putStrLn "Creating encoder"
  enc <- createEncoder
  print enc
  putStrLn "Destroying encoder"
  destroyEncoder enc
  encoderVersion >>= print

longReallyCompressable :: IO ()
longReallyCompressable = sampleRoundTrip (L.replicate (2 ^ 22) 0)
