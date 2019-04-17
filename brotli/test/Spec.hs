{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as CL
import Codec.Compression.Brotli
import Codec.Compression.Brotli.Internal
import System.IO.Error
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances

fastSettings :: CompressionSettings
fastSettings = defaultCompressionSettings { compressionQuality = 4 }

main :: IO ()
main = do
  putStrLn ""
  defaultMain $ testGroup "Tests"
    [ testProperty "Round-trip" $ \bs ->
        let cbs = (compressWith fastSettings (bs :: L.ByteString)) :: L.ByteString
            dbs = (decompress cbs) :: L.ByteString
        in dbs == bs
    , testCase "Semicolon" $ sampleRoundTrip ";"
    , testCase (show "\139\NUL\128\SOH\NUL\ETX") $ sampleRoundTrip "\139\NUL\128\SOH\NUL\ETX"
    , testCase "Empty string" $ sampleRoundTrip ""
    , testCase "Paragraph" $ sampleRoundTrip "What you need is an eclipse. However being a tidally locked planet you're not going to have a moon, at least your people would have been idiots for settling on a tidally locked planet with a moon as it would be unstable as discussed in this question: "
    , testCase "The dictionary" $ do
        catchIOError (L.readFile "/usr/share/dict/words") (const $ pure "") >>= sampleRoundTrip
    , testCase "Long, really compressable" longReallyCompressable
    , testCase "Streaming" $ do
        (Consume c) <- compressor fastSettings
        -- putStrLn "Got consumer"
        -- bs <- B.readFile "/usr/share/dict/words" -- B.replicate (2 ^ 18) 0
        -- putStrLn "Feed once"
        (Consume c) <- c $ Chunk "Hello "
        -- putStrLn "Flush"
        (Produce compressedPt1 followup) <- c Flush
        -- putStrLn "Got flush triggered produce"
        (Consume c) <- followup
        -- putStrLn "Back to consuming"
        (Consume c) <- c $ Chunk "World"
        -- putStrLn "Fed it some more"
        (Produce compressedPt2 followup) <- c $ Chunk ""
        -- putStrLn "Done, so should signal that now"
        Done <- followup
        -- putStrLn "Yup, hit the end"
        "Hello World" @?= decompress (CL.fromStrict (compressedPt1 `mappend` compressedPt2))
    ]
  {-
  quickCheck $ \bs ->
    let cbs = (compress (bs :: B.ByteString)) :: B.ByteString
        dbs = (decompress cbs) :: B.ByteString
    in dbs == bs
  -}

  {-
  sampleRoundTrip ";"
  sampleRoundTrip "\139\NUL\128\SOH\NUL\ETX"
  sampleRoundTrip ""
  sampleRoundTrip "What you need is an eclipse. However being a tidally locked planet you're not going to have a moon, at least your people would have been idiots for settling on a tidally locked planet with a moon as it would be unstable as discussed in this question: "
  L.readFile "/usr/share/dict/words" >>= sampleRoundTrip
  longReallyCompressable
-}
  {-
  needsOutputL
  let comped = compress f
  print (L.length f, L.length comped, map B.length $ L.toChunks comped)
  -}
  -- print (compress str :: ByteString)
  -- print (compress lstr :: L.ByteString)
  --
{-
-}
  -- threadDelay 10000000

sampleRoundTrip :: L.ByteString -> IO ()
sampleRoundTrip l = do
  let rt = decompress $ compressWith fastSettings l
  L.toStrict l @?= L.toStrict rt

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
