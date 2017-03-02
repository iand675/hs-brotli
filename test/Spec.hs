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
  bs <- L.readFile "/usr/share/dict/words"
  print $ L.take 1 $ compress bs
  {-
  quickCheck $ \bs ->
    let cbs = (compress (bs :: B.ByteString)) :: B.ByteString
        dbs = (decompress cbs) :: B.ByteString
    in dbs == bs
  quickCheck $ \bs ->
    let cbs = (compress (bs :: L.ByteString)) :: L.ByteString
        dbs = (decompress cbs) :: L.ByteString
    in dbs == bs
  -}
  {-
  needsOutputL
  let comped = compress f
  print (L.length f, L.length comped, map B.length $ L.toChunks comped)
  -}
  -- print (compress str :: ByteString)
  -- print (compress lstr :: L.ByteString)
  {-
  enc <- createEncoder
  res <- stream enc str
  finish <- finishStream enc
  print res
  print finish
  destroyEncoder enc
  -}

sample :: IO ()
sample = do
  putStrLn "Creating encoder"
  enc <- createEncoder
  print enc
  putStrLn "Destroying encoder"
  destroyEncoder enc
  encoderVersion >>= print

needsOutputL = print (L.length bs, L.length dbs)
  where
    bs = L.replicate (2 ^ 12) 0
    cbs = (compress bs) :: L.ByteString
    dbs = (decompress cbs) :: L.ByteString
