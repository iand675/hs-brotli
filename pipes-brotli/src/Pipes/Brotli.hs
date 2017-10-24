module Pipes.Brotli
    ( compress
    , compress'
    ) where

import Codec.Compression.Brotli hiding (compress, decompress)
import Control.Monad.Trans
import Data.ByteString as B
import Pipes

compress :: MonadIO m => Pipe B.ByteString B.ByteString m ()
compress = compress' defaultCompressionSettings

compress' :: MonadIO m => CompressionSettings -> Pipe B.ByteString B.ByteString m ()
compress' settings = do
  c <- liftIO $ compressor settings
  go c
  where
    go c = case c of
      Produce bs next -> do
        yield bs
        c' <- liftIO next
        go c'
      Consume f -> do
        bs <- await
        c' <- liftIO $ f bs
        go c'
      Error -> error "TODO"
      Done -> return ()

decompress :: MonadIO m => Pipe B.ByteString B.ByteString m ()
decompress = do
  c <- liftIO decompressor
  go c
  where
    go c = case c of
      Produce bs next -> do
        yield bs
        c' <- liftIO next
        go c'
      Consume f -> do
        bs <- await
        c' <- liftIO $ f bs
        go c'
      Error -> error "TODO"
      Done -> return ()
