module Data.Conduit.Brotli
    ( compress
    , compress'
    , Codec.Compression.Brotli.Chunk(..)
    , decompress
    , decompress'
    ) where

import Codec.Compression.Brotli hiding (compress, decompress)
import Control.Monad.Trans
import qualified Data.ByteString as B
import Data.Conduit

compress :: MonadIO m => Conduit Codec.Compression.Brotli.Chunk m B.ByteString
compress = compress' defaultCompressionSettings

compress' :: MonadIO m => CompressionSettings -> Conduit Codec.Compression.Brotli.Chunk m B.ByteString
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
        mres <- await
        case mres of
          Nothing -> do
            c' <- liftIO $ f $ Codec.Compression.Brotli.Chunk B.empty
            go c'
          Just chnk -> case chnk of
            Codec.Compression.Brotli.Chunk bs -> if B.null bs
              then go c
              else do
                c' <- liftIO $ f chnk
                go c'
            Codec.Compression.Brotli.Flush -> do
              c' <- liftIO $ f chnk
              go c'

      Error -> error "TODO"
      Done -> return ()

decompress :: MonadIO m => Conduit B.ByteString m B.ByteString
decompress = decompress'

decompress' :: MonadIO m => Conduit B.ByteString m B.ByteString
decompress' = do
  dc <- liftIO decompressor
  go dc
  where
    go dc = case dc of
      Produce bs next -> do
        yield bs
        c' <- liftIO next
        go c'
      Consume f -> do
        mres <- await
        case mres of
          Nothing -> do
            c' <- liftIO $ f B.empty
            go c'
          Just bs -> if B.null bs
            then go dc
            else do
              c' <- liftIO $ f bs
              go c'
      Error -> error "TODO"
      Done -> return ()
