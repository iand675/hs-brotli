{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MagicHash              #-}
module Codec.Compression.Brotli where
import Control.Monad (when, unless)
import Control.Exception (bracket)
import qualified Codec.Compression.Brotli.Internal as I
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI
import Data.IORef
import Data.Int
import Foreign.C
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Marshal (alloca, allocaBytes)
import Foreign.Storable (peek, poke)
import GHC.Int
import GHC.Types
import System.IO.Unsafe

class Compress a b where
  compress :: a -> b

instance Compress B.ByteString B.ByteString where
  compress b = unsafePerformIO $ bracket I.createEncoder I.destroyEncoder $ \inst -> do
    let estBufSize = fromIntegral $ I.maxCompressedSize $ fromIntegral $ B.length b
    res <- alloca $ \outSize -> do
      poke outSize estBufSize
      BI.createAndTrim (fromIntegral estBufSize) $ \outBuf -> B.unsafeUseAsCStringLen b $ \(inPtr, inLen) -> do

        ok <- I.encoderCompress I.defaultQuality I.defaultWindow I.defaultMode (fromIntegral inLen) (castPtr inPtr) outSize outBuf
        os <- peek outSize
        if (ok /= 1)
          then error "Compression error or output buffer is too small"
          else return $ fromIntegral os
    return res

isTrue :: CInt -> Bool
isTrue (CInt (I32# x)) = isTrue# x

maxCompressedSize :: Int -> Int
maxCompressedSize = fromIntegral . I.maxCompressedSize . fromIntegral

hasMoreOutput :: I.BrotliEncoderState -> IO Bool
hasMoreOutput = fmap isTrue . I.encoderHasMoreOutput

isFinished :: I.BrotliEncoderState -> IO Bool
isFinished = fmap isTrue . I.encoderIsFinished

takeOutput :: I.BrotliEncoderState -> IO B.ByteString
takeOutput st = alloca $ \sizeP -> do
  poke sizeP 0
  ptr <- I.encoderTakeOutput st sizeP
  takeSize <- peek sizeP
  B.packCStringLen (castPtr ptr, fromIntegral takeSize)

data FeedResponse = FeedResponse
  { unusedInput :: !B.ByteString
  , availableOutput :: !B.ByteString
  , totalProduced :: !Int
  } deriving (Show)

stream :: I.BrotliEncoderState -> B.ByteString -> IO FeedResponse
stream st bs = alloca $ \availableIn -> alloca $ \nextInP -> alloca $ \availableOut -> alloca $ \nextOutP -> alloca $ \totalOut -> B.unsafeUseAsCStringLen bs $ \(bsP, len) -> do
  poke availableIn (fromIntegral len)
  poke nextInP (castPtr bsP)
  poke availableOut $ fromIntegral LI.smallChunkSize
  outBs <- BI.createUptoN LI.smallChunkSize $ \outP -> do
    poke nextOutP outP
    res <- isTrue <$> I.encoderCompressStream st I.encoderOperationProcess availableIn nextInP availableOut nextOutP totalOut
    unless res $ error "Unknown stream encoding failure"
    outCount <- peek availableOut
    return $ LI.smallChunkSize - fromIntegral outCount
  unconsumedBytesCount <- peek availableIn
  unconsumedBytesP <- peek nextInP
  remainingInput <- B.packCStringLen (castPtr unconsumedBytesP, fromIntegral unconsumedBytesCount)
  avail <- peek availableOut
  total <- fromIntegral <$> peek totalOut
  return $ FeedResponse remainingInput outBs total

-- | Note that this should be called until returned bytestring is empty. Once is not enough.
finishStream :: I.BrotliEncoderState -> IO (B.ByteString, Int)
finishStream st = alloca $ \availableIn -> alloca $ \nextInP -> alloca $ \availableOut -> alloca $ \nextOutP -> alloca $ \totalOut -> do
  poke availableIn 0
  poke nextInP nullPtr
  poke availableOut $ fromIntegral LI.smallChunkSize
  outBs <- BI.createUptoN LI.smallChunkSize $ \outP -> do
    poke nextOutP outP
    res <- isTrue <$> I.encoderCompressStream st I.encoderOperationFinish availableIn nextInP availableOut nextOutP totalOut
    unless res $ error "Unknown stream encoding failure"
    outCount <- peek availableOut
    return $ LI.smallChunkSize - fromIntegral outCount
  total <- fromIntegral <$> peek totalOut
  return (outBs, total)


{-
consume :: I.BrotliEncoderState -> IO (Maybe ByteString)
consume st = do
  isMore <- hasMoreOutput st
  if isMore
    then Just <$> takeOutput st
    else return Nothing
-}

-- TODO properly finalize encoder
instance Compress L.ByteString L.ByteString where
  compress b = unsafePerformIO $ do
    inst <- I.createEncoder
    lazyCompress inst B.empty b
    where
      lazyCompress st previous c = unsafeInterleaveIO $ readChunks st previous c
      lazyFinish st = unsafeInterleaveIO $ do
        putStrLn "Demanding compressed finish chunk"
        (res, _) <- finishStream st
        if B.null res
          then return LI.Empty
          else do
            cs <- lazyFinish st
            return $ LI.Chunk res cs
      -- TODO previous can be replaced with raw ptr?
      readChunks st previous c = do
        putStrLn "Demanding compressed chunk"
        if B.null previous
          then case c of
            LI.Chunk bs next -> do
              fr <- stream st bs
              if B.null $ availableOutput fr
                then lazyCompress st (unusedInput fr) next
                else do
                  rest <- lazyCompress st (unusedInput fr) next
                  return $ LI.Chunk (availableOutput fr) rest
            LI.Empty -> lazyFinish st
          else do
            fr <- stream st previous
            if B.null $ availableOutput fr
              then lazyCompress st (unusedInput fr) c
              else do
                rest <- lazyCompress st (unusedInput fr) c
                return $ LI.Chunk (availableOutput fr) rest

{-
instance Decompress B.ByteString B.ByteString where
  decompress b = unsafePerformIO $ alloca $ \outBufSizeP -> do
    let outBufferLen = 2 * B.length b -- TODO this isn't sufficient. Should retry with exponentially larger buffers or something
    poke outBufSizeP (fromIntegral outBufferLen)
    err <- newIORef Nothing
    bs <- BI.createUptoN outBufferLen $ \outP -> do
      B.unsafeUseAsCStringLen b $ \(cstr, len) -> do
        status <- I.decoderDecompress (fromIntegral len) (castPtr cstr) outBufSizeP outP
        when (status /= 1) $ writeIORef err (Just status)
        fromIntegral <$> peek outBufSizeP
    wasErr <- readIORef err
    return $ case wasErr of
      Nothing -> bs
      Just e -> error (show e)
-}

data DecompressParams = DecompressParams

defaultDecompressParams :: DecompressParams
defaultDecompressParams = DecompressParams

decompress :: L.ByteString -> L.ByteString
decompress = (flip decompressWith) defaultDecompressParams

decompressWith :: L.ByteString -> DecompressParams -> L.ByteString
decompressWith b _ = unsafePerformIO $ do
  st <- I.createDecoder
  lazyDecompress st b
  where
    lazyDecompress st rest = unsafeInterleaveIO $ writeChunks st rest
    writeChunks st lbs = do
      putStrLn "Demanding decompressed chunk"
      case lbs of
        LI.Chunk bs rest -> alloca $ \inputLenP -> alloca $ \inputP -> alloca $ \outputLen -> alloca $ \outputPtr -> do
          all@(res, unconsumed', decompressed) <- B.unsafeUseAsCStringLen bs $ \(strP, strLen) -> do
            poke inputLenP (fromIntegral strLen)
            poke inputP (castPtr strP)
            res <- newIORef 0
            decompressed <- BI.createUptoN LI.smallChunkSize $ \outP -> do
              poke outputLen (fromIntegral LI.smallChunkSize)
              poke outputPtr (castPtr outP)
              writeIORef res =<< I.decoderDecompressStream st inputLenP inputP outputLen outputPtr nullPtr
              (\ol -> LI.smallChunkSize - fromIntegral ol) <$> peek outputLen
            remainingInputBytes <- peek inputLenP
            compressedBytesPtr <- peek inputP
            unconsumed' <- B.packCStringLen (castPtr compressedBytesPtr, fromIntegral remainingInputBytes)
            (\result -> (result, unconsumed', decompressed)) <$> readIORef res
          case res of
            1 -> do
              I.destroyDecoder st
              return $ if B.null decompressed
                then LI.Empty
                else LI.Chunk decompressed LI.Empty
            2 -> do -- Needs more input
              restOutput <- lazyDecompress st $ if B.null unconsumed'
                then rest
                else LI.Chunk unconsumed' rest
              return $ if B.null decompressed
                then restOutput
                else LI.Chunk decompressed restOutput
            3 -> do -- Needs more output
              -- Note that unconsumed here is always empty, but we feed an empty chunk
              -- in order to force as much consumption as needed before moving on.
              restOutput <- lazyDecompress st $ LI.Chunk unconsumed' rest
              return $ LI.Chunk decompressed restOutput
            _ -> do -- Failure
              I.destroyDecoder st
              error "Encountered an error"
        LI.Empty -> do
          I.destroyDecoder st
          return LI.Empty
