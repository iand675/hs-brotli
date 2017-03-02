{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE RecordWildCards        #-}
module Codec.Compression.Brotli where
import Control.Monad (when, unless, forM)
import Control.Exception (bracket)
import qualified Codec.Compression.Brotli.Internal as I
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI
import Data.Maybe (catMaybes)
import Data.IORef
import Data.Int
import Data.Word
import Foreign.C
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Marshal (alloca, allocaBytes, mallocBytes, free)
import Foreign.Storable (sizeOf, peek, poke)
import GHC.Int
import GHC.Types
import System.IO.Unsafe

data CompressionSettings = CompressionSettings
  { compressionQuality :: !Word32
  , compressionWindow :: !Word32
  , compressionMode :: !I.BrotliEncoderMode
  , compressionBufferSize :: !Int
  , compressionBlockSize :: !(Maybe Word32)
  , compressionDisableLiteralContextModeling :: !(Maybe Word32)
  , compressionSizeHint :: !(Maybe Word32)
  }

defaultCompressionSettings :: CompressionSettings
defaultCompressionSettings = CompressionSettings
  { compressionQuality = I.defaultQuality
  , compressionWindow = I.defaultWindow
  , compressionMode = I.defaultMode
  , compressionBufferSize = (16 * 1024) - LI.chunkOverhead
  , compressionBlockSize = Nothing
  , compressionDisableLiteralContextModeling = Nothing
  , compressionSizeHint = Nothing
  }

setCompressionSettings :: I.BrotliEncoderState -> CompressionSettings -> IO ()
setCompressionSettings st CompressionSettings{..}= do
  r1 <- I.encoderSetParameter st I.mode $ fromIntegral $ I.fromBrotliEncoderMode compressionMode
  r2 <- I.encoderSetParameter st I.quality compressionQuality
  r3 <- I.encoderSetParameter st I.lz77WindowSize compressionWindow
  mr4 <- forM compressionBlockSize $ I.encoderSetParameter st I.lz77BlockSize
  mr5 <- forM compressionDisableLiteralContextModeling $ I.encoderSetParameter st I.disableLiteralContextModeling
  mr6 <- forM compressionSizeHint $ I.encoderSetParameter st I.brotliParamSizeHint
  if any (/= 1) $ r1 : r2 : r3 : catMaybes [mr4, mr5, mr6]
    then error "Invalid compression setting parameter"
    else return ()

class Compress a b where
  compressWith :: a -> CompressionSettings -> b

compress :: Compress a b => a -> b
compress a = compressWith a defaultCompressionSettings

instance Compress B.ByteString B.ByteString where
  compressWith b CompressionSettings{..} = unsafePerformIO $ do
    let estBufSize = fromIntegral $ I.maxCompressedSize $ fromIntegral $ B.length b
    res <- alloca $ \outSize -> do
      poke outSize estBufSize
      BI.createAndTrim (fromIntegral estBufSize) $ \outBuf -> B.unsafeUseAsCStringLen b $ \(inPtr, inLen) -> do

        ok <- I.encoderCompress (fromIntegral compressionQuality) (fromIntegral compressionWindow) compressionMode (fromIntegral inLen) (castPtr inPtr) outSize outBuf
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

data StreamVars = StreamVars
  { availableIn :: !(Ptr CSize)
  , nextIn :: !(Ptr (Ptr Word8))
  , availableOut :: !(Ptr CSize)
  , nextOut :: !(Ptr (Ptr Word8))
  , totalOut :: !(Ptr CSize)
  , outBuffer :: !(Ptr Word8)
  }

stream :: I.BrotliEncoderState -> StreamVars -> Int -> B.ByteString -> IO FeedResponse
stream st StreamVars{..} bufSize bs = B.unsafeUseAsCStringLen bs $ \(bsP, len) -> do
  poke availableIn (fromIntegral len)
  poke nextIn (castPtr bsP)
  poke availableOut $ fromIntegral bufSize
  outBs <- BI.createUptoN bufSize $ \outP -> do
    poke nextOut outP
    res <- isTrue <$> I.encoderCompressStream st I.encoderOperationProcess availableIn nextIn availableOut nextOut totalOut
    unless res $ do
      free availableIn
      free outBuffer
      error "Unknown stream encoding failure"
    outCount <- peek availableOut
    return $ bufSize - fromIntegral outCount
  unconsumedBytesCount <- peek availableIn
  unconsumedBytesP <- peek nextIn
  -- TODO don't copy bytestring if possible. Should be possible
  remainingInput <- B.packCStringLen (castPtr unconsumedBytesP, fromIntegral unconsumedBytesCount)
  total <- fromIntegral <$> peek totalOut
  return $ FeedResponse remainingInput outBs total

-- | Note that this should be called until returned bytestring is empty. Once is not enough.
finishStream :: I.BrotliEncoderState -> StreamVars -> Int -> IO (B.ByteString, Int)
finishStream st StreamVars{..} bufSize = do
  poke availableIn 0
  poke nextIn nullPtr
  poke availableOut $ fromIntegral bufSize
  outBs <- BI.createUptoN bufSize $ \outP -> do
    poke nextOut outP
    res <- isTrue <$> I.encoderCompressStream st I.encoderOperationFinish availableIn nextIn availableOut nextOut totalOut
    unless res $ error "Unknown stream encoding failure"
    outCount <- peek availableOut
    return $ bufSize - fromIntegral outCount
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
pOff :: Int -> Ptr a -> Ptr b
pOff n p = castPtr $ plusPtr p (n * sizeOf p)

pushNoCheck :: B.ByteString -> L.ByteString -> L.ByteString
pushNoCheck = LI.Chunk

-- TODO properly finalize encoder, malloc'ed buffers in the event of exceptions
instance Compress L.ByteString L.ByteString where
  compressWith b settings = unsafePerformIO $ do
    inst <- I.createEncoder
    setCompressionSettings inst settings
    bs <- mallocBytes (5 * sizeOf (nullPtr :: Ptr ()))
    outBuf <- mallocBytes $ compressionBufferSize settings
    let vars = StreamVars (castPtr bs) (pOff 1 bs) (pOff 2 bs) (pOff 3 bs) (pOff 4 bs) outBuf
    lazyCompress inst vars b
    where
      lazyCompress st vars c = unsafeInterleaveIO $ readChunks st vars c
      lazyFinish st vars = unsafeInterleaveIO $ do
        (res, _) <- finishStream st vars $ compressionBufferSize settings
        if B.null res
          then free (availableIn vars) >> free (outBuffer vars) >> return L.empty
          else do
            cs <- lazyFinish st vars
            return $ pushNoCheck res cs
      readChunks st vars c = do
        case c of
          LI.Chunk bs next -> do
            fr <- stream st vars (compressionBufferSize settings) bs
            if B.null $ availableOutput fr
              then lazyCompress st vars $ LI.chunk (unusedInput fr) next
              else do
                rest <- lazyCompress st vars $ LI.chunk (unusedInput fr) next
                return $ pushNoCheck (availableOutput fr) rest
          LI.Empty -> lazyFinish st vars
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
                then L.empty
                else LI.chunk decompressed L.empty
            2 -> do -- Needs more input
              restOutput <- lazyDecompress st $ if B.null unconsumed'
                then rest
                else LI.chunk unconsumed' rest
              return $ if B.null decompressed
                then restOutput
                else LI.chunk decompressed restOutput
            3 -> do -- Needs more output
              -- Note that unconsumed here is always empty, but we feed an empty chunk
              -- in order to force as much consumption as needed before moving on.
              -- This technically breaks the invariants of normal bytestring usage,
              -- but this intermediate step isn't used outside of the function or
              -- fed into any other bytestring functions.
              restOutput <- lazyDecompress st $ LI.Chunk unconsumed' rest
              return $ LI.chunk decompressed restOutput
            _ -> do -- Failure
              I.destroyDecoder st
              error "Encountered an error"
        LI.Empty -> do
          I.destroyDecoder st
          return L.empty
