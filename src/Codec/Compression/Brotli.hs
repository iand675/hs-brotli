{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Codec.Compression.Brotli where
import Control.Monad (when, unless, forM)
import Control.Exception (SomeException, assert, handle, bracket, throw)
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

data StreamVars = StreamVars
  { availableIn :: !(Ptr CSize)
  , nextIn :: !(Ptr (Ptr Word8))
  , availableOut :: !(Ptr CSize)
  , nextOut :: !(Ptr (Ptr Word8))
  , totalOut :: !(Ptr CSize)
  }


freeStreamVars :: StreamVars -> IO ()
freeStreamVars = free . availableIn

data StreamResponse = StreamResponse
  { pendingInput :: !B.ByteString
  , output :: !B.ByteString
  }

stream :: I.BrotliEncoderState -> StreamVars -> Int -> B.ByteString -> IO StreamResponse
stream st vs@StreamVars{..} bufSize bs = B.unsafeUseAsCStringLen bs $ \(bsP, len) -> do
  poke availableIn (fromIntegral len)
  poke nextIn (castPtr bsP)
  poke availableOut $ fromIntegral bufSize
  outBs <- BI.createUptoN bufSize $ \outP -> do
    poke nextOut outP
    res <- isTrue <$> I.encoderCompressStream st I.encoderOperationProcess availableIn nextIn availableOut nextOut totalOut
    unless res $ error "Unknown stream encoding failure"
    outCount <- peek availableOut
    return $ bufSize - fromIntegral outCount
  unconsumedBytesCount <- peek availableIn
  unconsumedBytesP <- peek nextIn
  unusedInput <- if unconsumedBytesCount == 0
    then return B.empty
    else B.packCStringLen (castPtr unconsumedBytesP, fromIntegral unconsumedBytesCount)
  return $ StreamResponse unusedInput outBs

-- | Note that this should be called until returned bytestring is empty. Once is not enough.
finishStream :: I.BrotliEncoderState -> StreamVars -> Int -> IO B.ByteString
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
  return outBs


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

instance Compress L.ByteString L.ByteString where
  compressWith b settings = unsafePerformIO $ do
    inst <- I.createEncoder
    setCompressionSettings inst settings
    bs <- mallocBytes (5 * sizeOf (nullPtr :: Ptr ()))
    let vars = StreamVars (castPtr bs) (pOff 1 bs) (pOff 2 bs) (pOff 3 bs) (pOff 4 bs)
    lazyCompress inst vars b
    where
      lazyCompress st vars c = unsafeInterleaveIO $ readChunks st vars c
      lazyFinish st vars = unsafeInterleaveIO $ handle (\(e :: SomeException) -> freeStreamVars vars >> I.destroyEncoder st >> throw e) $ do
        res <- finishStream st vars $ compressionBufferSize settings
        if B.null res
          then freeStreamVars vars >> return L.empty
          else do
            cs <- lazyFinish st vars
            return $ pushNoCheck res cs
      readChunks st vars c = do
        case c of
          LI.Chunk bs next -> handle (\(e :: SomeException) -> freeStreamVars vars >> I.destroyEncoder st >> throw e) $ do
            (StreamResponse unusedInput output) <- stream st vars (compressionBufferSize settings) bs
            rest <- lazyCompress st vars $ LI.chunk unusedInput next
            return $ LI.chunk output rest
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

data DecompressionVars = DecompressionVars
  { dAvailableInput :: !(Ptr CSize)
  , dNextIn :: !(Ptr (Ptr Word8))
  , dAvailableOut :: !(Ptr CSize)
  , dNextOut :: !(Ptr (Ptr Word8))
  , dTotalOut :: !(Ptr CSize)
  }

createDecompressionVars :: IO DecompressionVars
createDecompressionVars = do
  bs <- mallocBytes (5 * sizeOf (nullPtr :: Ptr ()))
  return $ DecompressionVars (castPtr bs) (pOff 1 bs) (pOff 2 bs) (pOff 3 bs) (pOff 4 bs)

destroyDecompressionVars :: DecompressionVars -> IO ()
destroyDecompressionVars = free . dAvailableInput

decompress :: L.ByteString -> L.ByteString
decompress b = unsafePerformIO $ do
  st <- I.createDecoder
  vs <- createDecompressionVars
  poke (dAvailableOut vs) 0
  poke (dNextOut vs) nullPtr
  lazyDecompress st vs b
  where
    lazyDecompress st vs rest = unsafeInterleaveIO $ writeChunks st vs rest
    writeChunks st vs@DecompressionVars{..} lbs = do
      case lbs of
        LI.Chunk bs rest -> do
          v@(res, unconsumed') <- B.unsafeUseAsCStringLen bs $ \(strP, strLen) -> do
            poke dAvailableInput (fromIntegral strLen)
            poke dNextIn (castPtr strP)
            res <- I.decoderDecompressStream st dAvailableInput dNextIn dAvailableOut dNextOut dTotalOut
            remainingInputBytes <- peek dAvailableInput
            compressedBytesPtr <- peek dNextIn
            unconsumed' <- B.packCStringLen (castPtr compressedBytesPtr, fromIntegral remainingInputBytes)
            return (res, unconsumed')
          case I.decoderResult res of
            I.Success -> do
              allTheRest <- takeRestAvailable st (I.destroyDecoder st >> destroyDecompressionVars vs) L.empty
              return allTheRest
            I.NeedsMoreInput -> do
              lazyDecompress st vs $ LI.chunk unconsumed' rest
            I.NeedsMoreOutput -> do
              -- Sneak invariant breaking here by pushing what is quite possibly an empty Chunk.
              -- this is intentional because we need one last empty string to trigger either success or error
              -- depending on whether the string shouldn't have ended there
              afterOut <- lazyDecompress st vs $ LI.Chunk unconsumed' rest
              takeRestAvailable st (return ()) afterOut
            I.DecoderError e -> I.destroyDecoder st >> throw e
        LI.Empty -> do
          -- TODO empty is an error if the whole bytestring is empty
          I.destroyDecoder st
          destroyDecompressionVars vs
          return L.empty

maybeTakeOutput :: I.BrotliDecoderState -> IO (Maybe B.ByteString)
maybeTakeOutput st = do
  takeIn <- isTrue <$> I.decoderHasMoreOutput st
  if takeIn
    then do
      (bsp, len) <- alloca $ \s -> do
        poke s 0 -- TODO settings
        bsp <- I.decoderTakeOutput st s
        len <- peek s
        return (bsp, len)
      Just <$> B.packCStringLen (castPtr bsp, fromIntegral len)
    else return Nothing

takeRestAvailable :: I.BrotliDecoderState -> IO () -> L.ByteString -> IO L.ByteString
takeRestAvailable st cleanup graft = do
  out <- maybeTakeOutput st
  case out of
    Nothing -> cleanup >> return graft
    Just bs -> do
      rest <- unsafeInterleaveIO $ takeRestAvailable st cleanup graft
      return $ LI.Chunk bs rest
