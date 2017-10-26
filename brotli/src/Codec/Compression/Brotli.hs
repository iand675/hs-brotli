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
  , compressionBufferSize = 0
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

createStreamVars :: IO StreamVars
createStreamVars = do
  bs <- mallocBytes (5 * sizeOf (nullPtr :: Ptr ()))
  return $ StreamVars (castPtr bs) (pOff 1 bs) (pOff 2 bs) (pOff 3 bs) (pOff 4 bs)

newtype EncoderFeedResponse = EncoderFeedResponse
  { pendingInput :: B.ByteString
  }

feedEncoder :: I.BrotliEncoderState -> StreamVars -> Int -> B.ByteString -> IO EncoderFeedResponse
feedEncoder st vs@StreamVars{..} bufSize bs = B.unsafeUseAsCStringLen bs $ \(bsP, len) -> do
  poke availableIn (fromIntegral len)
  poke nextIn (castPtr bsP)
  res <- isTrue <$> I.encoderCompressStream st I.encoderOperationProcess availableIn nextIn availableOut nextOut totalOut
  unless res $ error "Unknown stream encoding failure"
  unconsumedBytesCount <- peek availableIn
  unconsumedBytesP <- peek nextIn
  unusedInput <- if unconsumedBytesCount == 0
    then return B.empty
    else B.packCStringLen (castPtr unconsumedBytesP, fromIntegral unconsumedBytesCount)
  return $ EncoderFeedResponse unusedInput

encoderMaybeTakeOutput :: I.BrotliEncoderState -> Int -> IO (Maybe B.ByteString)
encoderMaybeTakeOutput st bufSize = do
  takeOut <- isTrue <$> I.encoderHasMoreOutput st
  if takeOut
    then alloca $ \s -> do
      poke s $ fromIntegral bufSize
      bsp <- I.encoderTakeOutput st s
      len <- peek s
      Just <$> B.packCStringLen (castPtr bsp, fromIntegral len)
    else return Nothing

encoderTakeRestAvailable :: I.BrotliEncoderState -> IO () -> Int -> L.ByteString -> IO L.ByteString
encoderTakeRestAvailable st cleanup bufSize graft = do
  out <- encoderMaybeTakeOutput st bufSize
  case out of
    Nothing -> return graft
    Just bs -> do
      rest <- unsafeInterleaveIO $ encoderTakeRestAvailable st cleanup bufSize graft
      return $ LI.Chunk bs rest

-- | Note that this should be called until returned bytestring is empty. Once is not enough.
finishStream :: I.BrotliEncoderState -> StreamVars -> IO () -> Int -> IO L.ByteString
finishStream st StreamVars{..} cleanup bufSize = do
  poke availableIn 0
  poke nextIn nullPtr
  res <- isTrue <$> I.encoderCompressStream st I.encoderOperationFinish availableIn nextIn availableOut nextOut totalOut
  unless res $ error "Unknown stream encoding failure"
  encoderTakeRestAvailable st cleanup bufSize L.empty


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
    vars <- createStreamVars
    poke (availableOut vars) 0
    poke (nextOut vars) nullPtr
    let cleanup = freeStreamVars vars >> I.destroyEncoder inst
    lazyCompress cleanup inst vars b
    where
      lazyCompress cleanup st vars c = unsafeInterleaveIO $ readChunks cleanup st vars c
      readChunks cleanup st vars c = do
        case c of
          LI.Chunk bs next -> handle (\(e :: SomeException) -> cleanup >> throw e) $ do
            (EncoderFeedResponse unusedInput) <- feedEncoder st vars (compressionBufferSize settings) bs
            -- NOTE: LI.chunk checks for empty string results so we don't have to worry about empty chunks ourselves.
            rest <- lazyCompress cleanup st vars $ LI.chunk unusedInput next
            encoderTakeRestAvailable st cleanup (compressionBufferSize settings) rest
          LI.Empty -> finishStream st vars (freeStreamVars vars >> I.destroyEncoder st) $ compressionBufferSize settings
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
              -- allTheRest <- takeRestAvailable st (I.destroyDecoder st >> destroyDecompressionVars vs) L.empty
              -- return allTheRest
              return L.empty
            I.NeedsMoreInput -> do
              lazyDecompress st vs $ LI.chunk unconsumed' rest
            I.NeedsMoreOutput -> do
              -- Sneak invariant breaking here by pushing what is quite possibly an empty Chunk.
              -- this is intentional because we need one last empty string to trigger either success or error
              -- depending on whether the string shouldn't have ended there
              afterOut <- lazyDecompress st vs $ LI.Chunk unconsumed' rest
              decoderTakeRestAvailable st (return ()) afterOut
            -- TODO need a safe version of decompress too
            I.DecoderError e -> I.destroyDecoder st >> throw e
        LI.Empty -> do
          I.destroyDecoder st
          destroyDecompressionVars vs
          throw $ I.BrotliDecoderErrorCode 2

decoderMaybeTakeOutput :: I.BrotliDecoderState -> IO (Maybe B.ByteString)
decoderMaybeTakeOutput st = do
  takeIn <- isTrue <$> I.decoderHasMoreOutput st
  if takeIn
    then alloca $ \s -> do
      poke s 0 -- TODO settings
      bsp <- I.decoderTakeOutput st s
      len <- peek s
      Just <$> B.packCStringLen (castPtr bsp, fromIntegral len)
    else return Nothing

decoderTakeRestAvailable :: I.BrotliDecoderState -> IO () -> L.ByteString -> IO L.ByteString
decoderTakeRestAvailable st cleanup graft = do
  out <- decoderMaybeTakeOutput st
  case out of
    Nothing -> cleanup >> return graft
    Just bs -> do
      rest <- unsafeInterleaveIO $ decoderTakeRestAvailable st cleanup graft
      return $ LI.Chunk bs rest

data Compressor
  = Produce B.ByteString (IO Compressor)
  | Consume (B.ByteString -> IO Compressor)
  | Error
  | Done

compressor :: CompressionSettings -> IO Compressor
compressor settings = do
  inst <- I.createEncoder
  setCompressionSettings inst settings
  vars <- createStreamVars
  poke (availableOut vars) 0
  poke (nextOut vars) nullPtr
  go inst vars
  where
    -- TODO try to reuse pointers directly instead of going to and from bytestrings
    -- on leftover inputs
    go inst vars = return $ consume B.empty
      where
        consume :: B.ByteString -> Compressor
        consume leftovers =
          Consume $ \bs -> do
            if B.null leftovers && B.null bs
              then done
              else do
                (EncoderFeedResponse resp) <-
                  feedEncoder
                    inst
                    vars
                    (compressionBufferSize settings)
                    (if B.null leftovers
                       then bs
                       else if B.null bs
                            then leftovers
                            else (leftovers `mappend` bs)) 
                if B.null resp
                  then return $ consume B.empty
                  else produce resp
        produce :: B.ByteString -> IO Compressor
        produce leftovers = do
          out <- encoderMaybeTakeOutput inst (compressionBufferSize settings)
          case out of
            Nothing -> return $ consume leftovers
            Just str -> return $ Produce str (produce leftovers)
        -- err = undefined
        done :: IO Compressor
        done = do
          let StreamVars {..} = vars
          poke availableIn 0
          poke nextIn nullPtr
          I.encoderCompressStream
            inst
            I.encoderOperationFinish
            availableIn
            nextIn
            availableOut
            nextOut
            totalOut
          done'
        done' = do
          out <- encoderMaybeTakeOutput inst (compressionBufferSize settings)
          case out of
            Nothing -> do
              freeStreamVars vars
              I.destroyEncoder inst
              return Done
            Just str -> return $ Produce str done'

decompressor :: IO Compressor
decompressor = do
  st <- I.createDecoder
  vs <- createDecompressionVars
  poke (dAvailableOut vs) 0
  poke (dNextOut vs) nullPtr
  go st vs
  where
    go st vs@DecompressionVars {..} = return $ consume B.empty
      where
        consume :: B.ByteString -> Compressor
        consume leftover =
          Consume $ \bs -> do
            v@(res, unconsumed') <-
              B.unsafeUseAsCStringLen
                (if B.null leftover
                   then bs
                   else if B.null bs
                          then B.empty
                          else leftover `mappend` bs) $ \(strP, strLen) -> do
                poke dAvailableInput (fromIntegral strLen)
                poke dNextIn (castPtr strP)
                res <-
                  I.decoderDecompressStream
                    st
                    dAvailableInput
                    dNextIn
                    dAvailableOut
                    dNextOut
                    dTotalOut
                remainingInputBytes <- peek dAvailableInput
                compressedBytesPtr <- peek dNextIn
                unconsumed' <-
                  B.packCStringLen
                    ( castPtr compressedBytesPtr
                    , fromIntegral remainingInputBytes)
                return (res, unconsumed')
            case I.decoderResult res of
              I.Success -> done
              I.NeedsMoreInput -> return $ consume B.empty
              I.NeedsMoreOutput -> produce unconsumed'
              I.DecoderError e -> err -- I.destroyDecoder st >> throw e
        produce rem = do
          out <- decoderMaybeTakeOutput st
          case out of
            Nothing -> return (consume rem)
            Just bs -> return $ Produce bs (produce rem)
        err = return Error
        done = do
          I.destroyDecoder st
          destroyDecompressionVars vs
          return Done
