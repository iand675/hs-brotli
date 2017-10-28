{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE BangPatterns           #-}
module Codec.Compression.Brotli (
  Compress(..)
  , compress
  , decompress
  , BrotliStream(..)
  , Chunk(..)
  , compressor
  , decompressor
  , maxCompressedSize
  , CompressionSettings(..)
  , defaultCompressionSettings
  , setCompressionSettings
  , I.minWindowBits
  , I.maxWindowBits
  , I.minInputBlockBits
  , I.maxInputBlockBits
  , I.minQuality
  , I.maxQuality
  , I.encoderModeGeneric
  , I.encoderModeText
  , I.encoderModeFont
) where
import Control.Monad (when, unless, forM)
import Control.Exception (SomeException, assert, handle, bracket, throw)
import qualified Codec.Compression.Brotli.Internal as I
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI
import Foreign.ForeignPtr
import Data.Maybe (catMaybes)
import Data.IORef
import Data.Int
import Data.Word
import Foreign.C
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Marshal (alloca, allocaBytes, callocBytes, free)
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
    else pure ()

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
          else pure $ fromIntegral os
    pure res

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
  , availableOut :: !(Ptr CSize)
  , totalOut :: !(Ptr CSize)
  , nextIn :: !(Ptr (Ptr Word8))
  , nextOut :: !(Ptr (Ptr Word8))
  } deriving (Show)


freeStreamVars :: StreamVars -> IO ()
freeStreamVars = free . availableIn

createStreamVars :: IO StreamVars
createStreamVars = do
  bs <- callocBytes (3 * sizeOf (0 :: CSize) + 2 * sizeOf (nullPtr :: Ptr Word8))
  let aiPtr = bs
      aoPtr = plusPtr aiPtr (sizeOf (0 :: CSize))
      toPtr = plusPtr aoPtr (sizeOf (0 :: CSize))
      niPtr = castPtr $ plusPtr toPtr (sizeOf (nullPtr :: Ptr Word8))
      noPtr = plusPtr niPtr (sizeOf (nullPtr :: Ptr Word8))
      vs@StreamVars{..} = StreamVars aiPtr aoPtr toPtr niPtr noPtr
  pure vs

newtype EncoderFeedResponse = EncoderFeedResponse
  { pendingInput :: B.ByteString
  }

-- CAUTION: we aren't ensuring that bytestrings stay alive since the contents are
-- poked in and outlive the function (streaming and all)
--
-- Must use only within the context of the bytestring be alive via an external unsafeUseAsCStringLen
feedEncoder' :: I.BrotliEncoderState -> StreamVars -> Int -> B.ByteString -> IO CSize
feedEncoder' st vs@StreamVars{..} bufSize bs = B.unsafeUseAsCStringLen bs $ \(bsP, len) -> do
  poke availableIn (fromIntegral len)
  poke nextIn (castPtr bsP)
  res <- isTrue <$> I.encoderCompressStream st I.encoderOperationProcess availableIn nextIn availableOut nextOut totalOut
  unless res $ error "Unknown stream encoding failure"
  peek availableIn

-- CAUTION: we aren't ensuring that bytestrings stay alive since the contents are
-- poked in and outlive the function (streaming and all)
--
-- Must use only within the context of the bytestring be alive via an external unsafeUseAsCStringLen
feedEncoder :: I.BrotliEncoderState -> StreamVars -> Int -> B.ByteString -> IO EncoderFeedResponse
feedEncoder st vs@StreamVars{..} bufSize bs = B.unsafeUseAsCStringLen bs $ \(bsP, len) -> do
  poke availableIn (fromIntegral len)
  poke nextIn (castPtr bsP)
  res <- isTrue <$> I.encoderCompressStream st I.encoderOperationProcess availableIn nextIn availableOut nextOut totalOut
  unless res $ error "Unknown stream encoding failure"
  unconsumedBytesCount <- peek availableIn
  unconsumedBytesP <- peek nextIn
  unusedInput <- if unconsumedBytesCount == 0
    then pure B.empty
    else B.packCStringLen (castPtr unconsumedBytesP, fromIntegral unconsumedBytesCount)
  pure $ EncoderFeedResponse unusedInput

encoderMaybeTakeOutput :: I.BrotliEncoderState -> Int -> IO (Maybe B.ByteString)
encoderMaybeTakeOutput st bufSize = do
  takeOut <- isTrue <$> I.encoderHasMoreOutput st
  if takeOut
    then alloca $ \s -> do
      poke s $ fromIntegral bufSize
      bsp <- I.encoderTakeOutput st s
      len <- peek s
      Just <$> B.packCStringLen (castPtr bsp, fromIntegral len)
    else pure Nothing

encoderTakeRestAvailable :: I.BrotliEncoderState -> IO () -> Int -> L.ByteString -> IO L.ByteString
encoderTakeRestAvailable st cleanup bufSize graft = do
  out <- encoderMaybeTakeOutput st bufSize
  case out of
    Nothing -> pure graft
    Just bs -> do
      rest <- unsafeInterleaveIO $ encoderTakeRestAvailable st cleanup bufSize graft
      pure $ LI.Chunk bs rest

-- | Note that this should be called until returned bytestring is empty. Once is not enough.
finishStream :: I.BrotliEncoderState -> StreamVars -> IO () -> Int -> IO L.ByteString
finishStream st StreamVars{..} cleanup bufSize = do
  poke availableIn 0
  poke nextIn nullPtr
  res <- isTrue <$> I.encoderCompressStream st I.encoderOperationFinish availableIn nextIn availableOut nextOut totalOut
  unless res $ error "Unknown stream encoding failure"
  encoderTakeRestAvailable st cleanup bufSize L.empty


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

data DecompressionVars = DecompressionVars
  { dAvailableInput :: !(Ptr CSize)
  , dAvailableOut :: !(Ptr CSize)
  , dTotalOut :: !(Ptr CSize)
  , dNextIn :: !(Ptr (Ptr Word8))
  , dNextOut :: !(Ptr (Ptr Word8))
  }

createDecompressionVars :: IO DecompressionVars
createDecompressionVars = do
  bs <- callocBytes (3 * sizeOf (0 :: CSize) + 2 * sizeOf (nullPtr :: Ptr Word8))
  let aiPtr = bs
      aoPtr = plusPtr aiPtr (sizeOf (0 :: CSize))
      toPtr = plusPtr aoPtr (sizeOf (0 :: CSize))
      niPtr = castPtr $ plusPtr toPtr (sizeOf (nullPtr :: Ptr Word8))
      noPtr = plusPtr niPtr (sizeOf (nullPtr :: Ptr Word8))
  pure $ DecompressionVars aiPtr aoPtr toPtr niPtr noPtr

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
            poke dAvailableInput $ fromIntegral strLen
            poke dNextIn $ castPtr strP
            res <- I.decoderDecompressStream st dAvailableInput dNextIn dAvailableOut dNextOut dTotalOut
            remainingInputBytes <- peek dAvailableInput
            compressedBytesPtr <- peek dNextIn
            unconsumed' <- B.packCStringLen (castPtr compressedBytesPtr, fromIntegral remainingInputBytes)
            pure (res, unconsumed')
          case I.decoderResult res of
            I.Success -> do
              -- allTheRest <- takeRestAvailable st (I.destroyDecoder st >> destroyDecompressionVars vs) L.empty
              -- return allTheRest
              pure L.empty
            I.NeedsMoreInput -> do
              lazyDecompress st vs $ LI.chunk unconsumed' rest
            I.NeedsMoreOutput -> do
              -- Sneak invariant breaking here by pushing what is quite possibly an empty Chunk.
              -- this is intentional because we need one last empty string to trigger either success or error
              -- depending on whether the string shouldn't have ended there
              afterOut <- lazyDecompress st vs $ LI.Chunk unconsumed' rest
              decoderTakeRestAvailable st (pure ()) afterOut
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
    else pure Nothing

decoderTakeRestAvailable :: I.BrotliDecoderState -> IO () -> L.ByteString -> IO L.ByteString
decoderTakeRestAvailable st cleanup graft = do
  out <- decoderMaybeTakeOutput st
  case out of
    Nothing -> cleanup >> pure graft
    Just bs -> do
      rest <- unsafeInterleaveIO $ decoderTakeRestAvailable st cleanup graft
      pure $ LI.Chunk bs rest

data Chunk
  = Chunk !B.ByteString
  | Flush
  deriving (Show, Eq)

data BrotliStream input
  = Produce !B.ByteString (IO (BrotliStream input))
  | Consume (input -> IO (BrotliStream input))
  | Error
  | Done

withEncoder :: ForeignPtr I.BrotliEncoderState -> (I.BrotliEncoderState -> IO a) -> IO a
withEncoder p f = withForeignPtr p (f . I.BrotliEncoderState)

compressor :: CompressionSettings -> IO (BrotliStream Chunk)
compressor settings = do
  (I.BrotliEncoderState encoder) <- I.createEncoder
  efp <- newForeignPtr I.destroyEncoder_ptr encoder
  withEncoder efp $ \encoder -> setCompressionSettings encoder settings
  vars <- createStreamVars
  pure $ go efp vars
  where
    go efp vars = Consume (consume B.empty)
      where
        consume
          :: B.ByteString
             {- ^ Presently poked bytestring
                  Must `touch` underlying ForeignPtr until
                  the underlying contents have been consumed

                  In other words, consumption must be wrapped in an unsafeUseAsCStringLen to keep it alive.
                  We could be a bit more fine-grained on things if we use bytestring internals, but maybe
                  not worth the hassle?
             -}
          -> Chunk
          -> IO (BrotliStream Chunk)
        consume !currentBs !chunk = B.unsafeUseAsCStringLen currentBs $ \_ -> do
          let StreamVars{..} = vars
          case chunk of
            Chunk bs -> do
              -- print vars
              previousUnconsumedSize <- peek availableIn
              if previousUnconsumedSize == 0 && B.null bs
              then done
              else withEncoder efp $ \encoder -> do
                unconsumedSize <-
                  if previousUnconsumedSize == 0
                  then feedEncoder'
                         encoder
                         vars
                         (compressionBufferSize settings)
                         bs
                  else do
                    _ <- I.encoderCompressStream
                           encoder
                           I.encoderOperationProcess
                           availableIn
                           nextIn
                           availableOut
                           nextOut
                           totalOut
                    -- TODO assert result
                    peek availableIn

                let bytestringRef = if unconsumedSize > 0 then currentBs else B.empty
                moreOutput <- hasMoreOutput encoder
                if moreOutput
                  then produce bytestringRef $ if previousUnconsumedSize == 0
                    then Just chunk
                    else Nothing
                  else pure $ Consume (consume bytestringRef)
            Flush -> withEncoder efp $ \encoder -> do
              -- TODO make appropriate assertions here
              -- around in state & out state
              I.encoderCompressStream
                encoder
                I.encoderOperationFlush
                availableIn
                nextIn
                availableOut
                nextOut
                totalOut
              -- ls <- (,) <$> peek availableOut <*> peek availableIn
              -- print ls
              hasMore <- hasMoreOutput encoder
              produce B.empty Nothing

        produce :: B.ByteString -> Maybe Chunk -> IO (BrotliStream Chunk)
        produce currentInput unusedInput = withEncoder efp $ \encoder -> do
          -- assert: this function is only called from other functions
          -- when output is guaranteed
          out <- takeOutput encoder
          hasMore <- hasMoreOutput encoder
          pure $ Produce out $ if hasMore
            then produce currentInput unusedInput
            else
              maybe
                (pure $ Consume $ consume B.empty)
                (\c -> consume (case c of
                    Chunk bs -> bs
                    Flush -> B.empty) c)
                unusedInput

        -- err = undefined
        done :: IO (BrotliStream Chunk)
        done = withEncoder efp $ \encoder -> do
          let StreamVars {..} = vars
          poke availableIn 0
          poke nextIn nullPtr
          I.encoderCompressStream
            encoder
            I.encoderOperationFinish
            availableIn
            nextIn
            availableOut
            nextOut
            totalOut
          done'

        done' = withEncoder efp $ \encoder -> do
          out <- encoderMaybeTakeOutput encoder (compressionBufferSize settings)
          case out of
            Nothing -> do
              freeStreamVars vars
              pure Done
            Just str -> pure $ Produce str done'

decompressor :: IO (BrotliStream B.ByteString)
decompressor = do
  st <- I.createDecoder
  vs <- createDecompressionVars
  poke (dAvailableOut vs) 0
  poke (dNextOut vs) nullPtr
  go st vs
  where
    go st vs@DecompressionVars {..} = pure $ consume B.empty
      where
        consume :: B.ByteString -> BrotliStream B.ByteString
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
                pure (res, unconsumed')
            case I.decoderResult res of
              I.Success -> done
              I.NeedsMoreInput -> pure $ consume B.empty
              I.NeedsMoreOutput -> produce unconsumed'
              I.DecoderError e -> err -- I.destroyDecoder st >> throw e
        produce rem = do
          out <- decoderMaybeTakeOutput st
          case out of
            Nothing -> pure (consume rem)
            Just bs -> pure $ Produce bs (produce rem)
        err = pure Error
        done = do
          I.destroyDecoder st
          destroyDecompressionVars vs
          pure Done

