{-# LANGUAGE DeriveDataTypeable #-}
module Codec.Compression.Brotli.Internal where
import Control.Exception
import Data.Typeable (Typeable(..))
import Data.Word
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import System.IO.Unsafe

type BrotliAlloc a = FunPtr (Ptr a -> CSize -> IO (Ptr ()))
type BrotliFree a = FunPtr (Ptr a -> Ptr () -> IO ())

minWindowBits, maxWindowBits, minInputBlockBits, maxInputBlockBits, minQuality, maxQuality, defaultQuality, defaultWindow :: Num a => a
minWindowBits = 10
maxWindowBits = 24
minInputBlockBits = 16
maxInputBlockBits = 24
minQuality = 0
maxQuality = 11
defaultQuality = 11
defaultWindow = 22
defaultMode = encoderModeGeneric

newtype BrotliEncoderMode = BrotliEncoderMode { fromBrotliEncoderMode :: CInt }
  deriving (Show)

encoderModeGeneric = BrotliEncoderMode 0
encoderModeText = BrotliEncoderMode 1
encoderModeFont = BrotliEncoderMode 2

newtype BrotliEncoderState = BrotliEncoderState (Ptr BrotliEncoderState)
  deriving (Show)

newtype BrotliEncoderOperation = BrotliEncoderOperation CInt
  deriving (Show)

encoderOperationProcess = BrotliEncoderOperation 0
encoderOperationFlush = BrotliEncoderOperation 1
encoderOperationFinish = BrotliEncoderOperation 2
encoderOperationEmitMetadata = BrotliEncoderOperation 3

newtype BrotliEncoderParameter = BrotliEncoderParameter CInt
  deriving (Show)

mode = BrotliEncoderParameter 0
quality = BrotliEncoderParameter 1
lz77WindowSize = BrotliEncoderParameter 2
lz77BlockSize = BrotliEncoderParameter 3
disableLiteralContextModeling = BrotliEncoderParameter 4
brotliParamSizeHint = BrotliEncoderParameter 5

foreign import ccall unsafe "BrotliEncoderCreateInstance" brotliEncoderCreateInstance :: BrotliAlloc a -> BrotliFree a -> Ptr a -> IO BrotliEncoderState

createEncoder :: IO BrotliEncoderState
createEncoder = brotliEncoderCreateInstance nullFunPtr nullFunPtr nullPtr

foreign import ccall unsafe "BrotliEncoderSetParameter" encoderSetParameter
  :: BrotliEncoderState
  -> BrotliEncoderParameter
  -> Word32
  -> IO CInt -- ^ Bool
foreign import ccall unsafe "BrotliEncoderDestroyInstance" destroyEncoder :: BrotliEncoderState -> IO ()

foreign import ccall unsafe "BrotliEncoderMaxCompressedSize" maxCompressedSize :: CSize -> CSize

foreign import ccall safe "BrotliEncoderCompress" encoderCompress
  :: CInt
  -> CInt
  -> BrotliEncoderMode -- Sizeof?
  -> CSize
  -> Ptr Word8
  -> Ptr CSize
  -> Ptr Word8
  -> IO CInt -- ^ Bool

foreign import ccall safe "BrotliEncoderCompressStream" encoderCompressStream
  :: BrotliEncoderState
  -> BrotliEncoderOperation
  -> Ptr CSize
  -> Ptr (Ptr Word8)
  -> Ptr CSize
  -> Ptr (Ptr Word8)
  -> Ptr CSize
  -> IO CInt -- ^ Bool

foreign import ccall unsafe "BrotliEncoderIsFinished" encoderIsFinished
  :: BrotliEncoderState
  -> IO CInt -- ^ Bool

foreign import ccall unsafe "BrotliEncoderHasMoreOutput" encoderHasMoreOutput
  :: BrotliEncoderState
  -> IO CInt -- ^ Bool

foreign import ccall unsafe "BrotliEncoderTakeOutput" encoderTakeOutput
  :: BrotliEncoderState
  -> Ptr CSize
  -> IO (Ptr Word8)

foreign import ccall unsafe "BrotliEncoderVersion" encoderVersion
  :: IO Word32


newtype BrotliDecoderState = BrotliDecoderState (Ptr BrotliDecoderState)

foreign import ccall unsafe "BrotliDecoderCreateInstance" brotliDecoderCreateInstance
  :: BrotliAlloc a
  -> BrotliFree a
  -> Ptr a
  -> IO BrotliDecoderState

createDecoder :: IO BrotliDecoderState
createDecoder = brotliDecoderCreateInstance nullFunPtr nullFunPtr nullPtr

foreign import ccall unsafe "BrotliDecoderDestroyInstance" destroyDecoder
  :: BrotliDecoderState
  -> IO ()

foreign import ccall safe "BrotliDecoderDecompress" decoderDecompress
  :: CSize
  -> Ptr Word8
  -> Ptr CSize
  -> Ptr Word8
  -> IO CInt -- TODO result proper value

foreign import ccall safe "BrotliDecoderDecompressStream" decoderDecompressStream
  :: BrotliDecoderState
  -> Ptr CSize
  -> Ptr (Ptr Word8)
  -> Ptr CSize
  -> Ptr (Ptr Word8)
  -> Ptr CSize
  -> IO BrotliDecoderErrorCode -- TODO result proper value

foreign import ccall unsafe "BrotliDecoderHasMoreOutput" decoderHasMoreOutput
  :: BrotliDecoderState
  -> IO CInt -- Bool

foreign import ccall unsafe "BrotliDecoderTakeOutput" decoderTakeOutput
  :: BrotliDecoderState
  -> Ptr CSize
  -> IO (Ptr Word8)

foreign import ccall unsafe "BrotliDecoderIsUsed" decoderIsUsed
  :: BrotliDecoderState
  -> IO CInt

foreign import ccall unsafe "BrotliDecoderIsFinished" decoderIsFinished
  :: BrotliDecoderState
  -> IO CInt

newtype BrotliDecoderErrorCode = BrotliDecoderErrorCode CInt
  deriving (Eq, Typeable)

instance Show BrotliDecoderErrorCode where
  show c = unsafePerformIO (decoderErrorString c >>= peekCString)

instance Exception BrotliDecoderErrorCode

data DecoderResult
  = Success
  | NeedsMoreInput
  | NeedsMoreOutput
  | DecoderError BrotliDecoderErrorCode

decoderResult :: BrotliDecoderErrorCode -> DecoderResult
decoderResult c@(BrotliDecoderErrorCode n) = case n of
  1 -> Success
  2 -> NeedsMoreInput
  3 -> NeedsMoreOutput
  _ -> DecoderError c
{-# INLINE decoderResult #-}

foreign import ccall unsafe "BrotliDecoderGetErrorCode" decoderGetError
  :: BrotliDecoderState
  -> IO BrotliDecoderErrorCode

foreign import ccall unsafe "BrotliDecoderErrorString" decoderErrorString
  :: BrotliDecoderErrorCode
  -> IO CString

foreign import ccall unsafe "BrotliDecoderVersion" decoderVersion
  :: IO Word32
