-- | Framing for the node transport: a 4-byte big-endian payload length
-- followed by the payload.
module Dxdingo.Wire
  ( FrameError (..)
  , maxFrameSize
  , encodeFrame
  , decodeFrame
  , readFrame
  , writeFrame
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString, toLazyByteString, word32BE)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word32)
import System.IO (Handle, hFlush)

data FrameError
  = -- | The stream ended inside a frame.
    Truncated
  | -- | The length header exceeds 'maxFrameSize'.
    TooLarge Word32
  deriving (Eq, Show)

-- | Upper bound on one payload. The length is read before the body, so a
-- corrupt header must not trigger a 4 GiB allocation.
maxFrameSize :: Word32
maxFrameSize = 256 * 1024 * 1024

encodeFrame :: ByteString -> ByteString
encodeFrame p =
  BL.toStrict . toLazyByteString $
    word32BE (fromIntegral (BS.length p)) <> byteString p

decodeLength :: ByteString -> Word32
decodeLength = BS.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) 0

-- | Split one frame off the front of a buffer. 'Nothing' means the buffer
-- was empty: a clean end of stream between frames.
decodeFrame :: ByteString -> Either FrameError (Maybe (ByteString, ByteString))
decodeFrame buf
  | BS.null buf = Right Nothing
  | BS.length buf < 4 = Left Truncated
  | n > maxFrameSize = Left (TooLarge n)
  | BS.length body < len = Left Truncated
  | otherwise = Right (Just (BS.take len body, BS.drop len body))
  where
    (hdr, body) = BS.splitAt 4 buf
    n = decodeLength hdr
    len = fromIntegral n

-- | Read one frame. 'Right Nothing' on a clean end of stream.
readFrame :: Handle -> IO (Either FrameError (Maybe ByteString))
readFrame h = do
  hdr <- BS.hGet h 4
  if BS.null hdr
    then pure (Right Nothing)
    else
      if BS.length hdr < 4
        then pure (Left Truncated)
        else do
          let n = decodeLength hdr
          if n > maxFrameSize
            then pure (Left (TooLarge n))
            else do
              body <- BS.hGet h (fromIntegral n)
              pure $
                if BS.length body < fromIntegral n
                  then Left Truncated
                  else Right (Just body)

writeFrame :: Handle -> ByteString -> IO ()
writeFrame h p = BS.hPut h (encodeFrame p) >> hFlush h
