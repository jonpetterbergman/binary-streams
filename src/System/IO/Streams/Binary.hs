{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Binary 
  ( -- * Decoding
    binaryFromStream
  , binaryInputStream
  
    -- * Encoding
  , binaryToStream
  , binaryOutputStream

    -- * Decode Exceptions 
  , DecodeException(..) 
  ) where

import           Control.Exception            (throw,Exception)
import           Data.Binary                  (Binary,get,put)
import           Data.Binary.Builder          (Builder,toLazyByteString)
import           Data.Binary.Get              (runGetIncremental,
                                               Decoder(..),
                                               pushEndOfInput,
                                               pushChunk,
                                               ByteOffset)
import           Data.Binary.Put              (execPut)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as S
import           Data.Typeable                (Typeable)
import           System.IO.Streams            (makeInputStream,
                                               makeOutputStream,
                                               InputStream,
                                               OutputStream,
                                               write,
                                               unRead)
import qualified System.IO.Streams            as Stream
import           System.IO.Streams.ByteString (writeLazyByteString)


-- | An Exception raised when decoding fails.
data DecodeException = DecodeException ByteOffset String 
  deriving (Typeable)

instance Show DecodeException where
  show (DecodeException offset message) = 
        "Decode exception, offset " ++ show offset ++ ":" ++ show message

instance Exception DecodeException

decodeFromStream :: Decoder a 
                 -> InputStream ByteString 
                 -> IO (Maybe a)
decodeFromStream decoder is =
  Stream.read is >>=
  maybe (return Nothing)
        (\s -> if S.null s then go decoder else go $ pushChunk decoder s)
  where go (Fail _ offset message) = throw $ DecodeException offset message
        go (Done s _ x) =
                   do
                     if S.null s then return () else unRead s is
                     return $ Just x
        go  decoder' = Stream.read is >>=
                       maybe (go $ pushEndOfInput decoder') 
                             (\s -> if S.null s then go decoder' else go $ pushChunk decoder' s)

-- | Read an instance of 'Binary' from an 'InputStream', throwing a
--   'DecodeException' if the decoding fails.
--
--   'binaryFromStream' consumes only as much input as necessary: 
--   any unconsumed input is pushed back onto the 'InputStream'.
binaryFromStream :: Binary a
                 => InputStream ByteString
                 -> IO (Maybe a)
binaryFromStream = decodeFromStream (runGetIncremental get)

-- | Transform an 'InputStream' over byte strings to an 'InputStream' yielding
--   values of type a, throwing a 'DecodeException' if the decoding fails.
binaryInputStream :: Binary a
                  => InputStream ByteString
                  -> IO (InputStream a)
binaryInputStream = makeInputStream . binaryFromStream

-- | Write an instance of 'Binary' to an 'InputStream'.
binaryToStream :: Binary a
               => OutputStream ByteString
               -> Maybe a
               -> IO ()
binaryToStream os Nothing = write Nothing os
binaryToStream os (Just x) = writeLazyByteString (toLazyByteString $ execPut $ put x) os

-- | Transform an 'OutputStream' accepting byte strings to an 'OutputStream'
--   accepting values of type a.
binaryOutputStream :: Binary a
                   => OutputStream ByteString
                   -> IO (OutputStream a)
binaryOutputStream = makeOutputStream . binaryToStream
