{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Data.Encrypt where

import Control.Monad.Except
import Crypto.Cipher.Types
import Crypto.Error
import Data.ByteArray (ByteArray)
import Data.ByteArray.Encoding
import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Esqueleto.Experimental (PersistField)
import Database.Persist.Postgresql (PersistFieldSql)
import Prelude (Show (..))

data Key c a where
  Key :: (BlockCipher c, ByteArray a) => a -> Key c a

instance (BlockCipher c, ByteArray a) => Show (Key c a) where
  show (Key ba) = show (convertToBase Base64 ba :: ByteString)

newtype EncryptedText = EncryptedText Text
  deriving newtype (Show, PersistField, PersistFieldSql)

data EncryptionFailure
  = UnableToGenerateInitializationVector
  | UnableToDecodeInitializationVector
  | UnableToDecodeMessage
  | UnableToMakeInitializationVector
  | UnableToDecodeSecretKeyFromEnvironment
  | UnableToEncryptMessage CryptoError
  | UnableToDecryptMessage CryptoError
  | UnableToEncrypt Text
  deriving (Show)

type EncryptM m a = ExceptT EncryptionFailure m a
