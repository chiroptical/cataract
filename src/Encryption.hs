{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Encryption where

import Data.Encrypt

import Control.Monad.Except (runExceptT, throwError)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types
import Crypto.Error
import Crypto.Random.Types qualified as CRT
import Data.ByteArray (ByteArray)
import Data.ByteArray.Encoding
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Import.NoFoundation hiding (Key)

-- | Generates a string of bytes (key) of a specific length for a given block cipher
genSecretKey :: forall m c a. (CRT.MonadRandom m, BlockCipher c, ByteArray a) => c -> Int -> m (Key c a)
genSecretKey _ = fmap Key . CRT.getRandomBytes

-- | Generate a random initialization vector for a given block cipher
genRandomIV :: forall m c. (CRT.MonadRandom m, BlockCipher c) => c -> m (Maybe (IV c))
genRandomIV _ = do
  bytes :: ByteString <- CRT.getRandomBytes $ blockSize (error "blockSize called argument" :: c)
  return $ makeIV bytes

-- | Initialize a block cipher
initCipher :: (BlockCipher c, ByteArray a) => Key c a -> Either CryptoError c
initCipher (Key k) = case cipherInit k of
  CryptoFailed e -> Left e
  CryptoPassed a -> Right a

encrypt :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
encrypt secretKey initIV msg =
  case initCipher secretKey of
    Left e -> Left e
    Right c -> Right $ ctrCombine c initIV msg

decrypt :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
decrypt = encrypt

mkSecretKey ::
  (MonadUnliftIO m, MonadReader EncryptionSettings m) =>
  EncryptM m (Key AES256 ByteString)
mkSecretKey = do
  EncryptionSettings {..} <- ask
  let eSecretKey = convertFromBase @_ @ByteString Base64 encryptionSettingsCipherSecretKey
  case eSecretKey of
    Left _ -> throwError UnableToDecodeSecretKeyFromEnvironment
    Right secretKey -> pure $ Key secretKey

convertFromBase64With ::
  (MonadUnliftIO m, MonadReader EncryptionSettings m) =>
  EncryptionFailure ->
  ByteString ->
  EncryptM m ByteString
convertFromBase64With ef bs = do
  case convertFromBase Base64 bs of
    Left _ -> throwError ef
    Right x -> pure x

mkInitIvRandom ::
  (MonadUnliftIO m, MonadReader EncryptionSettings m) =>
  EncryptM m (IV AES256)
mkInitIvRandom = do
  mInitIV <- liftIO $ genRandomIV (error "genRandomIV called argument in encryptText" :: AES256)
  case mInitIV of
    Nothing -> throwError UnableToGenerateInitializationVector
    Just iv -> pure iv

mkInitIv ::
  (MonadUnliftIO m, MonadReader EncryptionSettings m) =>
  Text ->
  EncryptM m (IV AES256)
mkInitIv ivText = do
  encoded <- convertFromBase64With UnableToDecodeInitializationVector (TE.encodeUtf8 ivText)
  case makeIV encoded of
    Nothing -> throwError UnableToDecodeInitializationVector
    Just x -> pure x

encryptText ::
  (MonadUnliftIO m, MonadReader EncryptionSettings m) =>
  Text ->
  EncryptM m EncryptedText
encryptText msg = do
  secretKey <- mkSecretKey
  initIv <- mkInitIvRandom
  case encrypt secretKey initIv $ TE.encodeUtf8 msg of
    Left ce -> throwError $ UnableToEncryptMessage ce
    Right encryptedMessage ->
      pure . EncryptedText $
        TE.decodeUtf8 $
          (convertToBase Base64 initIv :: ByteString)
            <> "."
            <> (convertToBase Base64 encryptedMessage :: ByteString)

decryptText ::
  (MonadUnliftIO m, MonadReader EncryptionSettings m) =>
  EncryptedText ->
  EncryptM m Text
decryptText (EncryptedText ivDotMessage) = do
  let (ivText, msgText) = T.drop 1 <$> T.breakOn "." ivDotMessage
  secretKey <- mkSecretKey
  iv <- mkInitIv ivText
  encryptedMessage <- convertFromBase64With UnableToDecodeMessage (TE.encodeUtf8 msgText)
  case decrypt secretKey iv encryptedMessage of
    Left ce -> throwError $ UnableToDecryptMessage ce
    Right msg -> pure $ TE.decodeUtf8 msg

runEncryptM ::
  EncryptionSettings ->
  (forall m. (MonadUnliftIO m, MonadReader EncryptionSettings m) => EncryptM m a) ->
  IO (Either EncryptionFailure a)
runEncryptM es action = liftIO $ runReaderT (runExceptT action) es
