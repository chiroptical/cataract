{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Encrypt where

import Import.NoFoundation hiding (Key)

import Control.Monad.Except
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types
import Crypto.Error
import Crypto.Random.Types qualified as CRT
import Data.ByteArray (ByteArray)
import Data.ByteArray.Encoding
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

-- TODO:
-- Concatenate secret, initialization vector, and encrypted base16 text and store
-- them in the database, wrapping and unwrapping seamlessly

data Key c a where
  Key :: (BlockCipher c, ByteArray a) => a -> Key c a

instance (BlockCipher c, ByteArray a) => Show (Key c a) where
  show (Key ba) = show (convertToBase Base64 ba :: ByteString)

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

exampleAES128 :: ByteString -> IO ()
exampleAES128 msg = do
  -- secret key needs 256 bits (32 * 8)
  secretKey <- genSecretKey (error "genSecretKey called argument" :: AES256) 32
  mInitIV <- genRandomIV (error "genRandomIV called argument" :: AES256)
  case mInitIV of
    Nothing -> error "Failed to generate and initialization vector."
    Just initIV -> do
      let encryptedMsg = encrypt secretKey initIV msg
          decryptedMsg = decrypt secretKey initIV =<< encryptedMsg
      case (,) <$> encryptedMsg <*> decryptedMsg of
        Left err -> error $ show err
        Right (eMsg, dMsg) -> do
          putStrLn $ tshow secretKey
          putStrLn $ tshow (convertToBase Base64 initIV :: ByteString)
          putStrLn $ "Original Message: " <> tshow msg
          putStrLn $ "Message after encryption: " <> tshow (convertToBase Base64 eMsg :: ByteString)
          putStrLn $ "Message after decryption: " <> tshow dMsg

standaloneDecrypt :: IO ()
standaloneDecrypt = do
  let eSecretKey :: Either String (Key AES256 ByteString)
      eSecretKey = Key <$> convertFromBase Base16 ("38392ee7c8ad135da9f69ac947085354fec557e324a10fc2675ab6ccb657979d" :: ByteString)
  let eInitIV :: Either String (Maybe (IV AES256))
      eInitIV =
        makeIV @ByteString
          <$> convertFromBase Base16 ("5582b8783a29f1aeae0f4cf0afa120ed" :: ByteString)
  let eMsg :: Either String ByteString
      eMsg = convertFromBase Base16 ("7a3c467e9e6f04e8811709befe8361" :: ByteString)
  case (eSecretKey, eInitIV, eMsg) of
    (Right secretKey, Right (Just initIV), Right encryptedMsg) -> do
      let decryptedMsg = decrypt secretKey initIV encryptedMsg
      case decryptedMsg of
        Right msg -> putStrLn $ tshow msg
        Left err -> putStrLn $ tshow err
    _ ->
      putStrLn "It didn't work..."

newtype EncryptedText = EncryptedText Text
  deriving newtype (Show, PersistField)

data EncryptionFailure
  = UnableToGenerateInitializationVector
  | UnableToDecodeInitializationVector
  | UnableToDecodeMessage
  | UnableToMakeInitializationVector
  | UnableToDecodeSecretKeyFromEnvironment
  | UnableToEncryptMessage CryptoError
  | UnableToDecryptMessage CryptoError
  deriving (Show)

mkSecretKey ::
  (MonadUnliftIO m, MonadError EncryptionFailure m, MonadReader EncryptionSettings m) =>
  m (Key AES256 ByteString)
mkSecretKey = do
  EncryptionSettings{..} <- ask
  let eSecretKey = convertFromBase @_ @ByteString Base64 encryptionSettingsCipherSecretKey
  case eSecretKey of
    Left _ -> throwError UnableToDecodeSecretKeyFromEnvironment
    Right secretKey -> pure $ Key secretKey

mkInitIvRandom ::
  (MonadUnliftIO m, MonadError EncryptionFailure m) =>
  m (IV AES256)
mkInitIvRandom = do
  mInitIV <- liftIO $ genRandomIV (error "genRandomIV called argument in encryptText" :: AES256)
  case mInitIV of
    Nothing -> throwError UnableToGenerateInitializationVector
    Just iv -> pure iv

encryptText ::
  (MonadUnliftIO m, MonadError EncryptionFailure m, MonadReader EncryptionSettings m) =>
  Text ->
  m EncryptedText
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

-- TODO: Convert this to 'MonadError EncryptionFailure m' style
decryptText ::
  (MonadUnliftIO m, MonadReader EncryptionSettings m) =>
  EncryptedText ->
  m (Either EncryptionFailure Text)
decryptText (EncryptedText ivDotMessage) = do
  EncryptionSettings{..} <- ask
  let eSecretKey :: Either String (Key AES256 ByteString)
      eSecretKey = Key <$> convertFromBase Base64 encryptionSettingsCipherSecretKey
      (ivText, msgText) = T.drop 1 <$> T.breakOn "." ivDotMessage
      eInitIV :: Either String (Maybe (IV AES256))
      eInitIV =
        makeIV @ByteString
          <$> convertFromBase Base64 (TE.encodeUtf8 ivText)
  pure $ case (eSecretKey, eInitIV) of
    (Left _, _) -> Left UnableToDecodeSecretKeyFromEnvironment
    (_, Left _) -> Left UnableToDecodeInitializationVector
    (_, Right Nothing) -> Left UnableToMakeInitializationVector
    (Right secretKey, Right (Just iv)) ->
      let eEncryptedMessage = convertFromBase @_ @ByteString Base64 (TE.encodeUtf8 msgText)
       in case eEncryptedMessage of
            Left _ -> Left UnableToDecodeMessage
            Right encryptedMsg ->
              case decrypt secretKey iv encryptedMsg of
                Left ce -> Left $ UnableToDecryptMessage ce
                Right msg -> Right $ TE.decodeUtf8 msg

-- exampleAES256 :: IO ()
-- exampleAES256 = do
--   let encryptionSettings =
--         EncryptionSettings
--           { encryptionSettingsCipherSecretKey = "LG43ovFqO0wVMIOh+PlXOtVJ4WNqO1rW0yLcXEiV9ow="
--           }
--   eEncryptedText <- liftIO $ runReaderT (encryptText "hello, world...") encryptionSettings
--   putStrLn $ tshow eEncryptedText
--   case eEncryptedText of
--     Left err -> putStrLn $ tshow err
--     Right encryptedText -> do
--       eDecryptedText <- liftIO $ runReaderT (decryptText encryptedText) encryptionSettings
--       putStrLn $ tshow eDecryptedText
