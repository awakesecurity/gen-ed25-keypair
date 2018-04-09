{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gen.Ed25519.KeyPair where

import           Crypto.Error
import           Crypto.PubKey.Ed25519       as Ed25519
import           Data.Bifunctor              as Bifunctor
import           Data.ByteArray
import           Data.ByteArray.Encoding
import           Data.ByteString             (ByteString)
import           Data.Monoid
import qualified           Control.Monad.Except as Except
import           Data.Text                   as Text
import           Data.Text.Encoding          (decodeUtf8', encodeUtf8)
import           GHC.Generics
import qualified Options.Applicative         as Options
import           Options.Applicative.Builder
import           Options.Generic

-- | Base64 encode a 'ByteArray' into a 'Text'.
encode64 :: (Except.MonadError String m, ByteArrayAccess bin)
         => bin
         -> m Text
encode64 bytes = either bad pure convertedBytes
  where
    bad = Except.throwError . show
    convertedBytes = decodeUtf8' (convertToBase Base64 bytes)

-- | Base64 decode a 'Text' into a 'ByteArray'.
decode64 :: (Except.MonadError String m, ByteArray bout)
         => Text
         -> m bout
decode64 text = either bad pure convertedBytes
  where
    bad = Except.throwError
    convertedBytes = convertFromBase Base64 (encodeUtf8 text)

-- | Generate a keypair from a good source of random 'ScrubbedBytes'.
--
-- 'Cryptonite.Random.getRandomBytes' is recommended as a source of
-- random bytes.
makeKeyPair :: (Except.MonadError String m)
            => ScrubbedBytes
            -> m KeyPair
makeKeyPair keyMaterial = either bad pure (mkKeys <$> skey)
  where
    skey = eitherCryptoError (Ed25519.secretKey keyMaterial)
    bad  = Except.throwError . show

    mkKeys :: Ed25519.SecretKey -> KeyPair
    mkKeys sk = KeyPair sk (Ed25519.toPublic sk)

-- | Sign a message using a keypair.
sign :: KeyPair -> ByteString -> Signature
sign KeyPair{..} = Ed25519.sign seckey pubkey

-- | Verify a message's signature with a public key.
verify :: PublicKey -> ByteString -> Signature -> Bool
verify = Ed25519.verify

-- | Record type for a secret and public pair of keys.
data KeyPair = KeyPair
  { seckey :: Ed25519.SecretKey
  , pubkey :: Ed25519.PublicKey
  } deriving (Generic, Eq)

-- | Parse a base64 encoded 'Ed25519.SecretKey' or 'Ed25519.PublicKey'
-- using one of their constructors.
parseKey :: (ByteString -> CryptoFailable a) -> Text -> IO a
parseKey constructor val =
  case decode64 val of
    Left e  -> fail e
    Right (pk :: ByteString) ->
      throwCryptoErrorIO (constructor pk)

-- | Parse a base64 encoded 'Ed25519.Signature'.
sigReader :: ReadM Ed25519.Signature
sigReader = Options.eitherReader parseSig
  where
    parseSig (Text.pack -> str') =
      case decode64 str' of
        Left e  -> Left e
        Right (t :: ByteString) ->
          Bifunctor.first show $ eitherCryptoError (Ed25519.signature t)

instance ParseFields Ed25519.Signature where
  parseFields h n c =
      (Options.option sigReader $
       ( Options.metavar "Signature"
       <> foldMap  Options.short                c
       <> foldMap (Options.long  . Text.unpack) n
       <> foldMap (Options.help  . Text.unpack) h
       )
      )

instance ParseRecord Ed25519.Signature where
  parseRecord = fmap getOnly parseRecord

-- | Parse a base64 encoded 'Ed25519.PublicKey'.
pubkeyReader :: ReadM Ed25519.PublicKey
pubkeyReader = Options.eitherReader parsePubkey
  where
    parsePubkey (Text.pack -> str') =
      case decode64 str' of
        Left e  -> Left e
        Right (t :: ByteString) ->
          Bifunctor.first show $ eitherCryptoError (Ed25519.publicKey t)

instance ParseFields Ed25519.PublicKey where
  parseFields h n c =
      (Options.option pubkeyReader $
       ( Options.metavar "PublicKey"
       <> foldMap  Options.short                c
       <> foldMap (Options.long  . Text.unpack) n
       <> foldMap (Options.help  . Text.unpack) h
       )
      )

instance ParseRecord Ed25519.PublicKey where
  parseRecord = fmap getOnly parseRecord
