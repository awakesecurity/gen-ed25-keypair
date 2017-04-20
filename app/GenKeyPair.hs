{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import qualified Control.Monad.Except      as Except
import           Control.Monad.Trans       (lift)
import           Crypto.Random
import           Data.Maybe
import qualified Data.Text.IO              as Text.IO
import           Filesystem.Path           (FilePath)
import           Filesystem.Path.CurrentOS (encodeString)
import           Options.Generic
import           Prelude                   hiding (FilePath)
import           System.Exit

import           Gen.Ed25519.KeyPair       (KeyPair (..))
import qualified Gen.Ed25519.KeyPair       as KeyPair

data Options w = Options
  { secretKey :: w ::: Maybe FilePath
    <?> "Filepath to write the secret key to, if not provided, key will be written to the current working dir"
  , publicKey :: w ::: Maybe FilePath
    <?> "Filepath to write the public key to, if not provided, key will be written to the current working dir"
  }
  deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  Options{..} <- unwrapRecord "Generate a base64 encoded, Ed25519 keypair"

  let seckeyPath = fromMaybe "./secret-ed25519.key" (encodeString <$> secretKey)
      pubkeyPath = fromMaybe "./public-ed25519.key" (encodeString <$> publicKey)

  -- NB: 32 is a magic constant that's not exposed by the Ed25519
  -- module and is the required key length to create a SecretKey
  keyMaterial <- getRandomBytes 32

  result <- Except.runExceptT $ do
    KeyPair{..} <- KeyPair.makeKeyPair keyMaterial
    seckey'     <- KeyPair.encode64 seckey
    pubkey'     <- KeyPair.encode64 pubkey

    lift $ Text.IO.writeFile seckeyPath seckey'
    lift $ Text.IO.writeFile pubkeyPath pubkey'

  either die pure result
