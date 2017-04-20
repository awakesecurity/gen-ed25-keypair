{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import qualified Control.Monad.Except      as Except
import           Control.Monad.Trans       (lift)
import qualified Crypto.PubKey.Ed25519     as Ed25519
import           Data.ByteString           (ByteString)
import qualified Data.Text.IO              as Text.IO
import           Filesystem.Path           (FilePath)
import           Filesystem.Path.CurrentOS (encodeString)
import           Options.Generic
import           Prelude                   hiding (FilePath)
import           System.Exit

import qualified Gen.Ed25519.KeyPair       as KeyPair

data Options w = Options
  { secretKey  :: w ::: FilePath
    <?> "Path to the secret key"
  , publicKey  :: w ::: FilePath
    <?> "Path to the public key"
  , msg        :: w ::: ByteString
    <?> "Message to sign"
  } deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  Options{..} <- unwrapRecord "Sign a message with a Ed25519 keypair"
  let readKey constructor path =
        KeyPair.parseKey constructor =<< Text.IO.readFile path

  result <- Except.runExceptT $ do
    seckey <- lift $ readKey Ed25519.secretKey (encodeString secretKey)
    pubkey <- lift $ readKey Ed25519.publicKey (encodeString publicKey)

    sig <- KeyPair.encode64 (KeyPair.sign KeyPair.KeyPair{..} msg)
    lift $ Text.IO.putStrLn sig

  either die pure result
