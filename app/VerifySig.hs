{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Data.ByteString       (ByteString)
import           Options.Generic

import qualified Gen.Ed25519.KeyPair   as KeyPair

data Options w = Options
  { publickey :: w ::: Ed25519.PublicKey
    <?> "Public key to verify a message signature"
  , sig       :: w ::: Ed25519.Signature
    <?> "Message signature to verify"
  , msg       :: w ::: ByteString
    <?> "Message to verify signature"
  } deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  Options{..} <- unwrapRecord "Verify a message signature with an Ed25519 public key"
  print $ KeyPair.verify publickey msg sig
