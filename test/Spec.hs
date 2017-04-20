{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Crypto.PubKey.Ed25519 as Ed25519
import           Crypto.Random
import           Data.ByteString       (ByteString)
import           Data.Text             (Text)
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Gen.Ed25519.KeyPair   as KeyPair

secretKeyConst :: Text
secretKeyConst = "HdGaCl9DSfW212f2s3GwnLKwk64o9cCtQSo6y/i0afg="

publicKeyConst ::  Text
publicKeyConst = "68mKS897BDWQpo9c3vuoq4/EUd0anmVzGXc+pmWwwak="

messageConst :: ByteString
messageConst = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam non imperdiet purus, et lacinia justo. Nunc mi ante, molestie at est sit amet, imperdiet fringilla purus. Aliquam lobortis mi eros, sed molestie est maximus dignissim. In hac habitasse platea dictumst. Vivamus sit amet egestas ante, ut porttitor urna. Ut imperdiet sapien nulla, nec ullamcorper massa pharetra sit amet. Aenean non auctor mauris, et elementum magna. Donec varius feugiat eleifend."

signatureConst :: Text
signatureConst = "1F7lKfyxozlE0rHZu/vtGtGKsLD5nvuJPgiuDO8haEUoKoQ299p2rm9jPJ7nQY0kYhFQsOJDDfeRry5xOaG2BA=="

main :: IO ()
main =
  defaultMain
    (testGroup
       "Tests"
       [ testCase "Key Pair Generation" testKeyPairGen
       , testCase "Message Signing w/ Key Pair" testMessageSigning
       ])

testKeyPairGen :: Assertion
testKeyPairGen = do
  -- NB: 32 is a magic constant that's not exposed by the Ed25519
  -- module and the required key length to create a SecretKey
  keyMaterial <- getRandomBytes 32
  case KeyPair.makeKeyPair keyMaterial of
    Left e  -> assertFailure $ show e
    Right _ -> pure ()

testMessageSigning :: Assertion
testMessageSigning = do
  seckey <- KeyPair.parseKey Ed25519.secretKey secretKeyConst
  pubkey <- KeyPair.parseKey Ed25519.publicKey publicKeyConst

  let keypair = KeyPair.KeyPair{..}
      sig     = KeyPair.sign keypair messageConst

  case KeyPair.encode64 sig of
    Left e           -> assertFailure e
    Right encodedSig -> do
      encodedSig @?= signatureConst
      assertBool "Signature verification failed" (KeyPair.verify (KeyPair.pubkey keypair) messageConst sig)
