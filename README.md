**NOTE**: _This repository is no longer supported or updated by Awake Security / Arista Networks. If you wish to continue to develop this code yourself, we recommend you fork it._</code>

# Welcome!

This is a small Haskell CLI utility and library for generating an Ed25519
keypair, signing and verifying messages with an Ed25519 keypair.

## Example Usage

```shell
$ gen-ed25-keypair --help
Generate a base64 encoded, Ed25519 keypair

Usage: gen-ed25-keypair [--secretKey FILEPATH] [--publicKey FILEPATH]

Available options:
  -h,--help                Show this help text
  --secretKey FILEPATH     Filepath to write the secret key to, if not provided,
                           key will be written to the current working dir
  --publicKey FILEPATH     Filepath to write the public key to, if not provided,
                           key will be written to the current working dir
```

The keypair is generated using `getRandomBytes` from [Cryptonite.Random](https://hackage.haskell.org/package/cryptonite-0.22/docs/Crypto-Random.html) and the keys are base64 encoded.

Another convenience utility is also provided for signing messages with a generated keypair and verifying message signatures:

```shell
$ sign-ed25 --help
Sign a message with a Ed25519 keypair

Usage: sign-ed25 --secretKey FILEPATH --publicKey FILEPATH --msg TEXT

Available options:
  -h,--help                Show this help text
  --secretKey FILEPATH     Path to the secret key
  --publicKey FILEPATH     Path to the public key
  --msg TEXT               Message to sign
```

Another tool can be used to verify the signature:
```shell
$ verify-ed25 --help
Verify a message signature with an Ed25519 public key

Usage: verify-ed25 --publickey PublicKey --sig Signature --msg TEXT

Available options:
  -h,--help                Show this help text
  --publickey PublicKey    Public key to verify a message signature
  --sig Signature          Message signature to verify
  --msg TEXT               Message to verify signature
```

```shell
$ verify-ed25 --publickey 'zoOk/5Fpnbdu3DJ4V0/dGzde+xjdDD2L+WQkJpxOALQ=' --msg 'lorem ipsum dolor set, balbalbalbalbalbalbal' --sig 'vqiIWLc+ZXyjUg10CWXv2VAI1Q25mcUl4GpXpJYlOBU9oWc832AbYtizXnalpqlAVqmRJxLETXIa7zxEQJzADg=='
True
```

