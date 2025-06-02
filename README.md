# zkFold UTxO Accumulator Server

This repository houses off-chain code and server endpoints to interact with [zkFold](https://zkfold.io/)'s UTxO accumulator smart contract.

## Table of Contents

- [zkFold UTxO Accumulator Server](#zkfold-utxo-accumulator-server)
  - [Table of Contents](#table-of-contents)
  - [Structure of repository](#structure-of-repository)
  - [API server](#api-server)
    - [Building locally from source using the Haskell Toolchain](#building-locally-from-source-using-the-haskell-toolchain)
    - [OpenApi documentation](#openapi-documentation)
  - [Tests](#tests)

## Structure of repository

- [`utxo-accumulator-api`](./utxo-accumulator-api/) provides off-chain to UTxO Accumulator transactions.
- [`utxo-accumulator-server`](./utxo-accumulator-server/) contains the server that can accumulate and distribute funds.

## API server

### Building locally from source using the Haskell Toolchain

1. Make sure your environment is configured properly, consult ["How to build?"](https://atlas-app.io/getting-started/how-to-build) section of Atlas documentation for it.
2. Prepare a configuration, which can be stored either in file or in `SERVER_CONFIG` environment variable. Structure of it is as follows:

    ```yaml
     # Blockchain provider used by Atlas, our off-chain transaction building tool.
     # Head over to https://atlas-app.io/getting-started/endpoints#providing-data-provider section to know how to configure `coreProvider` and what all options are available for it.
    coreProvider:
      maestroToken: YOUR_MAESTRO_TOKEN
      turboSubmit: false
     # Network id, only `mainnet` and `preprod` are supported for at the moment.
    networkId: mainnet
     # Logging configuration. It's an array to cater for potentially multiple scribes.
     # See it's description mentioned at https://atlas-app.io/getting-started/endpoints#providing-data-provider for more information.
    logging:
      - type:
          tag: stderr
         # Possible values of `severity` are `Debug`, `Info`, `Warning` and `Error`.
        severity: Debug
         # Possible values of `verbosity` are `V0`, `V1`, `V2`, `V3` and `V4`. Consult https://hackage.haskell.org/package/katip-0.8.8.0/docs/Katip.html#t:Verbosity for more information about it.
        verbosity: V2
     # Port to serve endpoints at.
    port: 8082
     # API key to protect server endpoints with. It's value must be provided under `api-key` header of request.
    serverApiKey: YOUR_SECRET_KEY
     # SQLite database of sponsors.
    databasePath: "db.sqlite"
     # Gmail account used to send email notifications
    notifier:
      email: "email@gmail.com"
      password: "app-password"
     # UTxO to be used as collateral.
    collateral: tx-id#tx-ix
     # Wallet that provides UTxO to be used as collateral.
    collateralWallet:
      tag: mnemonicWallet
      contents:
        mnemonic:
          - health
          - unable
          - dog
          - lend
          - artefact
          - arctic
          - dinner
          - energy
          - silent
          - wealth
          - shock
          - safe
          - glad
          - mail
          - gas
          - flag
          - beauty
          - penalty
          - mixed
          - garbage
          - erupt
          - wonder
          - magnet
          - around
        # Account index.
        accIx: 0
        # Payment address index.
        addrIx: 0
    ```
3. Run the server with command `cabal run utxo-accumulator-server -- serve -c my-config.yaml`.

   Call: `cabal run utxo-accumulator-server -- -h` for help. 😉

4. Test if server is running successfully by calling, say, `/settings` endpoint. Example `curl` request: `curl -H 'api-key: YOUR_SECRET_KEY' -X GET http://localhost:8082/v0/settings | jq`, assuming port was specified as `8082`. On success, it should return something akin to:

```json
{
  "network":"mainnet",
  "version":"0.1.0",
  "collateral":"tx-id#tx-ix",
  "collateral_address":"addr1qx...w60mw"
}
```

### OpenApi documentation

Endpoints made available by server are specified [here](./web/openapi/api.yaml).

## Tests

To run for privnet tests:

```
cabal install --package-env=$(pwd) --overwrite-policy=always cardano-cli cardano-node
cabal run utxo-accumulator-api-tests -- -j1
```

Sometimes, node instances are still running even after completion of tests, execute `killall cardano-node` after running tests to kill node instances.