# zkFold UTxO Accumulator Server

This repository houses off-chain code and server endpoints to interact with [zkFold](https://zkfold.io/)'s UTxO accumulator smart contract.

## Table of Contents

- [zkFold UTxO Accumulator Server](#zkfold-utxo-accumulator-server)
  - [Table of Contents](#table-of-contents)
  - [Structure of repository](#structure-of-repository)
  - [API server](#api-server)
    - [Server Deployment](#server-deployment)
    - [Building locally from source using the Haskell Toolchain](#building-locally-from-source-using-the-haskell-toolchain)
    - [OpenApi documentation](#openapi-documentation)
  - [Tests](#tests)

## Structure of repository

- [`utxo-accumulator-api`](./utxo-accumulator-api/) provides off-chain to UTxO Accumulator transactions.
- [`utxo-accumulator-server`](./utxo-accumulator-server/) contains the server that can accumulate and distribute funds.

## API server

### Server Deployment

To deploy the UTxO Accumulator Server, follow these steps:

#### Prerequisites

The server requires several cryptographic libraries to be installed on your machine (same as for running `cardano-node`):

1. **Install LIBSODIUM**:
   ```bash
   git clone https://github.com/input-output-hk/libsodium
   cd libsodium
   git checkout dbb48cc
   ./autogen.sh
   ./configure
   make
   sudo make install
   sudo ldconfig
   ```

2. **Install SECP256K1**:
   ```bash
   git clone https://github.com/bitcoin-core/secp256k1
   cd secp256k1
   git checkout ac83be33d0956faf6b7f61a60ab524ef7d6a473a
   ./autogen.sh
   ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental
   make
   sudo make install
   sudo ldconfig
   # Create symlink for version compatibility if needed
   if [ -f /usr/lib/libsecp256k1.so.0 ] && [ ! -f /usr/lib/libsecp256k1.so.2 ]; then
     sudo ln -sf /usr/lib/libsecp256k1.so.0 /usr/lib/libsecp256k1.so.2
   fi
   ```

3. **Install BLST**:
   ```bash
   BLST_VERSION='v0.3.11'
   git clone --depth 1 --branch ${BLST_VERSION} https://github.com/supranational/blst
   cd blst
   ./build.sh
   # Install library files
   sudo cp bindings/blst_aux.h bindings/blst.h bindings/blst.hpp /usr/local/include/
   sudo cp libblst.a /usr/local/lib/
   # Create pkg-config file
   cat > libblst.pc << EOF
   prefix=/usr/local
   exec_prefix=\${prefix}
   libdir=\${exec_prefix}/lib
   includedir=\${prefix}/include

   Name: libblst
   Description: Multilingual BLS12-381 signature library
   URL: https://github.com/supranational/blst
   Version: ${BLST_VERSION#v}
   Cflags: -I\${includedir}
   Libs: -L\${libdir} -lblst
   EOF
   sudo cp libblst.pc /usr/local/lib/pkgconfig/
   sudo chmod u=rw,go=r /usr/local/lib/libblst.a /usr/local/lib/pkgconfig/libblst.pc /usr/local/include/blst*
   ```

#### Server installation

1. **Create a new directory for the server**:
   ```bash
   mkdir -p utxo-accumulator-server
   cd utxo-accumulator-server
   ```

2. **Download the setup script**:
   ```bash
   curl -L "https://raw.githubusercontent.com/zkFold/utxo-accumulator-server/main/setup.sh" -o setup.sh
   chmod +x setup.sh
   ```

3. **Run the setup script**:
   ```bash
   ./setup.sh
   ```

4. **Configure the server**:
   After setup is complete, you need to manually update the configuration files:

   a. **Choose a configuration file** to edit (e.g., `config/100ada.yaml` for a 100 ADA pool, etc.)

   b. **Update the following required fields**:

   - **`port`**: Set your desired port number (it must be open on your machine)
   - **`maestroToken`**: Update with your Maestro API token
   - **`wallet.contents.mnemonic`**: Replace with your 24-word wallet mnemonic phrase
   - **`networkId`**: Ensure it matches your target network (`mainnet` or `preprod`)

   **Example configuration sections to update**:
   ```yaml
   port: 8082                          # Change to your desired port
   networkId: preprod                  # or "mainnet"
   coreProvider:
     maestroToken: YOUR_MAESTRO_TOKEN  # Replace with your token
   wallet:
     contents:
       mnemonic:
         - word1    # Replace with your actual
         - word2    # 24-word mnemonic phrase
         # ... continue for all 24 words
   ```

5. **Verify the server installation**:
   - Check that the binary `utxo-accumulator-server` exists and is executable
   - Verify all required configuration files and directories are present:
     - `config/` directory with YAML configuration files
     - `crs.json` (common reference string for zero-knowledge proofs)
     - `database/cache.json` (database cache file)
     - `web/openapi/api.json` (API documentation)

6. **Test the server**:
   This step should output the server's command list:
   ```bash
   ./utxo-accumulator-server --help
   ```
7. **Launch the server**:
   Use the following command to run the server for a 100 ADA pool:
   ```bash
   ./utxo-accumulator-server run -c config/100ada.yaml --clean-db
   ```

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
3. Run the server with command `cabal run utxo-accumulator-server -- accumulate -c config.yaml`.

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