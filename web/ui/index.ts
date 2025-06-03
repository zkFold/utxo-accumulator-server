import * as CSL from '@emurgo/cardano-serialization-lib-browser';

// Simple UI for accumulating a blockchain address
const form = document.createElement('form');
form.id = 'accumulate-form';
form.innerHTML = `
  <label for="address">Blockchain Address:</label>
  <input type="text" id="address" name="address" style="width: 600px;" />
  <button type="submit">Accumulate</button>
  <div id="result" style="margin-top:1em;"></div>
`;
document.body.appendChild(form);

// Add Lace wallet connect button
const laceBtn = document.createElement('button');
laceBtn.textContent = 'Connect Lace Wallet';
laceBtn.type = 'button';
laceBtn.style.marginLeft = '1em';
form.querySelector('label')!.appendChild(laceBtn);

let laceApi: any = null;
laceBtn.onclick = async () => {
  if ((window as any).cardano?.lace) {
    try {
      laceApi = await (window as any).cardano.lace.enable();
      alert('Lace wallet connected!');
    } catch (e) {
      alert('Failed to connect Lace wallet: ' + e);
    }
  } else {
    alert('Lace wallet not found. Please install or enable it.');
  }
};

// Add Eternl wallet connect button
const eternlBtn = document.createElement('button');
eternlBtn.textContent = 'Connect Eternl Wallet';
eternlBtn.type = 'button';
eternlBtn.style.marginLeft = '1em';
form.querySelector('label')!.appendChild(eternlBtn);

let eternlApi: any = null;
eternlBtn.onclick = async () => {
  if ((window as any).cardano?.eternl) {
    try {
      eternlApi = await (window as any).cardano.eternl.enable();
      alert('Eternl wallet connected!');
    } catch (e) {
      alert('Failed to connect Eternl wallet: ' + e);
    }
  } else {
    alert('Eternl wallet not found. Please install or enable it.');
  }
};

// Add button to fill input with wallet address
const fillAddrBtn = document.createElement('button');
fillAddrBtn.textContent = 'Fill with Wallet Address';
fillAddrBtn.type = 'button';
fillAddrBtn.style.marginLeft = '1em';
form.querySelector('label')!.appendChild(fillAddrBtn);

fillAddrBtn.onclick = async () => {
  let api = eternlApi || laceApi;
  if (!api) {
    alert('Connect a wallet first!');
    return;
  }
  try {
    const used = await api.getUsedAddresses();
    if (used && used.length > 0) {
      let addr = used[0];
      let debugMsg = `Original address (hex?): ${addr}\n`;
      try {
        debugMsg += `CSL: imported module\n`;
        if (CSL && CSL.Address && /^[0-9a-fA-F]+$/.test(addr)) {
          debugMsg += 'Attempting hex to bech32 conversion...\n';
          const bytes = Uint8Array.from(addr.match(/.{1,2}/g).map((x: string) => parseInt(x, 16)));
          debugMsg += `Bytes: [${Array.from(bytes).join(', ')}]\n`;
          addr = CSL.Address.from_bytes(bytes).to_bech32();
          debugMsg += `Converted to bech32: ${addr}\n`;
        } else {
          debugMsg += 'CSL.Address not found or address not hex.\n';
        }
      } catch (e) {
        debugMsg += 'Conversion error: ' + e + '\n';
      }
      (document.getElementById('address') as HTMLInputElement).value = addr;
      alert(debugMsg); // Show debug info
    } else {
      alert('No used addresses found in wallet.');
    }
  } catch (e) {
    alert('Failed to get wallet address: ' + e);
  }
};

form.addEventListener('submit', async (e) => {
  e.preventDefault();
  const address = (document.getElementById('address') as HTMLInputElement).value;
  const resultDiv = document.getElementById('result');
  resultDiv!.textContent = 'Sending...';

  // The API expects a JSON object: { tx_sender, tx_recipient, tx_nonce }
  const body = JSON.stringify({
    tx_sender: address,
    tx_recipient: address,
    tx_nonce: 0
  });

  try {
    const response = await fetch('http://localhost:8082/v0/transaction', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json;charset=utf-8',
        'api-key': '123456',
      },
      body,
    } as any);

    if (!response.ok) {
      resultDiv!.textContent = `Error: ${response.status} ${response.statusText}`;
      return;
    }
    const data = await response.json();
    // Use the helper for Eternl or Lace if connected
    if (eternlApi && data && typeof data === 'string') {
      const ok = await signAndSubmitTxWithWallet(eternlApi, data, 'Eternl', resultDiv!);
      if (ok) return;
    }
    if (laceApi && data && typeof data === 'string') {
      const ok = await signAndSubmitTxWithWallet(laceApi, data, 'Lace', resultDiv!);
      if (ok) return;
    }
    resultDiv!.textContent = 'Success! Transaction: ' + JSON.stringify(data);
  } catch (err) {
    resultDiv!.textContent = 'Request failed: ' + err;
  }
});

// Helper to sign and submit a transaction with a wallet (Lace or Eternl)
async function signAndSubmitTxWithWallet(walletApi: any, cborHex: string, walletLabel: string, resultDiv: HTMLElement) {
  try {
    const unsignedTx = CSL.Transaction.from_bytes(hexToBytes(cborHex));
    const originalWitnessSet = unsignedTx.witness_set();
    const witnessSetHex = await walletApi.signTx(cborHex, true);
    const walletWitnessSet = CSL.TransactionWitnessSet.from_bytes(hexToBytes(witnessSetHex));
    // Clone the original witness set
    const mergedWitnessSet = CSL.TransactionWitnessSet.from_bytes(originalWitnessSet.to_bytes());
    // Merge vkey witnesses: append all from both sets
    const mergedVkeys = CSL.Vkeywitnesses.new();
    const origVkeys = originalWitnessSet.vkeys();
    if (origVkeys) for (let i = 0; i < origVkeys.len(); i++) mergedVkeys.add(origVkeys.get(i));
    const walletVkeys = walletWitnessSet.vkeys();
    if (walletVkeys) for (let i = 0; i < walletVkeys.len(); i++) mergedVkeys.add(walletVkeys.get(i));
    if (mergedVkeys.len() > 0) mergedWitnessSet.set_vkeys(mergedVkeys);
    // Build the signed transaction
    const signedTx = CSL.Transaction.new(
      unsignedTx.body(),
      mergedWitnessSet,
      unsignedTx.auxiliary_data()
    );
    const signedTxHex = bytesToHex(signedTx.to_bytes());
    const txHash = await walletApi.submitTx(signedTxHex);
    resultDiv.textContent = `Transaction signed and submitted with ${walletLabel}! Tx Hash: ${txHash}`;
    return true;
  } catch (err) {
    resultDiv.textContent = `${walletLabel} signing or submission failed: ${err}`;
    return false;
  }
}

// Helper functions for hex <-> bytes
function hexToBytes(hex: string): Uint8Array {
  if (hex.length % 2 !== 0) throw new Error('Invalid hex string');
  const arr = new Uint8Array(hex.length / 2);
  for (let i = 0; i < hex.length; i += 2) {
    arr[i / 2] = parseInt(hex.slice(i, i + 2), 16);
  }
  return arr;
}
function bytesToHex(bytes: Uint8Array): string {
  return Array.from(bytes).map(b => b.toString(16).padStart(2, '0')).join('');
}
