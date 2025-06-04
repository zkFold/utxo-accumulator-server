import { wallets, getWalletApi, getWalletUsedAddress, hexToBech32, signAndSubmitTxWithWallet, WalletApi, WalletInfo } from './wallet';
import { serverBases, fetchAllServerSettings, sendTransaction, serverSettings, txEndpoint } from './api';
import { parseAccumulationValue, setResultMessage, clearResultMessage, isValidPreprodBech32Address } from './utils';
import { walletSelect, serverSelect, amountSelect, addressInputGrid, fillAddrBtn, sendBtn, resultDivGrid, title, initUILayout } from './ui';

// Set up the UI layout and structure
initUILayout();

// --- UI Initialization ---
// Populate wallet dropdown
wallets.forEach((w: WalletInfo) => {
  const opt = document.createElement('option');
  opt.value = w.key;
  opt.textContent = w.label;
  walletSelect.appendChild(opt);
});

// Populate amount dropdown
const amountOptions = [
  { label: '100 ada', value: 100_000_000 },
  { label: '1000 ada', value: 1_000_000_000 },
  { label: '10000 ada', value: 10_000_000_000 },
];
amountOptions.forEach(opt => {
  const option = document.createElement('option');
  option.value = String(opt.value);
  option.textContent = opt.label;
  amountSelect.appendChild(option);
});

// Clear result on any user interaction
walletSelect.addEventListener('input', () => clearResultMessage(resultDivGrid));
serverSelect.addEventListener('input', () => clearResultMessage(resultDivGrid));
amountSelect.addEventListener('input', () => clearResultMessage(resultDivGrid));
addressInputGrid.addEventListener('input', () => clearResultMessage(resultDivGrid));
fillAddrBtn.addEventListener('click', () => clearResultMessage(resultDivGrid));
sendBtn.addEventListener('click', () => clearResultMessage(resultDivGrid));

// Helper to rebuild server dropdown based on selected amount
function rebuildServerDropdown(selectedAmount: number) {
  while (serverSelect.firstChild) serverSelect.removeChild(serverSelect.firstChild);
  let found = false;
  serverBases.forEach((s) => {
    const settings = serverSettings[s.base];
    const accVal = settings ? parseAccumulationValue(settings.accumulation_value) : null;
    if (settings && accVal === selectedAmount) {
      const opt = document.createElement('option');
      opt.value = s.base;
      opt.textContent = s.label;
      serverSelect.appendChild(opt);
      if (!found) found = true;
    }
  });
  if (!found) {
    const opt = document.createElement('option');
    opt.value = '';
    opt.textContent = 'No server available for this amount';
    opt.disabled = true;
    opt.selected = true;
    serverSelect.appendChild(opt);
  }
}

let walletApi: WalletApi = null;
let walletKey: string = wallets[0].key;
walletSelect.onchange = () => {
  walletKey = walletSelect.value;
  walletApi = null;
};

fillAddrBtn.onclick = async () => {
  if (!walletKey) return;
  try {
    walletApi = walletApi || await getWalletApi(walletKey);
    if (!walletApi) return;
    const addrHex = await getWalletUsedAddress(walletApi);
    if (addrHex) {
      const bech32 = hexToBech32(addrHex);
      if (bech32) addressInputGrid.value = bech32;
    }
  } catch {}
};

sendBtn.onclick = async () => {
  const address = addressInputGrid.value;
  if (!isValidPreprodBech32Address(address)) {
    setResultMessage(resultDivGrid, 'Please enter a valid Cardano (Preprod testnet) bech32 address.');
    return;
  }
  setResultMessage(resultDivGrid, 'Sending...');
  walletApi = walletApi || await getWalletApi(walletKey);
  if (!walletApi) {
    setResultMessage(resultDivGrid, `${walletKey} wallet not found. Please install or enable it.`);
    return;
  }
  const amount = Number(amountSelect.value);
  const serverBase = serverSelect.value;
  if (!serverBase) {
    setResultMessage(resultDivGrid, 'No server selected.');
    return;
  }
  const body = {
    tx_sender: address,
    tx_recipient: address,
    tx_nonce: amount
  };
  try {
    const response = await sendTransaction(serverBase, body);
    if (!response.ok) {
      setResultMessage(resultDivGrid, `Error: ${response.status} ${response.statusText}`);
      return;
    }
    const data = await response.json();
    if (walletApi && data && typeof data === 'string') {
      const ok = await signAndSubmitTxWithWallet(walletApi, data, wallets.find((w: WalletInfo) => w.key === walletKey)?.label || walletKey, resultDivGrid);
      if (ok) return;
      setResultMessage(resultDivGrid, 'Signing cancelled.');
      return;
    }
    setResultMessage(resultDivGrid, 'Success! Transaction: ' + JSON.stringify(data));
  } catch (err) {
    setResultMessage(resultDivGrid, 'Request failed: ' + err);
  }
};

amountSelect.onchange = () => {
  const selectedAmount = Number(amountSelect.value);
  rebuildServerDropdown(selectedAmount);
};

(async () => {
  await fetchAllServerSettings();
  const initialAmount = Number(amountSelect.value);
  rebuildServerDropdown(initialAmount);
})();
