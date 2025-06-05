// @ts-ignore: Allow JS import for branding config
import { BRANDING } from './branding';
import { wallets, getWalletApi, getWalletAnyAddress, hexToBech32, signAndSubmitTxWithWallet, WalletApi, WalletInfo } from './wallet';
import { serverBases, fetchAllServerSettings, sendTransaction, serverSettings, txEndpoint } from './api';
import { parseAccumulationValue, setResultMessage, clearResultMessage, isValidPreprodBech32Address, randomBlsScalarHex } from './utils';
import { walletSelect, serverSelect, amountSelect, addressInputGrid, fillAddrBtn, sendBtn, resultDivGrid, title, initUILayout, removalTimeSelect } from './ui';

// Set up branding: logo, title, styles, and labels
document.getElementById('app-title')!.textContent = BRANDING.appTitle;
document.title = BRANDING.appTitle;
const logoEl = document.getElementById('app-logo') as HTMLImageElement;
if (BRANDING.logoUrl) {
  logoEl.src = BRANDING.logoUrl;
  logoEl.style.display = 'inline';
} else {
  logoEl.style.display = 'none';
}
// Inject style variables
const styleVars = BRANDING.styles;
for (const key in styleVars) {
  document.documentElement.style.setProperty(`--${key.replace(/([A-Z])/g, "-$1").toLowerCase()}`, styleVars[key]);
}

// Set up the UI layout and structure
initUILayout();

// Apply branding to UI labels and buttons
title.textContent = BRANDING.appTitle;
document.title = BRANDING.appTitle;

// Set button and label texts from branding config
fillAddrBtn.textContent = BRANDING.labels.useWalletAddress || 'Use Wallet Address';
sendBtn.textContent = BRANDING.labels.send || 'Send';
// You can add more label assignments as needed

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
  walletApi = null; // Force re-enable on wallet change
};

fillAddrBtn.onclick = async () => {
  if (!walletKey) return;
  // Always re-enable walletApi to ensure we get the new wallet context
  walletApi = await getWalletApi(walletKey);
  if (!walletApi) return;
  try {
    const addrHex = await getWalletAnyAddress(walletApi);
    if (addrHex) {
      const bech32 = hexToBech32(addrHex);
      if (bech32) addressInputGrid.value = bech32;
    }
  } catch {}
};

sendBtn.onclick = async () => {
  // Always re-enable walletApi to ensure we get the new wallet context
  walletApi = await getWalletApi(walletKey);
  if (!walletApi) {
    setResultMessage(resultDivGrid, `${walletKey} wallet not found. Please install or enable it.`);
    return;
  }
  let senderAddress = '';
  try {
    const addrHex = await getWalletAnyAddress(walletApi);
    if (addrHex) {
      const bech32 = hexToBech32(addrHex);
      if (bech32) senderAddress = bech32;
    }
  } catch {}
  if (!senderAddress || !isValidPreprodBech32Address(senderAddress)) {
    setResultMessage(resultDivGrid, 'Could not get a valid Cardano (Preprod testnet) address from the wallet.');
    return;
  }
  const address = addressInputGrid.value;
  if (!isValidPreprodBech32Address(address)) {
    setResultMessage(resultDivGrid, 'Please enter a valid Cardano (Preprod testnet) bech32 address.');
    return;
  }
  setResultMessage(resultDivGrid, 'Sending...');
  const serverBase = serverSelect.value;
  if (!serverBase) {
    setResultMessage(resultDivGrid, 'No server selected.');
    return;
  }
  // Compute POSIX time for removal, or null
  let tx_distribution_time: number | null = null;
  const removalSeconds = Number(removalTimeSelect.value);
  if (removalSeconds > 0) {
    tx_distribution_time = Math.floor(Date.now() / 1000) + removalSeconds;
  }
  const nonceL = randomBlsScalarHex();
  const nonceR = randomBlsScalarHex();
  const body = {
    tx_sender: senderAddress,
    tx_recipient: address,
    tx_nonce_l: `0x${nonceL}`,
    tx_nonce_r: `0x${nonceR}`,
    tx_distribution_time: tx_distribution_time === null ? null : tx_distribution_time
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
