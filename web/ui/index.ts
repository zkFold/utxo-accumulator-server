// @ts-ignore: Allow JS import for branding config
import { BRANDING } from './branding';
import { wallets, getWalletApi, getWalletAnyAddress, hexToBech32, signAndSubmitTxWithWallet, WalletApi, WalletInfo } from './wallet';
import { serverBases, fetchAllServerSettings, sendTransaction, serverSettings } from './api';
import { parseAccumulationValue, setResultMessage, clearResultMessage, isValidPreprodBech32Address, randomBlsScalarHex, saveTransaction, resolveRecipientAddress } from './utils';
import { walletSelect, serverSelect, amountSelect, addressInputGrid, sendBtn, resultDivGrid, title, subtitle, initUILayout, removalTimeSelect } from './ui';

// Set up branding: logo, title, styles, and labels
document.title = BRANDING.title;

// Set favicon dynamically from BRANDING.logoUrl
(function setFavicon() {
  let favicon = document.querySelector('link[rel~="icon"]') as HTMLLinkElement | null;
  if (!favicon) {
    favicon = document.createElement('link');
    favicon.rel = 'icon';
    document.head.appendChild(favicon);
  }
  favicon.type = BRANDING.logoUrl.endsWith('.svg') ? 'image/svg+xml' : 'image/png';
  favicon.href = BRANDING.logoUrl;
})();

// Inject style variables
const styleVars = BRANDING.styles;
for (const key in styleVars) {
  document.documentElement.style.setProperty(`--${key.replace(/([A-Z])/g, "-$1").toLowerCase()}`, styleVars[key]);
}

// Set up the UI layout and structure
initUILayout();

// Set button and label texts from branding config
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
  { label: '100 ada', value: '100000000' },
  { label: '1000 ada', value: '1000000000' },
  { label: '10000 ada', value: '10000000000' },
  { label: '1000 INDY', value: 'acf8a16a63d0203e18a1fa1050e21c0754c10ea55c5e24ae15193325.INDY.1000' },
];
amountOptions.forEach(opt => {
  const option = document.createElement('option');
  option.value = opt.value;
  option.textContent = opt.label;
  amountSelect.appendChild(option);
});

// Clear result on any user interaction
walletSelect.addEventListener('input', () => clearResultMessage(resultDivGrid));
serverSelect.addEventListener('input', () => clearResultMessage(resultDivGrid));
amountSelect.addEventListener('input', () => clearResultMessage(resultDivGrid));
addressInputGrid.addEventListener('input', () => clearResultMessage(resultDivGrid));
sendBtn.addEventListener('click', () => clearResultMessage(resultDivGrid));

// Helper to rebuild server dropdown based on selected amount
function rebuildServerDropdown(selectedAmountStr: string) {
  while (serverSelect.firstChild) serverSelect.removeChild(serverSelect.firstChild);
  let found = false;
  serverBases.forEach((s) => {
    const settings = serverSettings[s.base];
    const accVal = settings ? parseAccumulationValue(settings.accumulation_value) : null;
    if (settings && accVal === selectedAmountStr) {
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
let walletLabel: string = wallets[0].label;
walletSelect.onchange = () => {
  walletKey = walletSelect.value;
  walletLabel = wallets.find((w: WalletInfo) => w.key === walletKey)?.label || walletKey;
  walletApi = null; // Force re-enable on wallet change
};

sendBtn.onclick = async () => {
  // Always re-enable walletApi to ensure we get the new wallet context
  walletApi = await getWalletApi(walletKey);
  if (!walletApi) {
    setResultMessage(resultDivGrid, `${walletLabel} wallet not found. Please install or enable it.`);
    return;
  }
  let senderAddress = '';
  try {
    const addrHex = await getWalletAnyAddress(walletApi);
    if (addrHex) {
      const bech32 = hexToBech32(addrHex);
      if (bech32) senderAddress = bech32;
    }
  } catch { }
  if (!senderAddress || !isValidPreprodBech32Address(senderAddress)) {
    setResultMessage(resultDivGrid, 'Could not get a valid Cardano (Preprod testnet) address from the wallet.');
    return;
  }
  let address = addressInputGrid.value;
  try {
    address = await resolveRecipientAddress(address);
  } catch (e) {
    setResultMessage(resultDivGrid, e instanceof Error ? e.message : String(e));
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
    const settings = serverSettings[serverBase];
    const response = await sendTransaction(serverBase, body, settings);
    const data = await response.json();
    if (!response.ok) {
      if (data && data.message) {
        setResultMessage(resultDivGrid, `Error: ${data.message}`);
      } else {
        setResultMessage(resultDivGrid, `Error: ${response.status} ${response.statusText}`);
      }
      return;
    }
    if (walletApi && data && typeof data === 'string') {
      saveTransaction(body, settings);
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
  const selectedAmountStr = amountSelect.value;
  rebuildServerDropdown(selectedAmountStr);
};

removalTimeSelect.addEventListener('change', () => {
  if (removalTimeSelect.value === '') {
    window.alert('Warning: You have selected "No timer". This means the transaction will not be processed automatically. You must have access to the relay server to complete the transaction.');
  }
});

(async () => {
  await fetchAllServerSettings();
  const initialAmountStr = amountSelect.value;
  rebuildServerDropdown(initialAmountStr);
})();
