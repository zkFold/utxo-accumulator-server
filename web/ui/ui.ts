import { BRANDING } from './branding';

// ui.ts
// Handles UI creation and exposes references to key DOM elements

export const title = document.createElement('h1');
title.textContent = BRANDING.title;

export const subtitle = document.createElement('div');
subtitle.textContent = BRANDING.subtitle;
subtitle.className = 'app-subtitle';

export const walletSelect = document.createElement('select');
walletSelect.id = 'wallet-select';
walletSelect.style.width = '350px';

export const serverSelect = document.createElement('select');
serverSelect.id = 'server-select';
serverSelect.style.width = '350px';

export const amountSelect = document.createElement('select');
amountSelect.id = 'amount-select';
amountSelect.style.width = '350px';

// --- UTxO Removal Timer Dropdown ---
export const removalTimeSelect = document.createElement('select');
removalTimeSelect.id = 'removalTimeSelect';
removalTimeSelect.style.width = '350px';
const removalOptions = [
  { label: '1 hour from now', value: '3600', default: true },
  { label: '1 day from now', value: String(24 * 3600) },
  { label: '1 week from now', value: String(7 * 24 * 3600) },
  { label: 'No timer (removed manually)', value: '' },
];
removalOptions.forEach(opt => {
  const option = document.createElement('option');
  option.value = opt.value;
  option.textContent = opt.label;
  if (opt.default) option.selected = true;
  removalTimeSelect.appendChild(option);
});

export const addressInputGrid = document.createElement('input');
addressInputGrid.type = 'text';
addressInputGrid.id = 'address';
addressInputGrid.name = 'address';
addressInputGrid.style.width = '750px';
addressInputGrid.placeholder = 'addr_test1abcdefghijklmnopqrstuvwxyz...';

export const fillAddrBtn = document.createElement('button');
fillAddrBtn.textContent = BRANDING.labels.useWalletAddress || 'Use Wallet Address';
fillAddrBtn.type = 'button';
fillAddrBtn.style.marginLeft = '1em';

export const sendBtn = document.createElement('button');
sendBtn.type = 'button';
sendBtn.textContent = BRANDING.labels.send || 'Send';

export const resultDivGrid = document.createElement('div');
resultDivGrid.id = 'result';
resultDivGrid.style.marginTop = '1em';
resultDivGrid.style.minHeight = '1.5em';
resultDivGrid.style.maxWidth = '750px';
resultDivGrid.style.overflowWrap = 'break-word';

// Set up the UI layout and structure
export function initUILayout() {
  document.body.innerHTML = '';
  const container = document.createElement('div');
  container.style.display = 'flex';
  container.style.flexDirection = 'column';
  container.style.alignItems = 'flex-start';
  container.style.gap = '1em';
  document.body.appendChild(container);

  // Title and subtitle
  container.appendChild(title);
  container.appendChild(subtitle);

  // Two-column layout for labels and inputs
  const formGrid = document.createElement('div');
  formGrid.style.display = 'grid';
  formGrid.style.gridTemplateColumns = 'max-content 1fr';
  formGrid.style.gap = '1em 1.5em';
  formGrid.style.alignItems = 'center';
  formGrid.style.justifyItems = 'start';

  // Wallet row
  const walletLabelGrid = document.createElement('label');
  walletLabelGrid.htmlFor = 'wallet-select';
  walletLabelGrid.textContent = BRANDING.labels.wallet || 'Wallet:';
  formGrid.appendChild(walletLabelGrid);
  formGrid.appendChild(walletSelect);

  // Amount row
  const amountLabelGrid = document.createElement('label');
  amountLabelGrid.htmlFor = 'amount-select';
  amountLabelGrid.textContent = BRANDING.labels.amount || 'Amount:';
  formGrid.appendChild(amountLabelGrid);
  formGrid.appendChild(amountSelect);

  // --- UTxO Removal Timer row ---
  const removalTimeLabelGrid = document.createElement('label');
  removalTimeLabelGrid.htmlFor = 'removalTimeSelect';
  removalTimeLabelGrid.textContent = BRANDING.labels.timer || 'Transaction timer:';
  formGrid.appendChild(removalTimeLabelGrid);
  formGrid.appendChild(removalTimeSelect);

  // Server row
  const serverLabelGrid = document.createElement('label');
  serverLabelGrid.htmlFor = 'server-select';
  serverLabelGrid.textContent = BRANDING.labels.server || 'Server:';
  formGrid.appendChild(serverLabelGrid);
  formGrid.appendChild(serverSelect);

  // Address row
  const addressLabelGrid = document.createElement('label');
  addressLabelGrid.htmlFor = 'address';
  addressLabelGrid.textContent = BRANDING.labels.address || 'Address:';
  formGrid.appendChild(addressLabelGrid);
  formGrid.appendChild(addressInputGrid);

  // Buttons row (empty label for alignment)
  const emptyLabelGrid = document.createElement('div');
  formGrid.appendChild(emptyLabelGrid);
  const buttonRowGrid = document.createElement('div');
  buttonRowGrid.style.display = 'flex';
  buttonRowGrid.style.alignItems = 'center';
  buttonRowGrid.style.gap = '1em';
  buttonRowGrid.appendChild(fillAddrBtn);
  buttonRowGrid.appendChild(sendBtn);
  formGrid.appendChild(buttonRowGrid);

  // Result row (empty label for alignment)
  const emptyLabelResultGrid = document.createElement('div');
  formGrid.appendChild(emptyLabelResultGrid);
  formGrid.appendChild(resultDivGrid);

  container.appendChild(formGrid);

  // Center the content of the page
  container.style.alignItems = 'center';
  container.style.justifyContent = 'center';
  container.style.margin = '0 auto';
  container.style.minHeight = '100vh';

  // Remove vertical scrollbar if not needed
  const style = document.createElement('style');
  style.innerHTML = `
    html, body {
      height: 100%;
      margin: 0;
      padding: 0;
      overflow-y: auto;
    }
    body {
      min-height: 100vh;
      box-sizing: border-box;
    }
    body:has(> div) {
      overflow-y: hidden;
    }
  `;
  document.head.appendChild(style);
}
