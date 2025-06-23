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
  { label: 'No timer', value: '' },
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
addressInputGrid.style.width = '825px';
addressInputGrid.placeholder = 'addr_test1abcdefghijklmnopqrstuvwxyz...';

export const fillAddrBtn = document.createElement('button');
fillAddrBtn.textContent = BRANDING.labels.useWalletAddress || 'Use Wallet Address';
fillAddrBtn.type = 'button';
fillAddrBtn.title = 'Fill the address field with your currently selected wallet address.';

export const sendBtn = document.createElement('button');
sendBtn.type = 'button';
sendBtn.textContent = BRANDING.labels.send || 'Send';

export const downloadTxsBtn = document.createElement('button');
downloadTxsBtn.type = 'button';
downloadTxsBtn.title = 'Download all saved transaction requests as a JSON file.';

// Add SVG download icon
const downloadIcon = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
downloadIcon.setAttribute('width', '18');
downloadIcon.setAttribute('height', '18');
downloadIcon.setAttribute('viewBox', '0 0 20 20');
downloadIcon.setAttribute('fill', 'none');
downloadIcon.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
downloadIcon.style.verticalAlign = 'middle';
downloadIcon.style.marginRight = '0.5em';
downloadIcon.innerHTML = `
  <path d="M10 2v10m0 0l-4-4m4 4l4-4" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
  <rect x="4" y="16" width="12" height="2" rx="1" fill="currentColor"/>
`;
downloadTxsBtn.appendChild(downloadIcon);
downloadTxsBtn.appendChild(document.createTextNode(BRANDING.labels.downloadTxs || 'Download Saved Transactions'));

export const resultDivGrid = document.createElement('div');
resultDivGrid.id = 'result';
resultDivGrid.style.marginTop = '1em';
resultDivGrid.style.minHeight = '1.5em';
resultDivGrid.style.maxWidth = '750px';
resultDivGrid.style.overflowWrap = 'break-word';

// --- HEADER CREATION ---
export const logo = document.createElement('img');
logo.src = BRANDING.logoUrl;
logo.alt = BRANDING.logoAlt || 'Logo';
logo.className = 'app-logo';
logo.style.height = '48px';
logo.style.width = '48px';
logo.style.objectFit = 'contain';
logo.style.marginRight = '1.2em';

export const header = document.createElement('div');
header.className = 'app-header';
header.style.display = 'flex';
header.style.alignItems = 'center';
header.style.gap = '1.2em';

const networkLabel = document.createElement('span');
networkLabel.className = 'app-network-label';
networkLabel.textContent = BRANDING.networkName || 'Preprod';

const titleBlock = document.createElement('div');
titleBlock.className = 'app-title-block';

const titleRow = document.createElement('div');
titleRow.className = 'app-title-row'; // Use a class for CSS control

titleRow.appendChild(title);
titleRow.appendChild(networkLabel);

titleBlock.appendChild(titleRow);
titleBlock.appendChild(subtitle);

// Network label (e.g., 'Preprod') next to the title
networkLabel.style.marginLeft = '1em';
networkLabel.style.fontSize = '0.7em';
networkLabel.style.fontWeight = '400';
networkLabel.style.color = 'var(--subtitle-color, #888)';
networkLabel.style.whiteSpace = 'nowrap';

header.appendChild(logo);
header.appendChild(titleBlock);

// Set up the UI layout and structure
export function initUILayout() {
  document.body.innerHTML = '';
  // Create a single flex column container for header and content
  const pageContainer = document.createElement('div');
  pageContainer.style.display = 'flex';
  pageContainer.style.flexDirection = 'column';
  pageContainer.style.alignItems = 'center';
  pageContainer.style.justifyContent = 'center';
  pageContainer.style.minHeight = '100vh';
  pageContainer.style.width = '100%';

  // Add header to the container
  pageContainer.appendChild(header);

  // Main content container
  const container = document.createElement('div');
  container.style.display = 'flex';
  container.style.flexDirection = 'column';
  container.style.alignItems = 'center';
  container.style.gap = '1em';

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

  container.appendChild(formGrid);
  pageContainer.appendChild(container);
  document.body.appendChild(pageContainer);

  // Add footer only if branding footer or email is available
  const footerBranding = (BRANDING as any).footer;
  if (footerBranding) {
    const footer = document.createElement('footer');
    footer.style.marginTop = '2em';
    footer.style.textAlign = 'center';
    footer.style.color = '#fff';
    footer.style.fontSize = '0.95em';
    footer.style.width = '100%';
    footer.style.position = 'fixed';
    footer.style.left = '0';
    footer.style.bottom = '0';
    footer.style.background = '#111';
    footer.style.zIndex = '100';
    footer.innerHTML = footerBranding.copyright;

    const emailLink = document.createElement('a');
    emailLink.href = `mailto:${footerBranding.email}`;
    emailLink.style.color = '#fff';
    emailLink.style.textDecoration = 'underline';
    emailLink.textContent = footerBranding.email;
    footer.appendChild(document.createTextNode(' | To become a relayer, use '));
    footer.appendChild(emailLink);

    // --- Add Download Saved Transactions link to footer ---
    const downloadLink = document.createElement('a');
    downloadLink.href = '#';
    downloadLink.style.color = '#fff';
    downloadLink.style.textDecoration = 'underline';
    downloadLink.style.display = 'inline';
    downloadLink.textContent = 'here';
    downloadLink.addEventListener('click', (e) => {
      e.preventDefault();
      const txs = localStorage.getItem('transactions') || '[]';
      const blob = new Blob([txs], { type: 'application/json' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = 'transactions.json';
      document.body.appendChild(a);
      a.click();
      setTimeout(() => {
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
      }, 0);
    });

    footer.appendChild(document.createTextNode(' | To download saved transactions, click '));
    footer.appendChild(downloadLink);
    footer.appendChild(document.createTextNode('.'));
    document.body.appendChild(footer);
  }

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
