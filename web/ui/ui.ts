// ui.ts
// Handles UI creation and exposes references to key DOM elements

export const title = document.createElement('h1');
title.textContent = 'UTxO Accumulator';

document.body.prepend(title);

export const walletSelect = document.createElement('select');
walletSelect.id = 'wallet-select';
walletSelect.style.width = '350px';

export const serverSelect = document.createElement('select');
serverSelect.id = 'server-select';
serverSelect.style.width = '350px';

export const amountSelect = document.createElement('select');
amountSelect.id = 'amount-select';
amountSelect.style.width = '350px';

export const addressInputGrid = document.createElement('input');
addressInputGrid.type = 'text';
addressInputGrid.id = 'address';
addressInputGrid.name = 'address';
addressInputGrid.style.width = '750px';
addressInputGrid.placeholder = 'addr_test1abcdefghijklmnopqrstuvwxyz...';

export const fillAddrBtn = document.createElement('button');
fillAddrBtn.textContent = 'Use Wallet Address';
fillAddrBtn.type = 'button';
fillAddrBtn.style.marginLeft = '1em';

export const sendBtn = document.createElement('button');
sendBtn.type = 'button';
sendBtn.textContent = 'Send';

export const resultDivGrid = document.createElement('div');
resultDivGrid.id = 'result';
resultDivGrid.style.marginTop = '1em';
resultDivGrid.style.minHeight = '1.5em';
resultDivGrid.style.maxWidth = '750px';
resultDivGrid.style.overflowWrap = 'break-word';

export function buildFormGrid(walletOptions: HTMLOptionElement[], amountOptions: HTMLOptionElement[], serverOptions: HTMLOptionElement[]) {
  // ...builds and returns the formGrid element, appends all controls in order...
  // For brevity, you can fill this in as needed.
}

// Set up the UI layout and structure
export function initUILayout() {
  document.body.innerHTML = '';
  const container = document.createElement('div');
  container.style.display = 'flex';
  container.style.flexDirection = 'column';
  container.style.alignItems = 'flex-start';
  container.style.gap = '1em';
  document.body.appendChild(container);

  // Title
  container.appendChild(title);

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
  walletLabelGrid.textContent = 'Wallet:';
  formGrid.appendChild(walletLabelGrid);
  formGrid.appendChild(walletSelect);

  // Amount row
  const amountLabelGrid = document.createElement('label');
  amountLabelGrid.htmlFor = 'amount-select';
  amountLabelGrid.textContent = 'Amount:';
  formGrid.appendChild(amountLabelGrid);
  formGrid.appendChild(amountSelect);

  // Server row
  const serverLabelGrid = document.createElement('label');
  serverLabelGrid.htmlFor = 'server-select';
  serverLabelGrid.textContent = 'Server:';
  formGrid.appendChild(serverLabelGrid);
  formGrid.appendChild(serverSelect);

  // Address row
  const addressLabelGrid = document.createElement('label');
  addressLabelGrid.htmlFor = 'address';
  addressLabelGrid.textContent = 'Address:';
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
