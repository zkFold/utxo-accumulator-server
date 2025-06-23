// utils.ts
// Helper functions for parsing, result display, etc.

export function parseAccumulationValue(val: any): number | null {
  if (typeof val === 'string') {
    const m = val.match(/GYLovelace,\s*(\d+)/);
    if (m) return Number(m[1]);
  }
  return null;
}

export function setResultMessage(resultDiv: HTMLElement, msg: string) {
  resultDiv.textContent = msg;
}

// Utility to clear the result message
export function clearResultMessage(resultDiv: HTMLElement) {
  resultDiv.textContent = '';
}

// Utility to check if a string is a valid Cardano preprod testnet bech32 address
export function isValidPreprodBech32Address(addr: string): boolean {
  if (!addr || typeof addr !== 'string') return false;
  if (!addr.startsWith('addr_test1')) return false;
  if (addr.length < 15 || addr.length > 120) return false;
  if (!/^addr_test1[0-9a-z]+$/.test(addr)) return false;
  return true;
}

// Helper functions for hex <-> bytes
export function hexToBytes(hex: string): Uint8Array {
  if (hex.length % 2 !== 0) throw new Error('Invalid hex string');
  const arr = new Uint8Array(hex.length / 2);
  for (let i = 0; i < hex.length; i += 2) {
    arr[i / 2] = parseInt(hex.slice(i, i + 2), 16);
  }
  return arr;
}

export function bytesToHex(bytes: Uint8Array): string {
  return Array.from(bytes).map(b => b.toString(16).padStart(2, '0')).join('');
}

// Generate a random BLS scalar in hex format
export function randomBlsScalarHex() {
  const arr = new Uint8Array(28); // 224 bits
  window.crypto.getRandomValues(arr);
  return Array.from(arr).map(b => b.toString(16).padStart(2, '0')).join('');
}

// Types matching backend AccumulatorData
export interface AccumulatorDataKey {
  adkAddress: string;
  adkNonceL: string;
}
export interface AccumulatorDataItem {
  adiNonceR: string;
  adiDistributionTime: number | null;
  adiThreadTokenRef: string;
}

// Save as array of [AccumulatorDataKey, AccumulatorDataItem] pairs
export function loadSavedTransactions(): [AccumulatorDataKey, AccumulatorDataItem][] {
  return JSON.parse(localStorage.getItem('transactions') || '[]');
}

export function saveTransaction(body: any, settings: any) {
  // Build AccumulatorDataKey and AccumulatorDataItem
  const key: AccumulatorDataKey = {
    adkAddress: body.tx_recipient,
    adkNonceL: body.tx_nonce_l
  };
  const item: AccumulatorDataItem = {
    adiNonceR: body.tx_nonce_r,
    adiDistributionTime: body.tx_distribution_time ?? null,
    adiThreadTokenRef: settings.thread_token_ref
  };
  const saved = loadSavedTransactions();
  saved.push([key, item]);
  localStorage.setItem('transactions', JSON.stringify(saved));
}
