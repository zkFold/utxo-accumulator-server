// utils.ts
// Helper functions for parsing, result display, etc.

export function parseAccumulationValue(val: string): string | null {
  if (typeof val !== 'string') return null;
  // ADA: GYLovelace, 1000000000
  let m = val.match(/GYLovelace,\s*(\d+)/);
  if (m) return m[1];
  // Single asset: valueFromList [(GYToken "<policy>" "<asset>",amount)]
  m = val.match(/valueFromList \[\(GYToken \"([0-9a-fA-F]+)\" \"([A-Za-z0-9_]+)\",(\d+)\)\]/);
  if (m) return `${m[1]}.${m[2]}.${m[3]}`;
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

// Resolves a recipient address: returns bech32 if valid, otherwise resolves Ada Handle (e.g. $bob or $bob.cardano)
export async function resolveRecipientAddress(input: string): Promise<string> {
  // If already a valid bech32 address, return as-is
  if (isValidPreprodBech32Address(input)) return input;
  // Ada Handle: starts with $ and is alphanumeric (optionally .cardano)
  const handleMatch = input.match(/^\$([a-zA-Z0-9_]+)(\.cardano)?$/);
  if (handleMatch) {
    const handle = handleMatch[1].toLowerCase();
    try {
      const resp = await fetch(`https://preprod.api.handle.me/handles/${handle}?hex=false`);
      if (!resp.ok) throw new Error('Ada Handle not found');
      const data = await resp.json();
      if (data && data.resolved_addresses
        && typeof data.resolved_addresses.ada === 'string'
        && isValidPreprodBech32Address(data.resolved_addresses.ada)) {
        return data.address;
      } else {
        throw new Error('Ada Handle did not resolve to a valid address');
      }
    } catch (e) {
      throw new Error('Failed to resolve Ada Handle: ' + (e instanceof Error ? e.message : e));
    }
  }
  throw new Error('Input is not a valid Cardano address or Ada Handle');
}
