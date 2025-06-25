import * as CSL from '@emurgo/cardano-serialization-lib-browser';
import { hexToBytes, bytesToHex } from './utils';

export type WalletApi = any;
export type WalletInfo = { label: string; key: string };

export const wallets: WalletInfo[] = [
  { label: 'Lace', key: 'lace' },
  { label: 'Eternl', key: 'eternl' },
  { label: 'Yoroi', key: 'yoroi' },
  { label: 'Gero', key: 'gerowallet' },
  { label: 'Begin', key: 'begin' },
  { label: 'Typhon', key: 'typhoncip30' },
];

export async function getWalletApi(walletKey: string): Promise<WalletApi | null> {
  const cardano = (window as any).cardano;
  if (!cardano?.[walletKey]) return null;
  return await cardano[walletKey].enable();
}

export async function getWalletUsedAddress(walletApi: WalletApi): Promise<string | null> {
  const used = await walletApi.getUsedAddresses();
  if (used && used.length > 0) return used[0];
  return null;
}

export function hexToBech32(addr: string): string | null {
  if (CSL && CSL.Address && /^[0-9a-fA-F]+$/.test(addr)) {
    const matches = addr.match(/.{1,2}/g);
    if (!matches) return null;
    const bytes = Uint8Array.from(matches.map((x: string) => parseInt(x, 16)));
    return CSL.Address.from_bytes(bytes).to_bech32();
  }
  return null;
}

export async function signAndSubmitTxWithWallet(walletApi: WalletApi, cborHex: string, walletLabel: string, resultDiv: HTMLElement) {
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

export async function getWalletAnyAddress(walletApi: WalletApi): Promise<string | null> {
  // Try used addresses first
  let addrs = await walletApi.getUsedAddresses();
  if (addrs && addrs.length > 0) return Array.isArray(addrs) ? addrs[0] : addrs;
  // Then unused addresses
  addrs = await walletApi.getUnusedAddresses();
  if (addrs && addrs.length > 0) return Array.isArray(addrs) ? addrs[0] : addrs;
  // Then change addresses
  addrs = await walletApi.getChangeAddress ? [await walletApi.getChangeAddress()] : [];
  if (addrs && addrs.length > 0 && addrs[0]) return addrs[0];
  return null;
}
