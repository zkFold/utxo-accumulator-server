import forge from 'node-forge';

// Convert decimal string to hex string for node-forge
function decToHex(decStr: string): string {
  return BigInt(decStr).toString(16);
}

// Accepts settings as argument to avoid refetching
export function encryptTransaction(transaction: object, settings: any): string {
  const nHex = decToHex(settings.rsa_public_key_n);
  const eHex = decToHex(settings.rsa_public_key_e);

  // 2. Construct forge BigIntegers
  const n = new forge.jsbn.BigInteger(nHex, 16);
  const e = new forge.jsbn.BigInteger(eHex, 16);
  const publicKey = forge.pki.setRsaPublicKey(n, e);

  // 3. Serialize and encrypt the transaction using PKCS#1 v1.5 padding
  const json = JSON.stringify(transaction);
  const encrypted = publicKey.encrypt(json, 'RSAES-PKCS1-V1_5');
  const b64 = forge.util.encode64(encrypted);
  return b64;
}
