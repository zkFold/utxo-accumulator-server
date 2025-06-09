// api.ts
// Handles network requests and server settings

import { encryptTransaction } from './rsa';

export const serverBases = [
  { label: 'Localhost', base: 'http://localhost:8082' },
  { label: 'Relay 1', base: 'https://relay1.io' },
  { label: 'Relay 2', base: 'https://relay2.io' },
];

export const txEndpoint = '/v0/transaction';
export const serverSettings: Record<string, any> = {};

export async function fetchAllServerSettings() {
  await Promise.all(serverBases.map(async (s) => {
    try {
      const resp = await fetch(s.base + '/v0/settings', {
        headers: { 'api-key': '123456' },
      });
      if (resp.ok) {
        const settings = await resp.json();
        serverSettings[s.base] = settings;
      } else {
        serverSettings[s.base] = null;
      }
    } catch {
      serverSettings[s.base] = null;
    }
  }));
}

export async function sendTransaction(serverBase: string, body: any, settings: any) {
  const b64 = encryptTransaction(body, settings);
  return fetch(serverBase + txEndpoint, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json;charset=utf-8',
      'api-key': '123456',
    },
    body: JSON.stringify({ unEncryptedTransaction: b64 }),
  });
}
