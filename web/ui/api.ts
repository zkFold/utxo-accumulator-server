// api.ts
// Handles network requests and server settings

import { encryptTransaction } from './rsa';

export const serverBases = [
  { label: 'Primary Relay', base: 'https://i7mkyoz28i.execute-api.eu-central-1.amazonaws.com' },
  { label: 'Primary Relay', base: 'https://tsffd900dj.execute-api.eu-central-1.amazonaws.com' },
  { label: 'Localhost', base: 'http://localhost:8082' }
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
