// api.ts
// Handles network requests and server settings

import { encryptTransaction } from './rsa';
import {
  fetchServersConfigFromGitHub,
  transformServerConfig,
  fallbackServers
} from './config';

export let serverBases: { label: string; base: string }[] = fallbackServers;
export const txEndpoint = '/v0/transaction';
export const serverSettings: Record<string, any> = {};

// Load server configuration on module initialization
export async function loadServerConfiguration(): Promise<void> {
  try {
    const servers = await fetchServersConfigFromGitHub();
    serverBases = transformServerConfig(servers);
    console.log('✓ Server configuration loaded successfully:', serverBases);
  } catch (error) {
    console.warn('⚠️ Using fallback server configuration due to error:', error);
    serverBases = fallbackServers;
  }
}

export async function fetchAllServerSettings() {
  // Ensure server configuration is loaded first
  if (serverBases === fallbackServers) {
    await loadServerConfiguration();
  }

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
