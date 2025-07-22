// api.ts
// Handles network requests and server settings

import { encryptTransaction } from './rsa';

// GitHub configuration
const GITHUB_CONFIG = {
  owner: 'zkFold',
  repo: 'utxo-accumulator-server',
  branch: 'main',
  path: 'web/server/servers_config.yaml'
};

// Server configuration interface
interface ServerConfig {
  name: string;
  url: string;
  port: number;
  aws_api_url?: string;
}

interface ServersConfigFile {
  servers: ServerConfig[];
}

// Default fallback servers (in case GitHub fetch fails)
const fallbackServers = [
  { label: 'Localhost (Fallback)', base: 'http://localhost:8082' }
];

export let serverBases: { label: string; base: string }[] = fallbackServers;
export const txEndpoint = '/v0/transaction';
export const serverSettings: Record<string, any> = {};

// Function to fetch and parse YAML from GitHub
async function fetchServersConfigFromGitHub(): Promise<ServerConfig[]> {
  const url = `https://raw.githubusercontent.com/${GITHUB_CONFIG.owner}/${GITHUB_CONFIG.repo}/${GITHUB_CONFIG.branch}/${GITHUB_CONFIG.path}`;

  try {
    console.log('Fetching server configuration from GitHub...');
    const response = await fetch(url);

    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }

    const yamlText = await response.text();
    const config = parseYaml(yamlText) as ServersConfigFile;

    if (!config.servers || !Array.isArray(config.servers)) {
      throw new Error('Invalid configuration format: missing servers array');
    }

    console.log(`✓ Loaded ${config.servers.length} servers from GitHub`);
    return config.servers;

  } catch (error) {
    console.error('Failed to fetch server configuration from GitHub:', error);
    throw error;
  }
}

// Simple YAML parser for our specific use case
function parseYaml(yamlText: string): ServersConfigFile {
  const lines = yamlText.split('\n');
  const servers: ServerConfig[] = [];
  let currentServer: Partial<ServerConfig> = {};
  let inServersArray = false;

  for (const line of lines) {
    const trimmed = line.trim();

    if (trimmed === 'servers:') {
      inServersArray = true;
      continue;
    }

    if (!inServersArray) continue;

    if (trimmed.startsWith('- name:')) {
      // Save previous server if complete
      if (currentServer.name && currentServer.url && currentServer.port) {
        servers.push(currentServer as ServerConfig);
      }
      // Start new server
      currentServer = {
        name: trimmed.replace('- name:', '').trim().replace(/['"]/g, '')
      };
    } else if (trimmed.startsWith('name:')) {
      currentServer.name = trimmed.replace('name:', '').trim().replace(/['"]/g, '');
    } else if (trimmed.startsWith('url:')) {
      currentServer.url = trimmed.replace('url:', '').trim().replace(/['"]/g, '');
    } else if (trimmed.startsWith('port:')) {
      currentServer.port = parseInt(trimmed.replace('port:', '').trim());
    } else if (trimmed.startsWith('aws_api_url:')) {
      currentServer.aws_api_url = trimmed.replace('aws_api_url:', '').trim().replace(/['"]/g, '');
    }
  }

  // Add the last server
  if (currentServer.name && currentServer.url && currentServer.port) {
    servers.push(currentServer as ServerConfig);
  }

  return { servers };
}

// Transform server config to the format expected by the frontend
function transformServerConfig(servers: ServerConfig[]): { label: string; base: string }[] {
  return servers
    .filter(server => server.aws_api_url) // Only include servers with AWS API URLs
    .map(server => ({
      label: server.name || `${server.url}:${server.port}`,
      base: server.aws_api_url! // Use non-null assertion since we filtered above
    }));
}

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
