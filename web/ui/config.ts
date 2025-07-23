// config.ts
// Helper functions for server configuration management

// GitHub configuration
const GITHUB_CONFIG = {
    owner: 'zkFold',
    repo: 'utxo-accumulator-server',
    branch: 'main',
    path: 'web/server/servers_config.yaml'
};

// Server configuration interface
export interface ServerConfig {
    name: string;
    url: string;
    port: number;
    aws_api_url?: string;
}

export interface ServersConfigFile {
    servers: ServerConfig[];
}

// Default fallback servers (in case GitHub fetch fails)
export const fallbackServers = [
    { label: 'Localhost (Fallback)', base: 'http://localhost:8082' }
];

// Function to fetch and parse YAML from GitHub
export async function fetchServersConfigFromGitHub(): Promise<ServerConfig[]> {
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
export function parseYaml(yamlText: string): ServersConfigFile {
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
export function transformServerConfig(servers: ServerConfig[]): { label: string; base: string }[] {
    return servers
        .filter(server => server.aws_api_url) // Only include servers with AWS API URLs
        .map(server => ({
            label: server.name || `${server.url}:${server.port}`,
            base: server.aws_api_url! // Use non-null assertion since we filtered above
        }));
}
