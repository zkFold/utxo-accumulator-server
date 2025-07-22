# UTxO Accumulator API Gateway Management

This directory contains scripts to manage AWS API Gateway APIs for UTxO Accumulator servers.

## Files

- `manage_apis.py` - Main management script
- `server_api.yaml` - Template API specification (your existing file)
- `servers_config.yaml` - Configuration file listing your servers
- `requirements.txt` - Python dependencies

## Setup

1. **Install dependencies:**
   ```bash
   pip install -r requirements.txt
   ```

2. **Configure AWS credentials:**
   ```bash
   aws configure
   ```
   Or set environment variables:
   ```bash
   export AWS_ACCESS_KEY_ID=your_access_key
   export AWS_SECRET_ACCESS_KEY=your_secret_key
   export AWS_DEFAULT_REGION=eu-central-1
   ```

3. **Edit `servers_config.yaml`** to include your server URLs and ports:
   ```yaml
   servers:
     - name: "Primary Server"      # Optional user-friendly name
       url: "your-server1.com"
       port: 8084
     - name: "Backup Server"       # Optional user-friendly name
       url: "your-server2.com"
       port: 8084
     - url: "your-server3.com"     # Name field is optional
       port: 8085
   ```

   The `name` field is optional and provides a user-friendly display name for the server.

## Usage

Run the script to delete old APIs and create new ones:

```bash
python3 manage_apis.py
```

The script will:
1. ✅ Delete all existing APIs with `utxo-accumulator--` in the title
2. ✅ Create new APIs for each server in `servers_config.yaml`
3. ✅ Use `server_api.yaml` as template, updating title and backend URI
4. ✅ Display all created API endpoints

## API Naming Convention

APIs are named: `utxo-accumulator--{url}--{port}`

Example: `utxo-accumulator--ec2-3-76-34-185.eu-central-1.compute.amazonaws.com--8084`

## Help

```bash
python3 manage_apis.py --help
```

## Example Output

```
🎯 UTxO Accumulator API Gateway Manager
==================================================
✓ AWS API Gateway client initialized
✓ Loaded template from server_api.yaml
✓ Loaded 3 servers from servers_config.yaml

🗑️  Searching for existing UTxO Accumulator APIs...
Found 2 APIs to delete:
  - utxo-accumulator--old-server--8084 (ID: abc123)
  - utxo-accumulator--old-server2--8084 (ID: def456)
✓ Cleanup completed. Deleted 2 APIs

🏗️  Creating 3 new APIs...

🚀 Creating API for: Frankfurt Server (server1.com:8084)
   API Name: utxo-accumulator--server1.com--8084
✓ Created API: utxo-accumulator--server1.com--8084
  API ID: xyz789
  Endpoint: https://xyz789.execute-api.eu-central-1.amazonaws.com/
  Backend: http://server1.com:8084

📊 Summary:
✓ Successfully created 3 APIs

🔗 API Endpoints:
  Frankfurt Server
    API Name: utxo-accumulator--server1.com--8084
    Endpoint: https://xyz789.execute-api.eu-central-1.amazonaws.com/
    Backend:  http://server1.com:8084
```
