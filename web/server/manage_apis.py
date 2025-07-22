#!/usr/bin/env python3
"""
AWS API Gateway v2 Management Script for UTxO Accumulator

This script:
1. Deletes all existing HTTP APIs with 'utxo-accumulator--' in the title
2. Creates new HTTP APIs for each server defined in servers_config.yaml
3. Configures CORS, integrations, and routes programmatically
"""

import boto3
import yaml
import sys
import time
from botocore.exceptions import ClientError


class APIGatewayManager:
    def __init__(self):
        try:
            self.client = boto3.client('apigatewayv2')
            print("✓ AWS API Gateway v2 (HTTP API) client initialized")
        except Exception as e:
            print(f"✗ Failed to initialize AWS client: {e}")
            print("Make sure AWS credentials are configured (aws configure)")
            sys.exit(1)

    def load_servers_config(self, config_path="servers_config.yaml"):
        """Load server configuration from YAML file"""
        try:
            with open(config_path, 'r') as file:
                config = yaml.safe_load(file)
            servers = config.get('servers', [])
            print(f"✓ Loaded {len(servers)} servers from {config_path}")
            return servers
        except FileNotFoundError:
            print(f"✗ Config file {config_path} not found")
            sys.exit(1)
        except yaml.YAMLError as e:
            print(f"✗ Error parsing YAML config: {e}")
            sys.exit(1)

    def delete_existing_apis(self):
        """Delete all HTTP APIs with 'utxo-accumulator--' in the title"""
        try:
            print("\n🗑️  Searching for existing UTxO Accumulator APIs...")

            # Get all HTTP APIs
            response = self.client.get_apis(MaxResults='500')
            apis_to_delete = []

            for api in response.get('Items', []):
                if 'utxo-accumulator--' in api['Name']:
                    apis_to_delete.append(api)

            if not apis_to_delete:
                print("✓ No existing UTxO Accumulator APIs found")
                return

            print(f"Found {len(apis_to_delete)} APIs to delete:")
            for api in apis_to_delete:
                print(f"  - {api['Name']} (ID: {api['ApiId']})")

            # Delete each API with persistent retry
            deleted_count = 0
            for i, api in enumerate(apis_to_delete):
                print(f"Deleting {api['Name']} ({i+1}/{len(apis_to_delete)})...")

                # Keep trying until success or max attempts reached
                max_attempts = 10
                attempt = 1
                deleted = False

                while attempt <= max_attempts and not deleted:
                    try:
                        if attempt > 1:
                            print(f"  Attempt {attempt}/{max_attempts}...")

                        self.client.delete_api(ApiId=api['ApiId'])
                        print(f"✓ Deleted {api['Name']}")
                        deleted_count += 1
                        deleted = True

                    except ClientError as e:
                        if "TooManyRequestsException" in str(e):
                            if attempt < max_attempts:
                                wait_time = min(10 + (attempt * 2), 30)  # Progressive backoff, max 30s
                                print(f"  ⚠️  Rate limited - waiting {wait_time}s before retry...")
                                time.sleep(wait_time)
                                attempt += 1
                            else:
                                print(f"✗ Failed to delete {api['Name']} after {max_attempts} attempts (rate limiting)")
                                break
                        else:
                            print(f"✗ Failed to delete {api['Name']}: {e}")
                            break

                # Small delay between different APIs to avoid immediate rate limiting
                if i < len(apis_to_delete) - 1:  # Don't wait after the last API
                    time.sleep(2)

            print(f"✓ Cleanup completed. Successfully deleted {deleted_count}/{len(apis_to_delete)} APIs")

            # Verify cleanup was complete
            if deleted_count < len(apis_to_delete):
                print(f"⚠️  Warning: {len(apis_to_delete) - deleted_count} APIs failed to delete due to rate limiting")
                print("   You may need to run the script again or manually delete them via AWS console")

        except ClientError as e:
            print(f"✗ Error accessing API Gateway: {e}")
            sys.exit(1)

    def create_api_for_server(self, server):
        """Create a new HTTP API for a specific server"""
        raw_url = server['url']
        port = server['port']
        name = server.get('name', '')  # Optional user-friendly name

        # Clean URL - remove protocol if present
        url = raw_url.replace('http://', '').replace('https://', '')

        api_name = f"utxo-accumulator--{url}--{port}"
        backend_uri = f"http://{url}:{port}"

        display_name = f"{name} ({url}:{port})" if name else f"{url}:{port}"
        print(f"\n🚀 Creating HTTP API for: {display_name}")
        print(f"   API Name: {api_name}")
        print(f"   Backend: {backend_uri}")

        try:
            # Step 1: Create the HTTP API with CORS
            api_response = self.client.create_api(
                Name=api_name,
                ProtocolType='HTTP',
                CorsConfiguration={
                    'AllowCredentials': True,
                    'AllowHeaders': [
                        'accept', 'accept-language', 'api-key', 'content-language',
                        'content-type', 'authorization', 'x-amz-date', 'x-api-key',
                        'x-requested-with'
                    ],
                    'AllowMethods': ['GET', 'POST', 'PUT', 'OPTIONS'],
                    'AllowOrigins': [
                        'http://localhost:5173',
                        'https://encryptedcoins.github.io',
                        'https://test.encoins.io',
                        'https://app.encoins.io'
                    ],
                    'ExposeHeaders': [
                        'authorization', 'content-type', 'date', 'x-api-id',
                        'x-amz-date', 'x-api-key', 'x-requested-with'
                    ],
                    'MaxAge': 300
                }
            )
            
            api_id = api_response['ApiId']
            print(f"✓ Created HTTP API: {api_id}")

            # Step 2: Create integration to backend
            integration_response = self.client.create_integration(
                ApiId=api_id,
                IntegrationType='HTTP_PROXY',
                IntegrationUri=backend_uri,
                IntegrationMethod='ANY',
                ConnectionType='INTERNET',
                TimeoutInMillis=29000,
                PayloadFormatVersion='1.0'
            )
            
            integration_id = integration_response['IntegrationId']
            print(f"✓ Created integration: {integration_id}")

            # Step 3: Create default route
            route_response = self.client.create_route(
                ApiId=api_id,
                RouteKey='$default',
                Target=f'integrations/{integration_id}'
            )
            
            route_id = route_response['RouteId']
            print(f"✓ Created route: {route_id}")

            # Step 4: Create and deploy stage
            stage_response = self.client.create_stage(
                ApiId=api_id,
                StageName='$default',
                AutoDeploy=True
            )
            
            print(f"✓ Created and deployed stage: $default")

            # Construct endpoint URL
            api_endpoint = f"https://{api_id}.execute-api.{self.client.meta.region_name}.amazonaws.com/"

            print(f"✓ HTTP API deployment completed")
            print(f"  API ID: {api_id}")
            print(f"  Endpoint: {api_endpoint}")
            print(f"  Backend: {backend_uri}")

            return {
                'name': api_name,
                'display_name': display_name,
                'friendly_name': name,
                'id': api_id,
                'endpoint': api_endpoint,
                'backend': backend_uri,
                'url': url,
                'port': port
            }

        except ClientError as e:
            print(f"✗ Failed to create HTTP API for {display_name}: {e}")
            return None

    def update_servers_config_with_apis(self, created_apis, config_path="servers_config.yaml"):
        """Update the servers_config.yaml file with AWS API Gateway URLs as additional field"""
        try:
            # Read the current config
            with open(config_path, 'r') as file:
                config = yaml.safe_load(file)

            servers = config.get('servers', [])

            # Create a mapping of server configs to API endpoints
            for server in servers:
                # Clean the server URL for matching (remove protocol)
                server_url_raw = server.get('url', '')
                server_url_clean = server_url_raw.replace('http://', '').replace('https://', '')
                server_port = server.get('port')

                # Find matching API (APIs are stored with clean URLs)
                for api in created_apis:
                    if api['url'] == server_url_clean and api['port'] == server_port:
                        server['aws_api_url'] = api['endpoint']
                        break
                else:
                    # Remove aws_api_url if no matching API was created
                    server.pop('aws_api_url', None)

            # Write the updated config back to file
            with open(config_path, 'w') as file:
                yaml.dump(config, file, default_flow_style=False, indent=2, sort_keys=False)

            print(f"✓ Updated {config_path} with AWS API Gateway URLs")

        except Exception as e:
            print(f"⚠️  Failed to update config file: {e}")

    def run(self):
        """Main execution function"""
        print("🎯 UTxO Accumulator HTTP API Gateway Manager")
        print("=" * 50)

        # Load configuration
        servers = self.load_servers_config()

        # Step 1: Delete existing APIs (always do cleanup)
        self.delete_existing_apis()

        # Step 2: Handle empty server list
        if not servers:
            print("\n📝 No servers found in configuration")
            print("✓ Cleanup completed - all existing UTxO Accumulator APIs have been removed")
            print("\nTo create new APIs:")
            print("1. Edit servers_config.yaml")
            print("2. Add server entries with 'url' and 'port' fields")
            print("3. Run this script again")
            return

        # Step 3: Create new HTTP APIs
        print(f"\n🏗️  Creating {len(servers)} new HTTP APIs...")
        created_apis = []

        for server in servers:
            if not all(key in server for key in ['url', 'port']):
                print(f"✗ Invalid server configuration (missing url or port): {server}")
                continue

            api_info = self.create_api_for_server(server)
            if api_info:
                created_apis.append(api_info)

            time.sleep(2)  # Rate limiting between API creations

        # Update config file with AWS API URLs
        if created_apis or servers:  # Update if we created APIs or if we have servers (to remove old URLs)
            print("\n📝 Updating servers_config.yaml with AWS API Gateway URLs...")
            self.update_servers_config_with_apis(created_apis)

        # Summary
        print("\n📊 Summary:")
        print(f"✓ Successfully created {len(created_apis)} HTTP APIs")

        if created_apis:
            print("\n🔗 API Endpoints:")
            for api in created_apis:
                friendly_name = api.get('friendly_name', '')
                if friendly_name:
                    print(f"  {friendly_name}")
                    print(f"    API Name: {api['name']}")
                else:
                    print(f"  {api['name']}")
                print(f"    Endpoint: {api['endpoint']}")
                print(f"    Backend:  {api['backend']}")
                print()


def main():
    if len(sys.argv) > 1 and sys.argv[1] in ['-h', '--help']:
        print("""
UTxO Accumulator HTTP API Gateway Manager

Usage: python3 manage_apis.py

This script manages AWS API Gateway v2 (HTTP APIs) for UTxO Accumulator servers.

Files required:
- servers_config.yaml: List of servers to create APIs for

The script will:
1. Delete all existing HTTP APIs with 'utxo-accumulator--' in the name
2. Create new HTTP APIs for each server in the configuration
3. Configure CORS, integrations, and routes programmatically

Make sure AWS credentials are configured:
  aws configure
        """)
        return

    try:
        manager = APIGatewayManager()
        manager.run()
        print("🎉 HTTP API management completed successfully!")

    except KeyboardInterrupt:
        print("\n\n⚠️  Operation cancelled by user")
        sys.exit(1)
    except Exception as e:
        print(f"\n💥 Unexpected error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
