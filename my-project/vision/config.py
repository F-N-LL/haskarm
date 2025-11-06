import json
import os

def load_config():
    config_path = os.path.join(os.path.dirname(__file__), '..', 'config.json')
    try:
        with open(config_path, 'r') as f:
            return json.load(f)
    except FileNotFoundError:
        print("Error: config.json not found. Copy config.example.json to config.json")
        exit(1)
