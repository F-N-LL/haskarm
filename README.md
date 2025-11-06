# HaskArm-CAM

Hand-tracking servo control system using Haskell, Python OpenCV, and ESP32-CAM.

## Overview

HaskArm uses computer vision to detect and track hand position, controlling a servo motor to follow the target. The system combines:
- **Python + OpenCV**: Hand detection via color segmentation
- **Haskell**: Control loop and HTTP communication
- **ESP32-CAM**: Camera streaming and servo control
- **Nix**: Reproducible development environment

## Hardware Requirements

- ESP32-CAM (Freenove WROVER recommended)
- Servo motor (SG90 or MG90S)
- 5V power supply (2A minimum)
- USB-Serial adapter (for programming ESP32)
- Jumper wires

## Hardware Setup

### Connections
```
ESP32-CAM Pin 15  →  Servo Signal (orange wire)
5V Power Supply   →  Servo VCC (red wire)
GND (common)      →  Servo GND (brown wire) + ESP32 GND
```

**Important**: Use external 5V power supply for servo, not ESP32's 5V pin.

## Software Setup

### 1. Enter Nix Development Environment
```bash
cd haskarm
nix develop
```
First run takes 1-2 hours (downloads and compiles dependencies). Subsequent runs are instant.

### 2. Configure WiFi in Firmware
Copy the example config and edit with your WiFi credentials:
```bash
cd firmware/src
cp config.example.h config.h
nano config.h  # Edit with your WiFi SSID and password
```

### 3. Upload Firmware to ESP32-CAM
```bash
cd firmware
pio run -t upload
pio device monitor  # Check IP address
```

Note the IP address shown (e.g., `192.168.1.200`).

### 4. Configure Project Settings
Copy the example config and edit with your ESP32 IP:
```bash
cd my-project
cp config.example.json config.json
nano config.json  # Edit esp32_ip to match your ESP32
```

### 5. Build and Run
```bash
cd my-project
cabal build
cabal run my-project
```

## Testing

### Test ESP32 Endpoints
```bash
# View camera image
curl http://192.168.1.200/capture --output test.jpg

# Move servo
curl "http://192.168.1.200/servo?angle=0"    # Left
curl "http://192.168.1.200/servo?angle=90"   # Center
curl "http://192.168.1.200/servo?angle=180"  # Right
```

### Test Vision Detection
```bash
cd my-project
python3 vision/vision.py
# Should output: {"angle": 95} or {"angle": null}
```

## Configuration

### Adjust Hand Detection Sensitivity
Edit `my-project/vision/vision.py`:
```python
MIN_AREA = 5000   # Minimum hand size (increase to ignore small objects)
MAX_AREA = 50000  # Maximum hand size (decrease to ignore face)
```

### Adjust Servo Movement Threshold
Edit `my-project/src/Main.hs` line 33:
```haskell
when (abs (a - prev) > 5) $ do  # Change 5 to higher value for less sensitivity
```

## Project Structure

```
haskarm/
├── firmware/              # ESP32-CAM firmware (PlatformIO)
│   ├── src/main.cpp      # Camera + servo control
│   └── platformio.ini    # Build configuration
├── my-project/           # Haskell control system
│   ├── src/
│   │   ├── Main.hs      # Main control loop
│   │   └── Control.hs   # HTTP servo control
│   ├── vision/
│   │   └── vision.py    # OpenCV hand detection
│   └── my-project.cabal # Haskell dependencies
├── nix/                  # Nix configuration
│   └── shell.nix        # Development environment
├── flake.nix            # Nix flake entry point
└── README.md
```

## How It Works

1. **ESP32-CAM** streams JPEG images via HTTP endpoint `/capture`
2. **Python script** fetches image, detects hand position using HSV color segmentation
3. **Haskell program** calls Python script, parses JSON output
4. **Haskell** sends servo angle to ESP32 via HTTP endpoint `/servo?angle=X`
5. **Servo** moves to track hand position
6. Loop repeats every 200ms

## Troubleshooting

### Python fails with "ModuleNotFoundError: No module named 'cv2'"
Make sure you're in the `nix develop` shell, not a regular shell.

### "readCreateProcess: python3 failed"
Check that Python path in `Main.hs` matches your Nix store path:
```bash
which python3  # Copy this path to Main.hs
```

### Servo doesn't move
- Check ESP32 IP is correct in both `vision.py` and `Main.hs`
- Test endpoints manually with `curl`
- Verify servo is connected to Pin 15

### Detection is inaccurate
- Adjust `MIN_AREA` and `MAX_AREA` in `vision.py`
- Improve lighting conditions
- Adjust HSV color range for your skin tone

## License

Apache-2.0
