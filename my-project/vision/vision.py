import cv2
import numpy as np
import requests
import json
import sys

CAMERA_URL = "http://esp32cam.local/capture"

def fetch_frame():
    resp = requests.get(CAMERA_URL, timeout=3)
    arr = np.asarray(bytearray(resp.content), dtype=np.uint8)
    return cv2.imdecode(arr, cv2.IMREAD_COLOR)

def detect_hand_angle(frame):
    hsv = cv2.cvtColor(frame, cv2.COLOR_BGR2HSV)
    lower = np.array([0, 30, 60], dtype=np.uint8)
    upper = np.array([20, 150, 255], dtype=np.uint8)
    mask = cv2.inRange(hsv, lower, upper)
    contours, _ = cv2.findContours(mask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    if not contours:
        return None
    largest = max(contours, key=cv2.contourArea)
    M = cv2.moments(largest)
    if M["m00"] == 0:
        return None
    cx = int(M["m10"] / M["m00"])
    width = frame.shape[1]
    angle = int((cx / width) * 180)
    return angle

def main():
    try:
        frame = fetch_frame()
        angle = detect_hand_angle(frame)
        if angle is None:
            result = {"angle": None}
        else:
            result = {"angle": angle}
        print(json.dumps(result))
    except Exception as e:
        print(json.dumps({"error": str(e)}))
        sys.exit(1)

if __name__ == "__main__":
    main()
