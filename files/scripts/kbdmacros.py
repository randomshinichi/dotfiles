import os
from evdev import InputDevice, categorize, ecodes
dev = InputDevice('/dev/input/by-id/usb-Ultimate_Gadget_Laboratories_UHK_60_v1-if01-event-kbd')
dev.grab()


for event in dev.read_loop():
    if event.type == ecodes.EV_KEY:
        print(categorize(event))