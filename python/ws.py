from __future__ import absolute_import, division, print_function
import serial

selected_port = 'COM1'
ser = serial.Serial(selected_port, 115200, timeout = 1)
print("connected to " + ser.name)

# message = (0x03, 0x01, 0x00, 0xFA, 0x00, 0xF8)
# message = '\x03\x01\x00\xFA\x00\xF8' # bag 1, 250ml
message = '\x03\x02\x00\x1E\x00\x1F' # bag 2, 90ml
# message = [0x03, 0x02, 0x00, 0x1E, 0x00, 0x1F] # bag 2, 90ml

03 02 00 1E 0 1F

print(message)
ser.write(message)
response = ser.readline()
print(response)
