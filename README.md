## CERN 1-Wire IP Core for ID and Temperature Sensors

This VHDL implementation targets the DS18B20 1-Wire ID and temperature sensor
and DS2401 silicon ID. It was designed to monitor installed parts and
temperatures within the Large Hadron Collider's next generation of power
converter control electronics. Device IDs and temperatures are being stored in
an internal memory, allocating 2 entries of 64 bits (ID + scratchpad) per
device.

Because things are much more fun when seen in action, it also contains a small
example for a UART dongle.
