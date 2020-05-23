#!/bin/bash

avr-gcc $1 -mmcu=atmega2560 -g -o a.out \
&& \
avr-objcopy -j .text -j .data -O ihex a.out a.hex \
&& \
avrdude -p m2560 -P /dev/ttyACM0 -c wiring -U flash:w:./a.hex:i -Dv \
&& \
rm a.out a.hex
