#!/bin/sh

strip Launch
upx -9 Launch
chmod +x Launch
rm *.enc
rm *.o
rm *.bf
rm *.or
rm *.ppu

